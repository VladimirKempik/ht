/*
 *	HT Editor
 *	ppcdis.cc
 *
 *	Copyright (C) 1999-2002 Sebastian Biallas (sb@biallas.net)
 *	Copyright 1994 Free Software Foundation, Inc.
 *	Written by Ian Lance Taylor, Cygnus Support
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License version 2 as
 *	published by the Free Software Foundation.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "endianess.h"
#include "rv64dis.h"
#include "rv64opc.h"
#include "snprintf.h"
#include "tools.h"

RV64Disassembler::RV64Disassembler(int aMode)
{
	mode = aMode;
}

void RV64Disassembler::load(ObjectStream &f)
{
	Disassembler::load(f);
	GET_INT32X(f, mode);
}

dis_insn *RV64Disassembler::decode(byte *code, int maxlen, CPU_ADDR addr)
{
	const struct riscv64_opcode *opcode;
	const struct riscv64_opcode *opcode_end;
	uint32 op;
	int dialect = -1;
    //fetch 4 bytes first, to check if it's Compressed opcode or not
    insn.data = createHostInt(code, 4, little_endian);

	if (!(maxlen ==2 || maxlen == 4)) {
		insn.valid = false;
		insn.size = maxlen;

		return &insn;
	}
    insn.size = 4;
    /* Get the major opcode of the instruction.  */

    op = insn.data; //extract_32bit_opcode(insn.data);

    if ((insn.data & 0b011) != 0b011) {
        //compressed opcode, leave only lower 16 bits
        insn.size = 2;
        insn.data &= 0xffff;
        op = insn.data;
    }

    
	/* Find the first match in the opcode table.  We could speed this up
	   a bit by doing a binary search on the major opcode.  */
	opcode_end = riscv64_opcodes + riscv64_num_opcodes;
//    fprintf(stderr, "decoding %x\n", op);
	for (opcode = riscv64_opcodes; opcode < opcode_end; opcode++) {
		uint32 table_op;
		const byte *opindex;
		const struct riscv64_operand *operand;
		bool invalid;

		table_op = opcode->opcode;

		if ((insn.data & opcode->mask) != opcode->opcode/* || (opcode->flags & dialect) == 0*/) {
			continue;
		}

		/* Make two passes over the operands.  First see if any of them
		   have extraction functions, and, if they do, make sure the
		   instruction is valid.  */
		
        invalid = false;
		for (opindex = opcode->operands; *opindex != 0; opindex++) {
			operand = riscv64_operands + *opindex;
			if (operand->extract) (*operand->extract)(insn.data, &invalid);
		}
		if (invalid) continue;

		/* The instruction is valid.  */
		insn.name = opcode->name;

		/* Now extract and print the operands.  */
		int opidx = 0;
		
        for (opindex = opcode->operands; *opindex != 0; opindex++) {
			sint32 value;

			operand = riscv64_operands + *opindex;


			//if ((operand->flags & PPC_OPERAND_FAKE) != 0) continue;

			insn.op[opidx].op = operand;
			insn.op[opidx].flags = operand->flags;
			if (operand->extract) {
				value = (*operand->extract)(insn.data, NULL);
			} else {
				value = (insn.data >> operand->shift) & ((1 << operand->bits) - 1);
				if ((operand->flags & RV64_OPERAND_SIGNED) != 0 && (value & (1 << (operand->bits - 1))) != 0) {
					value -= 1 << operand->bits;
				}
			}

/*
			if ((operand->flags & PPC_OPERAND_OPTIONAL) != 0 && (operand->flags & PPC_OPERAND_NEXT) == 0 && value == 0) {
				insn.op[opidx++].imm = 0;
				continue;
			}
*/
            if (operand->flags & RV64_OPERAND_GPR) {
                insn.op[opidx++].reg = value;
            } else if (operand->flags & RV64_COMPRESSED_GPR) {
                insn.op[opidx++].reg = value + 8;
			} else if (operand->flags & RV64_OPERAND_FPR) {
				insn.op[opidx++].freg = value;
            } else if (operand->flags & RV64_COMPRESSED_FPR) {
                insn.op[opidx++].reg = value + 8;
			} else if (operand->flags & RV64_OPERAND_VR) {
				insn.op[opidx++].vreg = value;
			} else if (operand->flags & RV64_OPERAND_RELATIVE) {
//				if (mode == RV64_MODE_32) {
//					insn.op[opidx++].rel.mem = addr.addr32.offset + value;
//				} else {
					insn.op[opidx++].rel.mem = addr.flat64.addr + value;
//				}
            } else if ((operand->flags & RV64_OPERAND_ABSOLUTE) != 0) {
                insn.op[opidx++].abs.mem = value;
//			} else if ((operand->flags & PPC_OPERAND_CR) == 0 || (dialect & PPC_OPCODE_PPC) == 0) {
//				insn.op[opidx++].imm = (sint64)value;
			} else if (operand->flags & RV64_OPERAND_FENCES) {
				insn.op[opidx++].creg = value;
            }

		}
         
		insn.ops = opidx;

		/* We have found and printed an instruction; return.  */
		insn.valid = true;
		return &insn;
	}

	insn.valid = false;
	return &insn;
}

dis_insn *RV64Disassembler::duplicateInsn(dis_insn *disasm_insn)
{
	rv64dis_insn *insn = ht_malloc(sizeof (rv64dis_insn));
	*insn = *(rv64dis_insn *)disasm_insn;
	return insn;
}

void RV64Disassembler::getOpcodeMetrics(int &min_length, int &max_length, int &min_look_ahead, int &avg_look_ahead, int &addr_align)
{
	max_length = min_look_ahead = avg_look_ahead = 4;
    min_length = addr_align = 2;
}

byte RV64Disassembler::getSize(dis_insn *disasm_insn)
{
	return ((rv64dis_insn*)disasm_insn)->size;
}

const char *RV64Disassembler::getName()
{
	return "RV64/Disassembler";
}

const char *RV64Disassembler::str(dis_insn *disasm_insn, int style)
{
	return strf(disasm_insn, style, "");
}

const char *RV64Disassembler::strf(dis_insn *disasm_insn, int style, const char *format)
{
	if (style & DIS_STYLE_HIGHLIGHT) enable_highlighting();
	
	const char *cs_default = get_cs(e_cs_default);
	const char *cs_number = get_cs(e_cs_number);
	const char *cs_symbol = get_cs(e_cs_symbol);

	rv64dis_insn *rv64_insn = (rv64dis_insn *) disasm_insn;
	if (!rv64_insn->valid) {
		switch (rv64_insn->size) {
		case 1:
			strcpy(insnstr, "db            ?");
			break;
		case 2:
			strcpy(insnstr, "dw            ?");
			break;
		case 3:
			strcpy(insnstr, "db            ? * 3");
			break;
		case 4:
			sprintf(insnstr, "dd           %s0x%08x", cs_number, rv64_insn->data);
			break;
		default: { /* braces for empty assert */
			strcpy(insnstr, "?");
//			assert(0);
		}
		}
	} else {
		char *is = insnstr+sprintf(insnstr, "%-13s", rv64_insn->name);
		int dialect=-1;

		bool need_comma = false;
		bool need_paren = false;
        bool skip_comma = false;
        
		for (int opidx = 0; opidx < rv64_insn->ops; opidx++) {
			int flags = rv64_insn->op[opidx].flags;

			if (need_comma) {
				is += sprintf(is, "%s, ", cs_symbol);
				need_comma = false;
			}
            if (flags & RV64_OPERAND_INSIDE_PARENS) {
                is += sprintf(is, "%s(", cs_symbol);
                need_paren = true;
            }
			if ((flags & RV64_OPERAND_GPR) || (flags & RV64_COMPRESSED_GPR)) {
				is += sprintf(is, "%sx%d", cs_default, rv64_insn->op[opidx].reg);
			} else if (((flags & RV64_OPERAND_FPR) != 0) || (flags & RV64_COMPRESSED_FPR) != 0) {
				is += sprintf(is, "%sf%d", cs_default, rv64_insn->op[opidx].freg);
			} else if ((flags & RV64_OPERAND_VR) != 0) {
				is += sprintf(is, "%sv%d", cs_default, rv64_insn->op[opidx].vreg);
			} else if ((flags & RV64_OPERAND_RELATIVE) != 0) {
				CPU_ADDR caddr;
				//if (mode == PPC_MODE_32) {
				//	caddr.addr32.offset = (uint32)rv64_insn->op[opidx].mem.disp;
				//} else {
					caddr.flat64.addr = rv64_insn->op[opidx].mem.disp;
				//}
				int slen;
				char *s = (addr_sym_func) ? addr_sym_func(caddr, &slen, addr_sym_func_context) : 0;
				if (s) {
					is += sprintf(is, "%s", cs_default);
					memcpy(is, s, slen);
					is[slen] = 0;
					is += slen;
				} else {
					is += ht_snprintf(is, 100, "%s0x%qx", cs_number, rv64_insn->op[opidx].rel.mem);
				}
            } else if (((flags & RV64_OPERAND_ABSOLUTE) != 0) && ((flags & RV64_OPERAND_AMO_ORDER) != 0)) {
                //amo ordering arg
                skip_comma = true;
                switch (rv64_insn->op[opidx].abs.mem) {
                    case 1:
                        is += sprintf(is, "%srl ", cs_default);
                        break;
                    case 2:
                        is += sprintf(is, "%saq ", cs_default);
                        break;
                    case 3:
                        is += sprintf(is, "%saq_rl ", cs_default);
                        break;
                    case 0:
                    default:
                        break;
                }
            } else if ((flags & RV64_OPERAND_ABSOLUTE) != 0) {
                if ((flags & RV64_CSR_REGS) != 0) {
                    char * csrname = csr_number_to_name(rv64_insn->op[opidx].abs.mem);
                    if (csrname != NULL) {
                        is += sprintf(is, "%s%s", cs_symbol, csrname);
                    } else {
                        is += ht_snprintf(is, 100, "%s0x%qx", cs_number, rv64_insn->op[opidx].abs.mem);

                    }
                } else {
                    if ((flags & RV64_OPERAND_SIGNED) != 0 && (int32_t(rv64_insn->op[opidx].abs.mem) < 0 ))
                    {
                        is += ht_snprintf(is, 100, "-%s0x%qx", cs_number, abs(int32_t(rv64_insn->op[opidx].abs.mem))); //negative imm number are less than 32 bits always
                        
                    } else {
                        is += ht_snprintf(is, 100, "%s0x%qx", cs_number, rv64_insn->op[opidx].abs.mem);
                    }
                    if (flags & RV64_SP_RELATIVE) {
                        is += ht_snprintf(is, 100, "%s(x2)", cs_default);
                    }
                }
            } else if ((flags & RV64_OPERAND_FENCES) != 0) {
                
                static const char *fencenames[4] = { "w", "r", "o", "i" };
                for (int arg_iter = 3; arg_iter >= 0; --arg_iter)
                {
                    if (((rv64_insn->op[opidx].creg)  >> arg_iter) & 1)
                        is += sprintf(is, "%s%s", cs_symbol, fencenames[arg_iter]);
                }
                //sanity check
                if(rv64_insn->op[opidx].creg == 0)
                    is += sprintf(is, "%s%d", cs_number, 0);
			}
		
			if (need_paren) {
				is += sprintf(is, "%s)", cs_symbol);
				need_paren = false;
			}

			if ((flags & RV64_OPERAND_PARENS) == 0) {
				need_comma = true;
			} else {
				is += sprintf(is, "%s(", cs_symbol);
				need_paren = true;
			}

            //no comma needed if it's aq/rl
            if (skip_comma)
            {
                need_comma = false;
                skip_comma = false;
            }
		}
         
	}
	disable_highlighting();
	return insnstr;     
}

ObjectID RV64Disassembler::getObjectID() const
{
    return ATOM_DISASM_RISCV64;
}

void RV64Disassembler::store(ObjectStream &f) const
{
	PUT_INT32X(f, mode);
}

bool RV64Disassembler::validInsn(dis_insn *disasm_insn)
{
	return ((rv64dis_insn*)disasm_insn)->valid;
}
