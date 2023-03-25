/*
 *	HT Editor
 *	ppcopc.cc
 *
 *	Copyright (C) 1999-2003 Sebastian Biallas (sb@biallas.net)
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

#include <cstdio>
#include <cstdlib>
#include "rv64opc.h"

/* Macros used to form opcodes.  */

//RV64I
#define OP_RTYPE(opcode, funct3, funct7) static_cast<uint32>(((opcode) | ((funct3) << 12) | ((funct7) << 25)))
#define OP_RTYPE_F5(opcode, funct3, funct5) static_cast<uint32>(((opcode) | ((funct3) << 12) | ((funct5) << 27)))
#define OP_ITYPE(opcode, funct3)        static_cast<uint32>(((opcode) | ((funct3) << 12)))
#define OP_ITYPE_F6(opcode, funct3, funct6) static_cast<uint32>(((opcode) | ((funct3) << 12) | ((funct6) << 26)))
#define OP_ITYPE_F7(opcode, funct3, funct7) OP_RTYPE((opcode), (funct3), (funct7))

#define OP_UTYPE(opcode)        static_cast<uint32>((opcode))
#define OP_ETYPE(opcode, funct3,  funct12)        static_cast<uint32>(((opcode) | ((funct3) << 12) |  ((funct12) << 20)))

#define MASK_RTYPE OP_RTYPE(0b1111111, 0b111, 0b1111111)
#define MASK_RTYPE_F5 OP_RTYPE_F5(0b1111111, 0b111, 0b11111)
#define MASK_ITYPE OP_ITYPE(0b1111111, 0b111)
#define MASK_ITYPE_F6 OP_ITYPE_F6(0b1111111, 0b111, 0b111111)
#define MASK_ITYPE_F7 OP_ITYPE_F7(0b1111111, 0b111, 0b1111111)

#define MASK_UTYPE OP_UTYPE(0b1111111)
#define MASK_ETYPE OP_ETYPE(0b1111111, 0b111, 0b111111111111)

//Compressed ops

#define OP_C_FUNCT3(opcode, funct3)     static_cast<uint16>((opcode) | ((funct3) << 13))
#define OP_C_FUNCT4(opcode, funct4)     static_cast<uint16>((opcode) | ((funct4) << 12))
#define OP_C_FUNCT6(opcode, funct6)     static_cast<uint16>((opcode) | ((funct6) << 10))
#define OP_C_FUNCT3_INST4(opcode, funct3, inst4)     static_cast<uint16>((opcode) | ((funct3) << 13) | ((inst4) << 7))
#define OP_C_FUNCT3_INST2(opcode, funct3, inst2)     static_cast<uint16>((opcode) | ((funct3) << 13) | ((inst2) << 10))
#define OP_C_FUNCT3_INST2_INST3(opcode, funct3, inst2, inst3)     static_cast<uint16>((opcode) | ((funct3) << 13) | ((inst2) << 10) | (((inst3) & 0b100) << 10) | (((inst3) & 0b011) << 5))
#define OP_C_FUNCT4_INST5(opcode, funct4, inst5)     static_cast<uint16>((opcode) | ((funct4) << 12) | ((inst5) << 2))
#define OP_C_FUNCT4_INST5_INST5(opcode, funct4, inst5, inst5_1)     static_cast<uint16>((opcode) | ((funct4) << 12) | ((inst5) << 2) | ((inst5_1) << 7))


#define MASK_OP_C_F3 OP_C_FUNCT3(0b11, 0b111)
#define MASK_OP_C_F4 OP_C_FUNCT4(0b11, 0b1111)
#define MASK_OP_C_F6 OP_C_FUNCT6(0b11, 0b111111)
#define MASK_OP_C_F3_INST4 OP_C_FUNCT3_INST4(0b11, 0b111, 0b1111)
#define MASK_OP_C_F3_INST2 OP_C_FUNCT3_INST2(0b11, 0b111, 0b11)
#define MASK_OP_C_F3_INST2_INST3 OP_C_FUNCT3_INST2_INST3(0b11, 0b111, 0b11, 0b111)
#define MASK_OP_C_F4_INST5 OP_C_FUNCT4_INST5(0b11, 0b1111, 0b11111)
#define MASK_OP_C_F4_INST5_INST5 OP_C_FUNCT4_INST5_INST5(0b11, 0b1111, 0b11111, 0b11111)

#define OP_C_OPCODE_C1 1

/* Smaller names for the flags so each entry in the opcodes table will
   fit on a single line.  */
#undef	RV64I
#define RV64I       RV64_OPCODE_I
#define RV64IMAFD	RV64I | RV64_OPCODE_M | RV64_OPCODE_A | RV64_OPCODE_F | RV64_OPCODE_D
#define RV64GC   RV64IMAFD | RV64_OPCODE_C

static uint32 sign_extend(uint32 value, uint32 imm_size_in_bits)
{
    if ((value & (1 << (imm_size_in_bits - 1))) != 0)
        value -= 1 << imm_size_in_bits;
    return value;
}
//argument extraction functions
static uint32 extract_store_offset(uint32 insn, bool *invalid)
{
    uint16 lower_part = (insn >>  7) & 0x1f;
    uint16 upper_part = ((insn >> 25) & 0x7f) << 5;
    return upper_part + lower_part;
}

static uint32 extract_upper_value(uint32 insn, bool *invalid)
{
    return (insn  & 0xfffff000);
}

static uint32 extract_upper_jal_value(uint32 insn, bool *invalid)
{
    uint32 part4 = (insn >> 31) & 1;
    uint32 part3 = (insn >> 12) & 0xff;
    uint32 part2 = (insn >> 20) & 1;
    uint32 part1 = (insn >> 21) & 0x3ff;
    
    uint32 res = (part1 << 1) + (part2  << 11) + (part3 << 12) + (part4 << 20);
    return sign_extend(res, 21);
}


static uint32 extract_upper_cj_value(uint32 insn, bool *invalid)
{
    
    /*
     C.J offset[11|4|9:8|10|6|7|3:1|5]
    uint32 imm_val =  ( insn >> 2 ) & 0x7ff;
    uint32 part8 = (imm_val >> 10) & 1;
    uint32 part7 = (imm_val >> 3) & 1;
    uint32 part6 = (imm_val >> 7) & 3;
    uint32 part5 = (imm_val >> 9) & 1;
    uint32 part4 = (imm_val >> 5) & 1;
    uint32 part3 = (imm_val >> 6) & 1;
    uint32 part2 = imm_val & 0x7;
    uint32 part1 = (imm_val >> 4) & 1;
    */
    // C.J offset[11|7|9:8|5|6|1|10|4:2]
    uint32 imm_val =  ( insn >> 2 ) & 0x7ff;
    uint32 part8 = (imm_val >> 10) & 1;
    uint32 part7 = (imm_val >> 6) & 1; //differs
    uint32 part6 = (imm_val >> 7) & 3;
    uint32 part5 = (imm_val >> 4) & 1; //differs
    uint32 part4 = (imm_val >> 5) & 1;
    uint32 part3 = (imm_val >> 0) & 1; //differs
    uint32 part2 = (imm_val >> 9) & 1; //differs
    uint32 part1 = (imm_val >> 1) & 0x7; //differs
    
    uint32 res = (part1 + (part2  << 3) + (part3 << 4) + (part4 << 5) + (part5 << 6) + (part6 << 7) + (part7 << 9) + (part8 << 10)) << 1;

    return sign_extend(res, 12);
}

static uint32 extract_b_type_imm(uint32 insn, bool *invalid)
{
    uint32 part4 = (insn >> 31) & 1;
    uint32 part3 = (insn >> 7 ) & 1;
    uint32 part2 = (insn >> 25) & 0x3f;
    uint32 part1 = (insn >> 8) & 0xf;
    uint32 imm = (part1 << 1) + (part2 << 5) + (part3 << 11) + (part4 << 12);
    return sign_extend(imm, 13);
}

static uint32 extract_ci_imm(uint32 insn, bool *invalid)
{
    uint32 part2 = (insn >> 12) & 1;
    uint32 part1 = (insn >> 2) & 0x1f;
    uint32 imm = part1 + (part2 << 5);
    return sign_extend(imm, 6);
}

static uint32 extract_ci_shamt(uint32 insn, bool *invalid)
{
    uint32 part2 = (insn >> 12) & 1;
    uint32 part1 = (insn >> 2) & 0x1f;
    uint32 imm = part1 + (part2 << 5);
    return imm;
}

static uint32 extract_ci_imm_upper(uint32 insn, bool *invalid)
{
    uint32 part2 = (insn >> 12) & 1;
    uint32 part1 = (insn >> 2) & 0x1f;
    uint32 imm = (part1 << 12) + (part2 << 17);
    return sign_extend(imm, 18);
}

static uint32 extract_c_lw_off(uint32 insn, bool *invalid)
{
    uint32 part1 = (insn >> 6) & 1;
    uint32 part2 = (insn >> 10) & 7;
    uint32 part3 = (insn >> 5) & 1;
    return ((part1 << 2) + (part2 << 3) + (part3 << 6));
}

static uint32 extract_c_ld_off(uint32 insn, bool *invalid)
{
    uint32 part1 = (insn >> 10) & 7;
    uint32 part2 = (insn >> 5) & 3;
    return ((part1 << 3) + (part2 << 6));
}

static uint32 extract_c_lwsp_off(uint32 insn, bool *invalid)
{
    uint32 part1 = (insn >> 4) & 7;
    uint32 part2 = (insn >> 12) & 1;
    uint32 part3 = (insn >> 2) & 3;
    return ((part1 << 2) + (part2 << 5) + (part3 << 6));
}

static uint32 extract_c_ldsp_off(uint32 insn, bool *invalid)
{
    uint32 part1 = (insn >> 5) & 3;
    uint32 part2 = (insn >> 12) & 1;
    uint32 part3 = (insn >> 2) & 7;
    return ((part1 << 3) + (part2 << 5) + (part3 << 6));
}

static uint32 extract_c_swsp_off(uint32 insn, bool *invalid)
{
    uint32 part1 = (insn >> 9) & 0xf;
    uint32 part2 = (insn >> 7) & 3;
    return ((part1 << 2) + (part2 << 6));
}

static uint32 extract_c_sdsp_off(uint32 insn, bool *invalid)
{
    uint32 part1 = (insn >> 10) & 7;
    uint32 part2 = (insn >> 7) & 7;
    return ((part1 << 3) + (part2 << 6));
}

static uint32 extract_ciw_imm(uint32 insn, bool *invalid)
{
    uint32 imm = (insn >> 5) & 0xff;
    uint32 part2 = imm & 1;
    uint32 part1 = (imm >> 1) & 1;
    uint32 part4 = (imm >> 2) & 0xf;
    uint32 part3 = (imm >> 6) & 3;
    uint32 res = (part1 << 2) + (part2 << 3) + (part3 << 4) + (part4 << 6);
    return sign_extend(res, 10);
}

static uint32 extract_c_branch_offset(uint32 insn, bool *invalid)
{
    uint32 imm1 = (insn >> 2) & 0x1f;
    uint32 imm2 = (insn >> 10) & 07;
    uint32 part3 = (imm1 >> 0) & 1;
    uint32 part1 = (imm1 >> 1) & 3;
    uint32 part4 = (imm1 >> 3) & 3;
    uint32 part2 = (imm2 >> 0) & 3;
    uint32 part5 = (imm2 >> 2) & 1;

    uint32 res = (part1 << 1) + (part2 << 3) + (part3 << 5) + (part4 << 6) + (part5 << 8);
    return sign_extend(res, 9);
}

static uint32 extract_addi16_imm(uint32 insn, bool *invalid)
{
    uint32 imm = (insn >> 2) & 0x1f;
    uint32 part1 = (imm >> 4) & 1;
    uint32 part2 = (imm >> 0) & 1;
    uint32 part3 = (imm >> 3) & 1;
    uint32 part4 = (imm >> 1) & 3;
    uint32 part5 = (insn >> 12) & 1;

    uint32 res = (part1 << 4) + (part2 << 5) + (part3 << 6) + (part4 << 7) + (part5 << 9);
    return sign_extend(res, 10);
}

char * csr_number_to_name(uint32 csr_no)
{
    switch (csr_no) {
        case 0x000:
            return "ustatus";
        case 0x001:
            return "fflags";
        case 0x002:
            return "frm";
        case 0x003:
            return "fcsr";
        case 0x004:
            return "uie";
        case 0x005:
            return "utvec";
        case 0x040:
            return "uscratch";
        case 0x041:
            return "uepc";
        case 0x042:
            return "ucause";
        case 0x043:
            return "utval";
        case 0x044:
            return "uip";
        case 0xc00:
            return "cycle";
        case 0xc01:
            return "time";
        case 0xc02:
            return "instret";
        case 0xc80:
            return "cycleh";
        case 0xc81:
            return "timeh";
        case 0xc82:
            return "instreth";
        default:
            //case for counters
            {
                static char * counters_names[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                                    };
                if ((csr_no >= 0xc03) && (csr_no <= 0xc1f))
                {
                    uint32 counter_num = csr_no - 0xc00;
                    if (counters_names[counter_num] == NULL){
                        counters_names[counter_num] = (char*)malloc(sizeof(char) * 20);
                        snprintf(counters_names[counter_num], sizeof(char) * 20, "hpmcounter%x", counter_num);
                    }
                    return counters_names[counter_num];
                }
                if ((csr_no >= 0xc83) && (csr_no <= 0xc8f))
                {
                    uint32 counter_num = csr_no - 0xc80;
                    if (counters_names[counter_num+0x20] == NULL){
                        counters_names[counter_num+0x20] = (char*)malloc(sizeof(char) * 20);
                        snprintf(counters_names[counter_num+0x20], sizeof(char) * 20, "hpmcounter%xh", counter_num);
                    }
                    return counters_names[counter_num+0x20];
                }
            }
            return NULL;
    }
}
/* The operands table.
 
 The fields are bits, shift, signed, extract, flags.  */


#undef UNUSED


const struct riscv64_operand riscv64_operands[] =
{
    /* The zero index is used to indicate the end of the list of
     operands.  */
#define UNUSED 0
    { 0, 0, 0, 0 },
    
    // source register 1
#define RS1 UNUSED + 1
    {5, 15, 0, RV64_OPERAND_GPR},
    
    // source register 2
#define RS2 RS1 + 1
    {5, 20, 0, RV64_OPERAND_GPR},
    
    // dest register
#define RD RS2 + 1
    {5, 7, 0, RV64_OPERAND_GPR},
    //IMM from type_I opcodes
#define RIMM_I RD + 1
    {12, 20, 0, RV64_OPERAND_ABSOLUTE | RV64_OPERAND_SIGNED},
//IMM offset from load ops
#define RLOFF RIMM_I + 1
    {12, 20, 0, RV64_OPERAND_PARENS | RV64_OPERAND_ABSOLUTE},
//IMM offset from store ops, needs extraction function
#define RSOFF RLOFF + 1
    {7, 4, extract_store_offset, RV64_OPERAND_PARENS | RV64_OPERAND_ABSOLUTE},
#define LUI_IMM RSOFF + 1
    {20, 12, extract_upper_value, RV64_OPERAND_ABSOLUTE},
#define AUIPC_IMM LUI_IMM + 1
    {20, 12, extract_upper_value, RV64_OPERAND_RELATIVE},
#define JAL_IMM AUIPC_IMM + 1
    {20, 12, extract_upper_jal_value, RV64_OPERAND_RELATIVE},
#define FENCE_PRED JAL_IMM + 1
    {4, 20, 0, RV64_OPERAND_FENCES},
#define FENCE_SUCR FENCE_PRED + 1
    {4, 24, 0, RV64_OPERAND_FENCES},
#define CJ_IMM FENCE_SUCR + 1
    {11, 2, extract_upper_cj_value, RV64_OPERAND_RELATIVE },
#define BTYPE_IMM CJ_IMM + 1
    {12, 7, extract_b_type_imm, RV64_OPERAND_RELATIVE },
#define SHAMT_R6 BTYPE_IMM + 1
    {6, 20, 0, RV64_OPERAND_ABSOLUTE},
#define SHAMT_R7 SHAMT_R6 + 1
    {5, 20, 0, RV64_OPERAND_ABSOLUTE},
#define C_RD    SHAMT_R7 + 1
    {5, 7, 0, RV64_OPERAND_GPR},
#define C_RS    C_RD + 1
    {5, 2, 0, RV64_OPERAND_GPR},
#define C_RD1    C_RS + 1
    {3, 2, 0, RV64_COMPRESSED_GPR},
#define C_RS1    C_RD1 + 1
    {3, 7, 0, RV64_COMPRESSED_GPR},
#define C_CI_IMM    C_RS1 + 1
    {6, 2, extract_ci_imm, RV64_OPERAND_ABSOLUTE | RV64_OPERAND_SIGNED},
#define C_CI_IMM_UP C_CI_IMM + 1
    {6, 2, extract_ci_imm_upper, RV64_OPERAND_ABSOLUTE | RV64_OPERAND_SIGNED},
#define C_LW_OFF    C_CI_IMM_UP + 1
    {6, 2, extract_c_lw_off, RV64_OPERAND_PARENS | RV64_OPERAND_ABSOLUTE},
#define C_LD_OFF    C_LW_OFF + 1
    {6, 2, extract_c_ld_off, RV64_OPERAND_PARENS | RV64_OPERAND_ABSOLUTE},
#define C_FS1 C_LD_OFF + 1
    {3, 2, 0, RV64_COMPRESSED_FPR},
#define C_FD1 C_FS1 + 1
    {3, 7, 0, RV64_COMPRESSED_FPR},
#define C_FD C_FD1 + 1
    {3, 7, 0, RV64_OPERAND_FPR},
#define C_LWSP_OFF C_FD + 1
    {7, 2, extract_c_lwsp_off,  RV64_OPERAND_ABSOLUTE | RV64_SP_RELATIVE},
#define C_LDSP_OFF C_LWSP_OFF + 1
    {8, 2, extract_c_ldsp_off,  RV64_OPERAND_ABSOLUTE | RV64_SP_RELATIVE},
#define C_SWSP_OFF C_LDSP_OFF + 1
    {7, 2, extract_c_swsp_off,  RV64_OPERAND_ABSOLUTE | RV64_SP_RELATIVE},
#define C_SDSP_OFF C_SWSP_OFF + 1
    {8, 2, extract_c_sdsp_off,  RV64_OPERAND_ABSOLUTE | RV64_SP_RELATIVE},
#define C_SHAMT5 C_SDSP_OFF + 1
    {2, 5, extract_ci_shamt, RV64_OPERAND_ABSOLUTE},
#define C_ADDI4IMM C_SHAMT5 + 1
    {5, 8, extract_ciw_imm, RV64_OPERAND_ABSOLUTE},
#define C_BR_OFF C_ADDI4IMM + 1
    {2, 8, extract_c_branch_offset, RV64_OPERAND_RELATIVE},
#define C_ADDI16IMM C_BR_OFF + 1
    {10, 2, extract_addi16_imm, RV64_OPERAND_ABSOLUTE | RV64_OPERAND_SIGNED},
#define RIMM_I_CSRREG C_ADDI16IMM + 1
    {12, 20, 0, RV64_OPERAND_ABSOLUTE | RV64_CSR_REGS},
#define RIMM_CSRI RIMM_I_CSRREG + 1
    {5, 15, 0, RV64_OPERAND_ABSOLUTE},
#define AMO_ORDERING RIMM_CSRI + 1
    {2, 25, 0, RV64_OPERAND_ABSOLUTE | RV64_OPERAND_AMO_ORDER},
#define RS1_ADDR AMO_ORDERING + 1
    {5, 15, 0, RV64_OPERAND_GPR | RV64_OPERAND_INSIDE_PARENS}
};
  /* The BA field in an XL form instruction.  */

/* The opcode table.

   The format of the opcode table is:

   NAME	     OPCODE	MASK		FLAGS		{ OPERANDS }

   NAME is the name of the instruction.
   OPCODE is the instruction opcode.
   MASK is the opcode mask; this is used to tell the disassembler
     which bits in the actual opcode must match OPCODE.
   FLAGS are flags indicated what processors support the instruction.
   OPERANDS is the list of operands.

   The disassembler reads the table in order and prints the first
   instruction which matches, so this table is sorted to put more
   specific instructions before more general instructions.  It is also
   sorted by major opcode.  */

const struct riscv64_opcode riscv64_opcodes[] = {
//R-type
{ "nop", 0x00000013, 0xFFFFFFFF, RV64I, {}}, // it's ADD x0, x0,0 aka nop
{ "add",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 0,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "sub",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 0,0x20), MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "sll",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 1,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "slt",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 2,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "sltu", OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 3,0),    MASK_RTYPE, RV64I,		{RD, RS1, RS2} },
{ "xor",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 4,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "srl",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 5,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "sra",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 5,0x20), MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "or",   OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 6,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },
{ "and",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 7,0),    MASK_RTYPE,	RV64I,		{RD, RS1, RS2} },

//64-bit addition
{ "addw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 0, 0), MASK_RTYPE,    RV64I,        {RD, RS1, RS2} },
{ "subw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 0, 0x20), MASK_RTYPE,    RV64I,     {RD, RS1, RS2} },
{ "sllw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 1, 0), MASK_RTYPE,    RV64I,        {RD, RS1, RS2} },
{ "srlw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 5, 0), MASK_RTYPE,    RV64I,        {RD, RS1, RS2} },
{ "sraw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 5, 0x20), MASK_RTYPE,    RV64I,     {RD, RS1, RS2} },

//I-type
{ "addi",   OP_ITYPE(RV64_OP_I1_OPCODE_VALUE, 0), MASK_ITYPE,	RV64I,		{RD, RS1, RIMM_I}},
{ "slti",   OP_ITYPE(RV64_OP_I1_OPCODE_VALUE, 2), MASK_ITYPE,	RV64I,		{RD, RS1, RIMM_I} },
{ "sltiu",  OP_ITYPE(RV64_OP_I1_OPCODE_VALUE, 3), MASK_ITYPE,	RV64I,		{RD, RS1, RIMM_I} },
{ "xori",   OP_ITYPE(RV64_OP_I1_OPCODE_VALUE, 4), MASK_ITYPE,	RV64I,		{RD, RS1, RIMM_I} },
{ "ori",    OP_ITYPE(RV64_OP_I1_OPCODE_VALUE, 6), MASK_ITYPE,    RV64I,		{RD, RS1, RIMM_I} },
{ "andi",   OP_ITYPE(RV64_OP_I1_OPCODE_VALUE, 7), MASK_ITYPE,	RV64I,		{RD, RS1, RIMM_I} },
{ "slli",   OP_ITYPE_F6(RV64_OP_I1_OPCODE_VALUE, 1,0), MASK_ITYPE_F6,	RV64I,		{RD, RS1, SHAMT_R6} },
{ "srli",   OP_ITYPE_F6(RV64_OP_I1_OPCODE_VALUE, 5,0), MASK_ITYPE_F6,	RV64I,		{RD, RS1, SHAMT_R6} },
{ "srai",   OP_ITYPE_F6(RV64_OP_I1_OPCODE_VALUE, 5,0x10), MASK_ITYPE_F6, RV64I,		{RD, RS1, SHAMT_R6} },
    
    //64-bit addition
{ "addiw",   OP_ITYPE(RV64_OP_I3_OPCODE_VALUE, 0),      MASK_ITYPE,    RV64I,        {RD, RS1, RIMM_I} },
{ "slliw",   OP_ITYPE_F7(RV64_OP_I3_OPCODE_VALUE, 1, 0),   MASK_ITYPE_F7,    RV64I,        {RD, RS1, SHAMT_R7} },
{ "srliw",   OP_ITYPE_F7(RV64_OP_I3_OPCODE_VALUE, 5, 0),   MASK_ITYPE_F7,    RV64I,        {RD, RS1, SHAMT_R7} },
{ "sraiw",   OP_ITYPE_F7(RV64_OP_I3_OPCODE_VALUE, 5, 0x20), MASK_ITYPE_F7,    RV64I,        {RD, RS1, SHAMT_R7} },
 
    
//load I-type
{ "lb",   OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 0), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} },
{ "lh",   OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 1), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} },
{ "lw",   OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 2), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} },
{ "lbu",  OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 4), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} },
{ "lhu",  OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 5), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} },
{ "lwu",  OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 6), MASK_ITYPE, RV64I,      {RD, RLOFF, RS1} },
{ "ld",   OP_ITYPE(RV64_OP_I2_OPCODE_VALUE, 3), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} },
    //stopped here
//store S-type
{ "sb",  OP_ITYPE(RV64_OP_S_OPCODE_VALUE, 0), MASK_ITYPE,	RV64I,		{RS2, RSOFF, RS1} },
{ "sh",  OP_ITYPE(RV64_OP_S_OPCODE_VALUE, 1), MASK_ITYPE,	RV64I,		{RS2, RSOFF, RS1} },
{ "sw",  OP_ITYPE(RV64_OP_S_OPCODE_VALUE, 2), MASK_ITYPE,	RV64I,		{RS2, RSOFF, RS1} },
{ "sd",  OP_ITYPE(RV64_OP_S_OPCODE_VALUE, 3), MASK_ITYPE,    RV64I,     {RS2, RSOFF, RS1} },

//branch SB-type
{ "beq",   OP_ITYPE(RV64_OP_SB_OPCODE_VALUE, 0), MASK_ITYPE,	RV64I,		{RS1, RS2, BTYPE_IMM} },
{ "bne",   OP_ITYPE(RV64_OP_SB_OPCODE_VALUE, 1), MASK_ITYPE,	RV64I,		{RS1, RS2, BTYPE_IMM} },
{ "blt",   OP_ITYPE(RV64_OP_SB_OPCODE_VALUE, 4), MASK_ITYPE,	RV64I,		{RS1, RS2, BTYPE_IMM} },
{ "bge",   OP_ITYPE(RV64_OP_SB_OPCODE_VALUE, 5), MASK_ITYPE,	RV64I,		{RS1, RS2, BTYPE_IMM} },
{ "bltu",  OP_ITYPE(RV64_OP_SB_OPCODE_VALUE, 6), MASK_ITYPE,	RV64I,		{RS1, RS2, BTYPE_IMM} },
{ "bgeu",  OP_ITYPE(RV64_OP_SB_OPCODE_VALUE, 7), MASK_ITYPE,	RV64I,		{RS1, RS2, BTYPE_IMM} },
//U-type
{ "lui",    OP_UTYPE(RV64_OP_LUI_OPCODE_VALUE), MASK_UTYPE,	RV64I,		{RD, LUI_IMM} },
{ "auipc",  OP_UTYPE(RV64_OP_AUIPC_OPCODE_VALUE), MASK_UTYPE,	RV64I,	{RD, AUIPC_IMM} },
{ "jal",    OP_UTYPE(RV64_OP_JAL_OPCODE_VALUE), MASK_UTYPE,	RV64I,		{RD, JAL_IMM} },
//i-type
{ "jalr",   OP_ITYPE(RV64_OP_JALR_OPCODE_VALUE, 0), MASK_ITYPE,	RV64I,		{RD, RLOFF, RS1} }, //FIXME, sign extend as well?
//fences
{ "fence",    OP_ITYPE(RV64_OP_FEN_OPCODE_VALUE,0), MASK_ITYPE,	RV64I,		{FENCE_PRED, FENCE_SUCR} },
{ "fence.i",  OP_ITYPE(RV64_OP_FEN_OPCODE_VALUE,1), MASK_ITYPE,	RV64I,		{} },
//ecall/ebreak
{ "ecall",    OP_ETYPE(RV64_OP_E_OPCODE_VALUE, 0, 0), MASK_ETYPE,	RV64I,		{} },
{ "ebreak",   OP_ETYPE(RV64_OP_E_OPCODE_VALUE, 0, 1), MASK_ETYPE,	RV64I,		{} },
//csrs
{ "csr.rw",    OP_ITYPE(RV64_OP_CSR_OPCODE_VALUE, 1),    MASK_ITYPE,	RV64I,		{RD, RIMM_I_CSRREG, RS1} },
{ "csr.rs",    OP_ITYPE(RV64_OP_CSR_OPCODE_VALUE, 2),	 MASK_ITYPE,	RV64I,		{RD, RIMM_I_CSRREG, RS1} },
{ "csr.rc",    OP_ITYPE(RV64_OP_CSR_OPCODE_VALUE, 3),	 MASK_ITYPE,	RV64I,		{RD, RIMM_I_CSRREG, RS1} },
{ "csr.rwi",   OP_ITYPE(RV64_OP_CSR_OPCODE_VALUE, 5),    MASK_ITYPE,    RV64I,        {RD, RIMM_I_CSRREG, RIMM_CSRI} },
{ "csr.rsi",   OP_ITYPE(RV64_OP_CSR_OPCODE_VALUE, 6),    MASK_ITYPE,    RV64I,        {RD, RIMM_I_CSRREG, RIMM_CSRI} },
{ "csr.rci",   OP_ITYPE(RV64_OP_CSR_OPCODE_VALUE, 7),    MASK_ITYPE,    RV64I,        {RD, RIMM_I_CSRREG, RIMM_CSRI} },
//Compressed
{ "c.addi4spn",   OP_C_FUNCT3(0,0), MASK_OP_C_F3,    RV64GC,        {C_RD1, C_ADDI4IMM} },
    { "c.fld",   OP_C_FUNCT3(0,1), MASK_OP_C_F3,    RV64GC,       {C_FD1, C_LD_OFF, C_RS1} },
    { "c.lw",   OP_C_FUNCT3(0,2), MASK_OP_C_F3,    RV64GC,        {C_RD1, C_LW_OFF, C_RS1} },
    { "c.ld",   OP_C_FUNCT3(0,3), MASK_OP_C_F3,    RV64GC,        {C_RD1, C_LD_OFF, C_RS1} },
    { "c.fsd",   OP_C_FUNCT3(0,5), MASK_OP_C_F3,    RV64GC,       {C_FD1, C_LD_OFF, C_RS1} },
    { "c.sw",   OP_C_FUNCT3(0,6), MASK_OP_C_F3,    RV64GC,        {C_RD1, C_LW_OFF, C_RS1} },
    { "c.sd",   OP_C_FUNCT3(0,7), MASK_OP_C_F3,    RV64GC,        {C_RD1, C_LD_OFF, C_RS1} },
    { "c.nop",   0x0001, 0xFFFF,    RV64GC,        {} }, //should be before c.addi
    { "c.addi",   OP_C_FUNCT3(1,0), MASK_OP_C_F3,    RV64GC,        {C_RD, C_CI_IMM} },
    { "c.addiw",   OP_C_FUNCT3(1,1), MASK_OP_C_F3,    RV64GC,        {C_RD, C_CI_IMM} },
    { "c.li",   OP_C_FUNCT3(1,2), MASK_OP_C_F3,    RV64GC,        {C_RD, C_CI_IMM} },
    { "c.addi16sp",   OP_C_FUNCT3_INST4(1,3,2), MASK_OP_C_F3_INST4,    RV64GC,        {C_RD, C_ADDI16IMM} }, //should be before lui
    { "c.lui",   OP_C_FUNCT3(1,3), MASK_OP_C_F3,    RV64GC,        {C_RD, C_CI_IMM_UP} }, //FIXME: check on negatives
    { "c.srli",   OP_C_FUNCT3_INST2(1,4,0), MASK_OP_C_F3_INST2,    RV64GC,        {C_RS1, C_SHAMT5} },
    { "c.srai",   OP_C_FUNCT3_INST2(1,4,1), MASK_OP_C_F3_INST2,    RV64GC,        {C_RS1, C_SHAMT5} },
    { "c.andi",   OP_C_FUNCT3_INST2(1,4,2), MASK_OP_C_F3_INST2,    RV64GC,        {C_RS1, C_CI_IMM} },
    { "c.sub",   OP_C_FUNCT3_INST2_INST3(1,4,3,0), MASK_OP_C_F3_INST2_INST3,    RV64GC,        {C_RS1, C_RD1} },
    { "c.xor",   OP_C_FUNCT3_INST2_INST3(1,4,3,1), MASK_OP_C_F3_INST2_INST3,    RV64GC,        {C_RS1, C_RD1} },
    { "c.or",   OP_C_FUNCT3_INST2_INST3(1,4,3,2), MASK_OP_C_F3_INST2_INST3,    RV64GC,        {C_RS1, C_RD1} },
    { "c.and",   OP_C_FUNCT3_INST2_INST3(1,4,3,3), MASK_OP_C_F3_INST2_INST3,    RV64GC,        {C_RS1, C_RD1} },
    { "c.subw",   OP_C_FUNCT3_INST2_INST3(1,4,3,4), MASK_OP_C_F3_INST2_INST3,    RV64GC,        {C_RS1, C_RD1} },
    { "c.addw",   OP_C_FUNCT3_INST2_INST3(1,4,3,5), MASK_OP_C_F3_INST2_INST3,    RV64GC,        {C_RS1, C_RD1} },
    { "c.j",   OP_C_FUNCT3(1,5), MASK_OP_C_F3,    RV64GC,        {CJ_IMM} },
    { "c.beqz",   OP_C_FUNCT3(1,6), MASK_OP_C_F3,    RV64GC,        {C_RS1, C_BR_OFF} },
    { "c.bnez",   OP_C_FUNCT3(1,7), MASK_OP_C_F3,    RV64GC,        {C_RS1, C_BR_OFF} },
    { "c.slli",   OP_C_FUNCT3(2,0), MASK_OP_C_F3,    RV64GC,        {C_RD, C_SHAMT5} },
    { "c.fldsp",   OP_C_FUNCT3(2,1), MASK_OP_C_F3,    RV64GC,        {C_FD, C_LDSP_OFF} },
    { "c.lwsp",   OP_C_FUNCT3(2,2), MASK_OP_C_F3,    RV64GC,        {C_RD, C_LWSP_OFF} },
    { "c.ldsp",   OP_C_FUNCT3(2,3), MASK_OP_C_F3,    RV64GC,        {C_RD, C_LDSP_OFF} },
    { "c.jr",   OP_C_FUNCT4_INST5(2,8,0), MASK_OP_C_F4_INST5,    RV64GC,        {C_RD} }, //c.jr should be before c.mv
    { "c.mv",   OP_C_FUNCT4(2,8), MASK_OP_C_F4,    RV64GC,        {C_RD, C_RS} },
    { "c.ebreak",   OP_C_FUNCT4_INST5_INST5(2,9, 0, 0), MASK_OP_C_F4_INST5_INST5,    RV64GC,        {} },  //c.ebreak before c.jalr
    { "c.jalr",   OP_C_FUNCT4_INST5(2, 9, 0), MASK_OP_C_F4_INST5,    RV64GC,        {C_RD} }, //jalr before c.add,
    { "c.add",    OP_C_FUNCT4(2,9), MASK_OP_C_F4,    RV64GC,        {C_RD, C_RS} },
    { "c.fsdsp",  OP_C_FUNCT3(2,5), MASK_OP_C_F3,    RV64GC,        {C_FD, C_SDSP_OFF} },
    { "c.swsp",   OP_C_FUNCT3(2,6), MASK_OP_C_F3,    RV64GC,        {C_RS, C_SWSP_OFF} },
    { "c.sdsp",   OP_C_FUNCT3(2,7), MASK_OP_C_F3,    RV64GC,        {C_RS, C_SDSP_OFF} },

    //M extension
    //RV64_OP_R_OPCODE_VALUE and RV64_OP_R64_OPCODE_VALUE for 32-bit ops
    { "mul",    OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 0, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "mulh",   OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 1, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "mulhsu", OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 2, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "mulhu",  OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 3, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "div",    OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 4, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "divu",   OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 5, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "rem",    OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 6, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "remu",   OP_RTYPE(RV64_OP_R_OPCODE_VALUE, 7, 1),   MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "mulw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 0, 1), MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "divw",   OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 4, 1), MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "divuw",  OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 5, 1), MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "rem",    OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 6, 1), MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    { "remuw",  OP_RTYPE(RV64_OP_R64_OPCODE_VALUE, 7, 1), MASK_RTYPE,    RV64GC,        {RD, RS1, RS2} },
    //A extension
    //w width
    { "amoswap.w",  OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 1),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoadd.w",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 0),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoxor.w",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 4),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoand.w",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 12),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoor.w",    OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 8),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amomin.w",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 16),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amomax.w",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 20),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amominu.w",  OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 24),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amomaxu.w",  OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 28),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "lr.w",       OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 2),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "sc.w",       OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_W_VALUE, 3),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },

        //d width
    { "amoswap.d",  OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 1),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoadd.d",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 0),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoxor.d",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 4),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoand.d",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 12),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amoor.d",    OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 8),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amomin.d",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 16),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amomax.d",   OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 20),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amominu.d",  OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 24),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "amomaxu.d",  OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 28),   MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "lr.d",       OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 2),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },
    { "sc.d",       OP_RTYPE_F5(RV64_OP_AMO_OPCODE_VALUE, RV64_AMO_WIDTH_D_VALUE, 3),    MASK_RTYPE_F5,    RV64GC,        {AMO_ORDERING, RD, RS2, RS1_ADDR} },


};

const int riscv64_num_opcodes =
  sizeof (riscv64_opcodes) / sizeof (riscv64_opcodes[0]);

//srai now working
