/*
 *	HT Editor
 *	ppcopc.cc
 *
 *	Copyright (C) 1999-2003 Sebastian Biallas (sb@biallas.net)
 *	Copyright 1994, 1995, 1999, 2000, 2001, 2002
 *	Free Software Foundation, Inc.
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

#ifndef __RV64_OPC_H__
#define __RV64_OPC_H__

#include "io/types.h"

/* The opcode table is an array of struct riscv64_opcode.  */
struct riscv64_opcode
{
	/* The opcode name.  */
	const char *name;

	/* The opcode itself.  Those bits which will be filled in with
	   operands are zeroes.  */
	uint32 opcode;

	/* The opcode mask.  This is used by the disassembler.  This is a
	   mask containing ones indicating those bits which must match the
	   opcode field, and zeroes indicating those bits which need not
	   match (and are presumably filled in by operands).  */
	uint32 mask;

	/* One bit flags for the opcode.  These are used to indicate which
	   specific processors support the instructions.  The defined values
	   are listed below.  */
	uint32 flags;

	/* An array of operand codes.  Each code is an index into the
	   operand table.  They appear in the order which the operands must
	   appear in assembly code, and are terminated by a zero.  */
	byte operands[8];
};

/* The table itself is sorted by major opcode number, and is otherwise
   in the order in which the disassembler should consider
   instructions.  */
extern const struct riscv64_opcode riscv64_opcodes[];
extern const int riscv64_num_opcodes;
extern char * csr_number_to_name(uint32 csr_no);
/* Values defined for the flags field of a struct riscv64_opcode.  */

/* Opcode is defined for the RV64I architecture.  */
#define RV64_OPCODE_I (01)

/* Opcode is defined for the M extension.  */
#define RV64_OPCODE_M (02)

/* Opcode is defined for the A extension.  */
#define RV64_OPCODE_A (04)

/* Opcode is defined for the F extension.  */
#define RV64_OPCODE_F (010)

/* Opcode is defined for the D extension.  */
#define RV64_OPCODE_D (020)

/* Opcode is defined for the C extension.  */

#define RV64_OPCODE_C (040)

/* Opcode is defined for the B extension.  */

#define RV64_OPCODE_B (0100)

/*
#define RV64_OPCODE_ANY (0200)

#define RV64_OPCODE_64_BRIDGE (0400)

#define RV64_OPCODE_ALTIVEC (01000)

#define RV64_OPCODE_403 (02000)

#define RV64_OPCODE_BOOKE (04000)

#define RV64_OPCODE_BOOKE64 (010000)

#define RV64_OPCODE_POWER4 (020000)

#define RV64_OPCODE_NOPOWER4 (040000)

#define RV64_OPCODE_CLASSIC (0100000)

#define RV64_OPCODE_SPE     (0200000)

#define RV64_OPCODE_ISEL     (0400000)

#define RV64_OPCODE_EFS      (01000000)

#define RV64_OPCODE_BRLOCK   (02000000)

#define RV64_OPCODE_PMR      (04000000)

#define RV64_OPCODE_CACHELCK (010000000)

#define RV64_OPCODE_RFMCI    (020000000)
*/

/* A macro to extract the major opcode from an instruction.  */

//RV64 32-bit opcode. lowest7 bits
#define RV64_OPCODE_32(i) ((i) & 0x7F)
#define RV64_FUNCT3(i) (((i) >> 12) & 0x7)
#define RV64_OPCODE_16(i) ((i) & 0x3)

//R-type 32-bit opcode, opcode value is 0b0110011
#define RV64_OP_R(i) ((i) & 0xFE00707F)
#define RV64_OP_R_OPCODE_VALUE  (0b0110011)
#define RV64_OP_R64_OPCODE_VALUE  (0b0111011)

//I and S-type 32-bit opcode , I opcode value is 0b0010011 and 0b0000011. S opcode value is 0b0100011, SB is 0b1100011
#define RV64_OP_ISB(i) ((i) & 0x707F)
#define RV64_OP_I1_OPCODE_VALUE (0b0010011)
#define RV64_OP_I2_OPCODE_VALUE (0b0000011)
#define RV64_OP_I3_OPCODE_VALUE (0b0011011)
#define RV64_OP_S_OPCODE_VALUE  (0b0100011)
#define RV64_OP_SB_OPCODE_VALUE (0b1100011)


//U-type 32-bit opcode LUI 0b0110111, AUIPC 0b0010111, JAL 0b1101111, JALR 0b1100111
#define RV64_OP_U(i) ((i) & 0x7F)
#define RV64_OP_LUI_OPCODE_VALUE   (0b0110111)
#define RV64_OP_AUIPC_OPCODE_VALUE (0b0010111)
#define RV64_OP_JAL_OPCODE_VALUE   (0b1101111)
#define RV64_OP_JALR_OPCODE_VALUE  (0b1100111)

// Special opcodes, fence, fence.i, opcode value is 0b0001111
#define RV64_OP_FEN(i) ((i) & 0xF00FFFFF)
#define RV64_OP_FEN_OPCODE_VALUE  (0b0001111)


#define RV64_OP_FUNCT3(i) (((i) > 12) & 0x7)
//ecall, ebreak, opcode value is 0b1110011, func3 is 0b000
#define RV64_OP_E(i) ((i) & 0xFFFFFFFF)
#define RV64_OP_E_OPCODE_VALUE  (0b1110011)

//csr* , opcode value is 0b1110011
#define RV64_OP_CSR(i) ((i) & 0x707F)
#define RV64_OP_CSR_OPCODE_VALUE  (0b1110011)



/* A macro to extract the major opcode from an comppressed instruction CR-type.  */
#define RV64_OPCR(i) ((i) & 0xF003)

/* A macro to extract the major opcode from an comppressed instruction C*-type except CR-type.  */
#define RV64_OPCA(i) ((i) & 0xE003)

//rest is from PPC

/* The operands table is an array of struct riscv64_operand.  */

struct riscv64_operand
{
	/* The number of bits in the operand.  */
	byte bits;

	/* How far the operand is left shifted in the instruction.  */
	byte shift;

	/* Extraction function.  This is used by the disassembler.  To
	   extract this operand type from an instruction, check this field.

	If it is NULL, compute
	    op = ((i) >> o->shift) & ((1 << o->bits) - 1);
	 if ((o->flags & RV64_OPERAND_SIGNED) != 0
		&& (op & (1 << (o->bits - 1))) != 0)
	   op -= 1 << o->bits;
	(i is the instruction, o is a pointer to this structure, and op
	is the result; this assumes twos complement arithmetic).

	If this field is not NULL, then simply call it with the
	instruction value.  It will return the value of the operand.  If
	the INVALID argument is not NULL, *INVALID will be set to
	non-zero if this operand type can not actually be extracted from
	this operand (i.e., the instruction does not match).  If the
	operand is valid, *INVALID will not be changed.  */

	uint32 (*extract)(uint32 instruction, bool *invalid);

	/* One bit syntax flags.  */
	uint32 flags;
};

/* Elements in the table are retrieved by indexing with values from
   the operands field of the riscv64_opcodes table.  */

extern const struct riscv64_operand riscv64_operands[];


/* Values defined for the flags field of a struct riscv64_operand.  */

/* This operand takes signed values.  */
#define RV64_OPERAND_SIGNED (01)

/* The next operand should be wrapped in parentheses rather than
   separated from this one by a comma.  This is used for the load and
   store instructions which want their operands to look like
	  reg,displacement(reg)
   */
#define RV64_OPERAND_PARENS (02)

/* This operand names a register.  The disassembler uses this to print
   register names with a leading 'r'.  */
#define RV64_OPERAND_GPR (04)

/* This operand names a floating point register.  The disassembler
   prints these with a leading 'f'.  */
#define RV64_OPERAND_FPR (010)

/* This operand is a relative branch displacement.  The disassembler
   prints these symbolically if possible.  */
#define RV64_OPERAND_RELATIVE (020)

/* This operand is an absolute branch address.  The disassembler
   prints these symbolically if possible.  */
#define RV64_OPERAND_ABSOLUTE (040)

/* fence args  */
#define RV64_OPERAND_FENCES (0100)

/* registers from compressed opcodes*/

#define RV64_COMPRESSED_GPR (0200)

#define RV64_COMPRESSED_FPR (0400)

//for c.ldsp and friends
#define RV64_SP_RELATIVE (01000)

// for csr register names

#define RV64_CSR_REGS (02000)

/* This operand names a vector unit register.  The disassembler
   prints these with a leading 'v'.  */
#define RV64_OPERAND_VR (04000)

#endif
