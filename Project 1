#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer(FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate(){
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo(int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch(int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode(unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
	unsigned int opcode =  instr & 0b11111100000000000000000000000000;
	d->op = opcode >> 26;
	//printf("got opcode = %d\n", d->op);
	if(d->op == 0){
		d->type = R;
		//printf("got R type = %d\n", d->type);
	}
	else if(d->op == 2 || d->op == 3){
		d->type = J;
		//printf("got J type = %d\n", d->type);
	}
	else{
		d->type = I;
		//printf("got I type = %d\n", d->type);
	}
	if(d->type == R){ //R-format
		//printf("R-format \n");
		unsigned int funct = instr & 0b00000000000000000000000000111111;
		d->regs.r.funct = funct;
		//printf("got d->regs.r.funct = %d\n", d->regs.r.funct);
		if(d->regs.r.funct != 33 && d->regs.r.funct != 35 && d->regs.r.funct != 0 
		&& d->regs.r.funct != 2 && d->regs.r.funct != 36 && d->regs.r.funct != 37 
		&& d->regs.r.funct != 42 && d->regs.r.funct != 8){
			exit(0);
		}
		else{
			unsigned int rs = instr & 0b00000011111000000000000000000000;
			d->regs.r.rs = rs >> 21;
			//printf("got d->regs.r.rs = %d\n", d->regs.r.rs);
			unsigned int rt = instr & 0b00000000000111110000000000000000;
			d->regs.r.rt = rt >> 16;
			//printf("got d->regs.r.rt = %d\n", d->regs.r.rt);
			unsigned int rd = instr & 0b00000000000000001111100000000000;
			d->regs.r.rd = rd >> 11;
			//printf("got d->regs.r.rd = %d\n", d->regs.r.rd);
			unsigned int shamt = instr & 0b00000000000000000000011111000000;
			d->regs.r.shamt = shamt >> 6;
			//printf("got d->regs.r.shamt = %d\n", d->regs.r.shamt);
			//printf("got d->regs.r.funct = %d\n", d->regs.r.funct);
		}
	}
	
	else if(d->type == J){ //J-format
		//printf("J-format \n");
		int target = (instr & 0b00000011111111111111111111111111) << 2;
		//printf("got target = %d\n", target);
		d->regs.j.target = target;
        //printf("got d->regs.j.target: %8.8x \n", d->regs.j.target);
		
	}
	else if(d->type == I){ //I-format
		//printf("I-format \n");
		if(d->op != 9 && d->op != 12 && d->op != 13 && d->op != 15 && d->op != 4 && 
		d->op != 5 && d->op != 35 && d->op != 43){
			exit(0);
		}
		else{
			unsigned int rs = instr & 0b00000011111000000000000000000000;
			d->regs.i.rs = rs >> 21;
			//printf("got d->regs.i.rs = %d\n", d->regs.i.rs);
			unsigned int rt = instr & 0b00000000000111110000000000000000;
			d->regs.i.rt = rt >> 16;
			//printf("got d->regs.i.rt = %d\n", d->regs.i.rt);
			int immed = instr & 0b00000000000000001111111111111111;
			d->regs.i.addr_or_immed = immed;
			//printf("got immed = %d\n", immed);
			if(d->op == 4 || d->op == 5){ //beq
				d->regs.i.addr_or_immed = mips.pc + 4 +(d->regs.i.addr_or_immed << 2);
			}
			if(d->op != 12 && d->op != 13 && immed >> 15 == 1){// sign extension
				d->regs.i.addr_or_immed = d->regs.i.addr_or_immed + 0b11111111111111110000000000000000;
			}
		}
	}
}
	//printf("decode is finished\n");

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction(DecodedInstr* d) {
    /* Your code goes here */
	//printf("print instruction activated\n");
	char name[6] = " ";
	//Check to see instruction name
	//printf("d->type: %d\n", d->type);
	if(d->type == R){ //R-format
		if(d->regs.r.funct == 33){
			strcpy(name,"addu");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 35){
			strcpy(name,"subu");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 0){
			strcpy(name,"sll");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 2){
			strcpy(name,"srl");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 36){
			strcpy(name,"and");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 37){
			strcpy(name,"or");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 42){
			strcpy(name,"slt");
			//printf("name: %s\n", name);
		}
		else if(d->regs.r.funct == 8){
			strcpy(name,"jr");
			//printf("name: %s\n", name);
		}
	}
	//printf("done with R checks\n");
	//printf("d->op: %d\n", d->op);
	if(d->op == 9){
		strcpy(name,"addiu");
		//printf("name: %s\n", name);
	}
	else if(d->op == 12){
		strcpy(name,"andi");
		//printf("name: %s\n", name);
	}
	else if(d->op == 13){
		strcpy(name,"ori");
		//printf("name: %s\n", name);
	}
	else if(d->op == 15){
		strcpy(name,"lui");
		//printf("name: %s\n", name);
	}
	else if(d->op == 4){
		strcpy(name,"beq");
		//printf("name: %s\n", name);
	}
	else if(d->op == 5){
		strcpy(name,"bne");
		//printf("name: %s\n", name);
	}
	else if(d->op == 2){
		strcpy(name,"j");
		//printf("name: %s\n", name);
	}
	else if(d->op == 3){
		strcpy(name,"jal");
		//printf("name: %s\n", name);
	}
	else if(d->op == 35){
		strcpy(name,"lw");
		//printf("name: %s\n", name);
	}
	else if(d->op == 43){
		strcpy(name,"sw");
		//printf("name: %s\n", name);
	}
	//printf("finished with all checks\n");
	if(d->type == R){ //R-format
		int rd = d->regs.r.rd;
		int rt = d->regs.r.rt;
		if(d->regs.r.funct == 0 || d->regs.r.funct == 2){ //sll or srl
			int shamt = d->regs.r.shamt;
			printf("%s\t$%d, $%d, %d\n", name, rd, rt, shamt);
		}
		else if(d->regs.r.funct == 8){ //jr
			int rs = d->regs.r.rs;
			printf("%s\t$%d\n", name, rs);
		}
		else{
			int rs = d->regs.r.rs;
			printf("%s\t$%d, $%d, $%d\n", name, rd, rs, rt);
		}
	}
	else if(d->type == J){ //J-format
		int target = d->regs.j.target;
		printf("%s\t0x00%x\n", name, target);
	}
	else{ //I-format
		int rs = d->regs.i.rs;
		int rt = d->regs.i.rt;
		int immed = d->regs.i.addr_or_immed;
		if(d->op == 4 || d->op == 5){ //beq or bne
			printf("%s\t$%d, $%d, 0x00%x\n", name, rs, rt, immed);
		}
		else if(d->op == 12 || d->op == 13){ //andi or ori
			printf("%s\t$%d, $%d, 0x00%x\n", name, rs, rt, immed);
		}
		else if(d->op == 9){ //addiu
			printf("%s\t$%d, $%d, %d\n", name, rt, rs, immed);	
		}
		else if(d->op == 35 || d->op == 43){ //lw or sw
			printf("%s\t$%d, %d($%d)\n", name, rs, immed, rt);
		}
		else if(d->op == 15){ //lui
			printf("%s\t$%d, $%d, 0x00%x\n", name, rt, immed);
		}
	}
	
}

/* Perform computation needed to execute d, returning computed value */
int Execute(DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
	unsigned int val;
	if(d->type == R){
		if(d->regs.r.funct == 33){ //addu
			val = mips.registers[d->regs.r.rs] + mips.registers[d->regs.r.rt];
		}
		else if(d->regs.r.funct == 35){ //subu
			val = mips.registers[d->regs.r.rs] - mips.registers[d->regs.r.rt];
		}
		else if(d->regs.r.funct == 0){ //sll
			val = mips.registers[d->regs.r.rt] << d->regs.r.shamt;
		}
		else if(d->regs.r.funct == 2){ //srl
			val = mips.registers[d->regs.r.rt] >> d->regs.r.shamt;
		}
		else if(d->regs.r.funct == 36){ //and
			val = mips.registers[d->regs.r.rs] & mips.registers[d->regs.r.rt];
		}
		else if(d->regs.r.funct == 37){ //or
			val = mips.registers[d->regs.r.rs] | mips.registers[d->regs.r.rt];
		}
		else if(d->regs.r.funct == 42){ //slt
			if(mips.registers[d->regs.r.rs] < mips.registers[d->regs.r.rt]){
				val = 1;
			}
			else{
				val = 0;
			}
		}
		else if(d->regs.r.funct == 8){ //jr
			val = mips.registers[d->regs.r.rs];
		}
	}
	if(d->op == 9){ //addiu
		val = mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed;
	}
	else if(d->op == 12){ //andi
		val = mips.registers[d->regs.i.rs] & d->regs.i.addr_or_immed;
	}
	else if(d->op == 13){ //ori
		val = mips.registers[d->regs.i.rs] | d->regs.i.addr_or_immed;
	}
	else if(d->op == 15){ //lui
		val = d->regs.i.addr_or_immed << 16;
	}
	else if(d->op == 4){ //beq
		if(mips.registers[d->regs.i.rs] == mips.registers[d->regs.i.rt]){
			val = 1;
		}
		else val = 0;
	}
	else if(d->op == 5){ //bne
		if(mips.registers[d->regs.i.rs] != mips.registers[d->regs.i.rt]){
			val = 1;
		}
		else val = 0;
	}
	else if(d->op == 2){ //j
		val = 0;
	}
	else if(d->op == 3){ //jal
		val = mips.pc + 4;
	}
	else if(d->op == 35){ //lw
		val = mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed;
	}
	else if(d->op == 43){ //sw
		val = mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed;
	}
	return val;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC(DecodedInstr* d, int val) {
    /* Your code goes here */
	if(d->op == 4 || d->op == 5){ //beq or bne
		if(val == 1){
			mips.pc = d->regs.i.addr_or_immed;
		}
		else{
			mips.pc = mips.pc + 4;
		}
	}
	else if(d->op == 2 || d->op == 3){ //j or jal
		mips.pc = d->regs.j.target;
	}
	else if(d->type == R){
		if(d->regs.r.funct == 8){
			mips.pc = val;
		}
		else{
			mips.pc = mips.pc + 4;
		}
	}
	else{
		mips.pc = mips.pc + 4;
	}
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem(DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */
	if(d->op == 35){ //lw
		if(val < 0x00401000 || val > 0x00403fff){ //invalid memory address
			printf("Memory Access Exception at 0x%08x: address 0x%08x\n", mips.pc-4, val);
			exit(0);
		}
		else{
			return mips.memory[(val-0x00401000)/4];
		}
	}
	if(d->op == 43){
		*changedMem = val;
		if(val < 0x00401000 || val > 0x00403fff){ //invalid memory address
			printf("Memory Access Exception at 0x%08x: address 0x%08x\n", mips.pc-4, val);
			exit(0);
		}
		else{
			mips.memory[(val-0x00400000)/4] = mips.registers[d->regs.i.rt];
		}
	}
	return val;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
	if(d->type == R){
		if(d->regs.r.funct != 8){
		*changedReg = d->regs.r.rd;
		mips.registers[*changedReg] = val;
		}
		else{
			*changedReg = -1;
		}
	}
	else if(d->type == J){
		if(d->op == 3){ //jal
			*changedReg = 31;
			mips.registers[*changedReg] = val; //mips.pc + 4
		}
		else{
			*changedReg = -1;
		}
	}
	else if(d->type == I){
		if(d->op == 4 || d->op == 5 || d->op == 43){ //beq or bne
			*changedReg = -1;
		}
		else{
			*changedReg = d->regs.i.rt;
			mips.registers[*changedReg] = val;
		}
	}
}
