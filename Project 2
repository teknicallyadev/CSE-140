#include "tips.h"

/* The following two functions are defined in util.c */
/* finds the highest 1 bit, and returns its position, else 0xFFFFFFFF */

unsigned int uint_log2(word w);

/* return random int from 0..x-1 */
int randomint( int x );

	/*
	This function allows the lfu information to be displayed
	assoc_index - the cache unit that contains the block to be modified
	block_index - the index of the block to be modified
	returns a string representation of the lfu information
	*/

char* lfu_to_string(int assoc_index, int block_index) {
	static char buffer[9];
	sprintf(buffer, "%u", cache[assoc_index].block[block_index].accessCount);
	return buffer;
}

	/*
	This function allows the lru information to be displayed
	assoc_index - the cache unit that contains the block to be modified
	block_index - the index of the block to be modified
	returns a string representation of the lru information
	*/

char* lru_to_string(int assoc_index, int block_index){
	/* Buffer to print lru information -- increase size as needed. */
	static char buffer[9];
	sprintf(buffer, "%u", cache[assoc_index].block[block_index].lru.value);
	return buffer;
}

	/*
	This function initializes the lfu information
	assoc_index - the cache unit that contains the block to be modified
	block_number - the index of the block to be modified
	*/

void init_lfu(int assoc_index, int block_index){
	cache[assoc_index].block[block_index].accessCount = 0;
}

	/*
	This function initializes the lru information
	assoc_index - the cache unit that contains the block to be modified
	block_number - the index of the block to be modified
	*/

void init_lru(int assoc_index, int block_index){
	cache[assoc_index].block[block_index].lru.value = 0;
}
	/*
	This is the primary function you are filling out,
	You are free to add helper functions if you need them
	@param addr 32-bit byte address
	@param data a pointer to a SINGLE word (32-bits of data)
	@param we   if we == READ, then data used to return
				information back to CPU
				if we == WRITE, then data used to
				update Cache/DRAM
	*/

void accessMemory(address addr, word* data, WriteEnable we){
	address addrcpy = addr;
	unsigned int transfersize = uint_log2(block_size);
	int temp = 0;

	unsigned int offset = (block_size - 1) & addrcpy;
	addrcpy = addrcpy >> uint_log2(block_size); //offset bit
	unsigned int index = (set_count - 1) & addrcpy;
	addrcpy = addrcpy >> uint_log2(set_count); //index bit
	unsigned int tag = addrcpy;
	
	if(assoc == 0) {
		accessDRAM(addr, (byte*)data, WORD_SIZE, we);
		return;
	} 
	
	for(int i = 0; i < assoc; i++){
		//IF HIT
		if(cache[index].block[i].tag == tag){
			if(cache[index].block[i].valid == VALID){
				if(policy == LRU){
					cache[index].block[i].lru.value++;
				}
				if(we == READ){
					memcpy(data, cache[index].block[i].data + offset, transfersize);
				}
				else{
					memcpy(cache[index].block[i].data + offset, data, transfersize);
					if(memory_sync_policy == WRITE_BACK){
						cache[index].block[i].dirty = DIRTY;
					}
					else{
						accessDRAM(addr, cache[index].block[i].data, transfersize, WRITE);
					}
				}
				highlight_offset(index, i, offset, HIT);
				return;
			}
		}
	}
	//IF MISS
	if(assoc > 1){
		if(policy == LRU){
			for(int i = 0; i < assoc; i++){
				if(cache[index].block[i].lru.value > cache[index].block[temp].lru.value){
					temp = i;
				}
			}
		}
		else if(policy == RANDOM){
			temp = randomint(assoc);
		}
		else{
			return;
		}
	}
	//tested that writeback and writethrough did not do anything in regards to checking and running the accessDRAM
	//Neither did checking for dirty bits or virgin bits
	accessDRAM(addr, cache[index].block[temp].data, transfersize, READ);
	if(memory_sync_policy == WRITE_BACK){
      if(cache[index].block[temp].dirty == DIRTY){
        accessDRAM(addr, cache[index].block[temp].data, transfersize, WRITE);
      }
    }
	
	if(we == WRITE){
		memcpy(cache[index].block[temp].data + offset, data, transfersize);
	}
	else{
		memcpy(data, cache[index].block[temp].data + offset, transfersize);
	}

	highlight_block(index, temp);
	highlight_offset(index, temp, offset, MISS);
	cache[index].block[temp].lru.value = 0;
	cache[index].block[temp].valid = VALID;
	cache[index].block[temp].tag = tag;
}
