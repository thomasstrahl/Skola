#include <string.h>
#include <sys/types.h>
#include <unistd.h>

typedef struct block_t 	block_t;

struct block_t{
	size_t size; /*size excluding block_t*/
	block_t* next;	/*next block if exists, otherwise NULL*/
	int free;
};

block_t* global_memory = NULL;

block_t* block_exists(block_t** last,size_t size)
{
	block_t* block = global_memory;
	while(block) {
		if(block->free && block->size >= size)
			return block;
		
		*last = block;
		block = block->next;
	}
	return NULL;
}

block_t* extend_memory(block_t* last, size_t size)
{
	void* test = sbrk(0);
	block_t* block = sbrk(size+sizeof(block_t));
	if(block == (void*)-1)
		return NULL;
	
	block->size = size;
	block->free = 0;
	block->next = NULL;

	if(last)
		last->next = block;
	else
		global_memory = block;

	return block;	
}

void* malloc(size_t size)
{
	block_t* block;
	if(size<= 0)
		return NULL;

	if(global_memory) {
		block_t* last = global_memory;
		block = block_exists(&last,size);
		
		if(block)
			block->free = 0;
		else
			block = extend_memory(last,size);

	} else {
		block = extend_memory(NULL,size);
	}
	return (block+1);
}

void free(void* ptr)
{
	if(!ptr)
		return;
	block_t* block = (block_t*)ptr-1;
	block->free = 1;
}

void* calloc(size_t count, size_t size)
{
	size_t tot_size = count*size;
	void* ptr = malloc(tot_size);

	if(!ptr)
		return NULL;

	memset(ptr,0,tot_size);
	return ptr;
}

void* realloc(void* ptr, size_t size)
{
	if(!ptr)
		return malloc(size);
	
	block_t* block = (block_t*)ptr-1;
	if(block->size>=size)
		return ptr;

	void* ptr2 = malloc(size);
	if(!ptr2)
		return NULL;

	memcpy(ptr2 ,ptr,block->size);
	free(ptr);
	return ptr2;
}