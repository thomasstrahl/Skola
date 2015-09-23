#ifndef sh_list 
#define sh_list

typedef struct list_t	list_t;

struct list_t {
	list_t*		succ;
	list_t*		pred;
	void*		data;
};

list_t*		new_list(void*);
void		free_list(list_t**);
void*		remove_first(list_t**);
void*		remove_last(list_t**);
void		insert_before(list_t**, void*);
void		insert_after(list_t**, void*);
void 		insert_last(list_t**, void*);
unsigned	length(list_t*);
void		print_list(list_t*);

#endif
