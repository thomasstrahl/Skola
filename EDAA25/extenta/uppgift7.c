# include <stdio.h>
# include <stdlib.h>

typedef struct list_t list_t;


struct list_t {
	list_t* next;
	void* data;
};

static list_t* rec_reverse(list_t* new_next, list_t* current)
{
	list_t* old_next;
	if (current == NULL)
		return new_next;
	else {
		old_next = current->next;
		current->next = new_next;
		return rec_reverse(current, old_next);
	}
}
void reverse(list_t** list)
{
	*list = rec_reverse(NULL, *list);
}

int main(void)
{
	list_t* list;
	reverse(&list);
	return 0;
}