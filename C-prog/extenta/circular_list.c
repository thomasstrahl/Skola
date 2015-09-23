#include <stdio.h>
#include <stdlib.h>

typedef struct list_t list_t;

struct list_t
{
	list_t* succ;
	list_t* prev;
	int data;
};

list_t* new_list()
{
	list_t* head = malloc(sizeof(list_t));

	if (head == NULL) {
		printf("Out of memory\n");
		exit(1);
	}	
	head->succ = NULL;
	head->prev = NULL;
	return head;
}

void insert_first(list_t* list, int data)
{
	list_t* head = list;
	list_t* t = malloc(sizeof(list_t));

	if (t == NULL) {
		printf("Out of memory\n");
		exit(1);
	}
	t->data = data;

	if (head->succ == NULL) {
		head->succ = t;
		head->prev = t;
		t->succ = head;
		t->prev = head;
	} else {
		t->succ = head->succ;
		t->prev = head;
		head->succ->prev = t;
		head->succ = t;
	}
}

void free_list(list_t* list)
{
	list_t* p = list;
	list_t* q;

	if (p->prev == NULL) {
		return;
	} else {
		p->prev->succ = NULL;
		while (p != NULL) {
			q = p->succ;
			free(p);
			p = q;	
		}
	}
}

int main() 
{
	list_t* head = new_list();

	insert_first(head, 3);
	insert_first(head, 3);
	insert_first(head, 3);
	int i;
	for (i = 0; i < 3; ++i)
	{
		printf("%d\n", head->succ->data);
	}

	free_list(head);

	return 0;
}