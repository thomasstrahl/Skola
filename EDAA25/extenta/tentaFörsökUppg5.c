#include <stdio.h>
#include <stdlib.h>

typedef struct stack_t stack_t;

struct stack_t {
	stack_t* next;
	int data;
	
};

stack_t* new_stack(void) 
{
	printf("påbörjar stacken\n");
	stack_t* stack = malloc(sizeof(stack_t));
	if (stack == NULL) {
		printf("Can not create a new stack, out of memory!\n");
		exit(1);
	}
	stack->next = NULL;
	return stack;
}

void push(stack_t* stack, int data)
{
	stack_t* s;
	s = malloc(sizeof(stack_t));
	if (s == NULL) {
		printf("out of memory\n");
		exit(1);
	}
	s->data = data;
	s->next = stack->next;
	stack->next = s;
}

int pop(stack_t* stack) 
{
	if (stack->next == NULL) {
		printf("can not pop empty stack.\n");
		exit(1);
	}

	stack_t* p;
	int data;

	p = stack->next;
	stack->next = p->next;
	data = p->data;
	free(p);
	return data;
}

void free_stack(stack_t** stack) 
{
	stack_t* p = *stack;
	stack_t* q;

	while (p != NULL) {
		q = p->next;
		free(p);
		p = q;
	}
	*stack = NULL;
}

int main(void)
{
	stack_t*	stack;
	int 		a;

	stack = new_stack();
	printf("stacken skapad\n");

	push(stack, 2014);
	printf("first push\n");


	a = pop(stack);
	printf("pop\n");
	
	push(stack, 10);
	push(stack, 10);
	free_stack(&stack);
	printf("färdig\n");

	return stack == NULL ? 0 : 1;
}