# include <stdio.h>
# include <stdlib.h>

typedef struct stack_t stack_t;

struct stack_t {
	stack_t* next;
	int data;
};


stack_t* new_stack()
{
	stack_t* stack = malloc(sizeof(stack_t));

	if (stack == NULL) {
		printf("Out of memory!\n");
		exit(1);
	}
	stack->next = NULL;
	return stack;
}

void push(stack_t* stack, int data)
{	
	stack_t* p = malloc(sizeof(stack_t));
	if (p == NULL) {
		printf("Out of memory\n");
		exit(1);
	}
	p->data = data;
	p->next = stack->next;
	stack->next = p; 
}

int pop(stack_t* stack)
{
	if (stack->next == NULL) {
		printf("Dont pop empty stack\n");
		exit(1);
	}
	stack_t* p;
	p = stack->next;
	stack->next = p->next;
	int data = p->data;
	free(p);
	return data;
}

void free_stack(stack_t** stack)
{
	stack_t* p;
	stack_t* q;

	p = *stack;
	while(p != NULL) {
		q = p->next;
		free(p);
		p = q;
	}
	stack = NULL;
}


int main(void)
{
	stack_t* stack;
	int a;
	stack = new_stack();

	push(stack, 2014);
	a = pop(stack);
	printf("%d\n", a);
	push(stack, 10);
	push(stack, 30);
	free_stack(&stack);

	if (stack == NULL)
	{
		printf("yes!!\n");
	}
	return stack == NULL ? 0 : 1;
}
