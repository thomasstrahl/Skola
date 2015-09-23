#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

typedef struct stack_t stack_t;

struct stack_t {
	stack_t* next;
	int data;	
};


stack_t* new_stack() {
	
	stack_t* stack = malloc(sizeof(stack_t));
	if (stack == NULL) {
		printf("No stack created, out of memory!\n");
		exit(1);
	}
	stack->next = NULL;
	return stack;
}

void push(stack_t* stack, int data) 
{
	stack_t* p = malloc(sizeof(stack_t));

	if (p == NULL){
		printf("out of memory in push\n");
		exit(1);
	}
	p->next = stack->next;
	p->data = data;
	stack->next = p;	
}

int pop(stack_t* stack)
{
	if (stack->next == NULL) {
		printf("Do not pop empty stack!\n");
		exit(1);
	}
	stack_t* p = stack->next;
	stack->next = p->next;
	int a = p->data;
	free(p);
	return a;
}

void free_stack(stack_t** stack) 
{
	stack_t* p = *stack;
	stack_t* q;

	while (p->next != NULL) {
		q = p->next;
		free(p);
		p = q;
	}

	*stack = NULL;

}

int main () 
{
	stack_t* stack = new_stack();
	char input = getchar();
	int more_digs = 0;
	while (input != EOF) {

		while (input == ' ') {
			input = getchar();
			more_digs = 0;

		}

		if (isdigit(input)) {
			//printf("number\n");// vi har en siffra!!
			int temp = 0;
			if (more_digs == 0) {
				temp = input - '0';
				push(stack, temp);
			} else {
				temp = pop(stack);
				temp = temp*10 + (input - '0');		//ex: temp == 5, input == 7 => temp = 57
				push(stack, temp);
			}
			++more_digs;
			//printf("more_digs: %d\n", more_digs);
		}
		int a = 0;
		int b = 0;

		if (input == '+') {
			//printf("add\n");//add
			a = pop(stack);
			b = pop(stack);
			a = a + b;
			push(stack, a);
			more_digs = 0;

		}
		if (input == '-') {
			//printf("sub\n");//sub
			a = pop(stack);
			b = pop(stack);
			b = b - a;
			push(stack, b);
			more_digs = 0;
		}
		if (input == '*') {
			//printf("mul\n");//mul
			a = pop(stack);
			b = pop(stack);
			a = a * b;
			push(stack, a);
			more_digs = 0;

		}
		if (input == '/') {
			//printf("div\n");
			a = pop(stack);
			b = pop(stack);
			if (b == 0) {
				printf("Illigal division with zero!\n");
				exit(1);
			} else {
				b = b / a;
				push(stack, b);
			}
			more_digs = 0;
		} 
		if (isalpha(input)) {
			printf("Only numbers are premitted.\n");
			exit(1);
		}
		input = getchar();
	}
	printf("Answer: %d\n", pop(stack));
	free_stack(stack);
	return 0;
}