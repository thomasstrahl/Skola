//Thomas Strahl, dat11tst, 910612-5835, Resubmitted version, last examiner Jonas/Johan
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "poly.h"

typedef struct term_t term_t; 

struct poly_t {
	term_t* first;
	term_t* last;
};

struct term_t {
	int deg, cof;
	term_t* next;
};

static term_t* new_term(int cof, int deg)
{
	term_t* term;
	term = malloc(sizeof(term_t));
	if (term == NULL) {
		fprintf(stderr, "%s\n", "Malloc returned null" );
		exit(1);
	}
	term->next = NULL;
	term->cof = cof;
	term->deg = deg;
	return term; 
}

void insert(poly_t** head, int cof, int deg)
{
	term_t* term;
	term_t* temp;
	term = new_term(cof, deg);
	if ((*head)->first == NULL) {
		(*head)->first = term;
		(*head)->last = term;	
		return;
	}
	temp = (*head)->last;
	if (temp->deg == deg) {
		temp->cof = temp->cof + cof;
		free(term);
	}
	else {
		temp->next = term;	
		(*head)->last = term;			
	}
}

poly_t* new_poly_from_string(const char* a)
{
	poly_t* head;
	head = malloc(sizeof(poly_t));
	if (head == NULL) {
		fprintf(stderr, "%s\n", "Malloc returned null" );
		exit(1);
	}
	head->first = NULL;
	head->last = NULL;
	
	int count = 0;
	int c = a[count];
	int cof = 0;
	int deg = 0;
	int sign = 1;

	while (c != '\0') {
		cof = 0;
		deg = 0;
		if (c == '-') {
			sign = -1;
			c = a[++count]; 
		}
		else if (c == '+') {
			sign = 1;
			c = a[++count];
		}
		if (c == 'x') {
			cof = 1;
		}
		if (c == ' ') {
			c = a[++count];
		}
		while (isdigit(c)) {
			cof = 10*cof + c -'0';
			c = a[++count];
		}
		cof = sign*cof;
		if (c == 'x') {
			c = a[++count];
			if (c == '\0') {
				deg = 1;
			}
			if (c == ' ') {
				deg = 1;
				c = a[++count];	
			}
			if (c == '^') {
				c = a[++count];
			}
			while (isdigit(c)) {
				deg = 10*deg + c - '0';
				c = a[++count];
			}
			if (c == ' ') {
				c = a[++count];	
			}
		}
		insert(&head, cof, deg);
	}
	return head;
}

void free_poly(poly_t* head)
{
	term_t* p;
	term_t* q;

	p = head->first;
	free(head);
	
	while (p != NULL) {
		q = p->next;
		free(p);
		p = q;
	}
}

poly_t* mul(poly_t* p, poly_t* q)
{
	poly_t* result;
	term_t* p_term;
	term_t* q_term;
	term_t* temp;
	p_term = p->first;
	q_term = q->first;
	temp = q_term;
	
	result = malloc(sizeof(poly_t));
	if (result == NULL) {
		fprintf(stderr, "%s\n", "Malloc returned null" );
		exit(1);
	}
	result->last = NULL;
	result->first = NULL;
	while (p_term != NULL) {
		while (q_term != NULL) {
			insert(&result,(p_term->cof)*(q_term->cof), (p_term->deg) + (q_term->deg));
			q_term = q_term->next;
		}
		p_term = p_term->next;
		q_term = temp;
	}
	return result;
}

void print_poly(poly_t* head)
{
	if (head == NULL) {
		fprintf(stderr,"%s\n","No valid pointer!");
		exit(1);
	}
	term_t* term;
	term = head->first;
	char power = '^';
	char base = 'x';
	char operation;
	int first_term = 1;
	while (term != NULL) {
		if (term->cof > 0) {
			operation = '+';
		} 
		else {
			operation = '-';
		}
		if (first_term == 1) {
			if (term->cof < 0) {
				printf("%d", term->cof + (0 - term->cof)*2);	
			}
			else if (term->cof == 1 && term->deg > 0) {
			}
			else {
				printf("%d ", term->cof);
			}
			if (term->deg > 1) {
				printf("%c%c%d" ,base,power,term->deg);
			}
			if (term->deg == 1) {
				printf("%c",base);
			}
			first_term = 0;
		}
		else {
			if (term->next != NULL) {
				if (term->cof < 0) {
					printf(" %c %d ",operation, term->cof + (0 - term->cof)*2);	
				}
				else if (term->cof == 1 && term->deg > 0) {
				}
				else {
					printf(" %c %d ", operation, term->cof);
				}
			}
			else {
				if (term->cof < 0) {
				printf(" %c %d",operation, term->cof + (0 - term->cof)*2);	
				}
				else if (term->cof == 1 && term->deg > 0) {
				}
				else {
					printf(" %c %d", operation, term->cof);
				}
			}
			if (term->deg > 1) {
				printf("%c%c%d" ,base,power,term->deg);
			}
			if (term->deg == 1) {
				printf("%c", base);
			}
		}
		term = term->next;
	}
	printf("\n");
}