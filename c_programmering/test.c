# include <stdio.h>
# include <stdlib.h>
# include <ctype.h>
# include <stdbool.h>
# include <string.h>

#define SIZE (10)

typedef struct list_t list_t;

struct list_t {
	size_t n;
	char* s;
	list_t* next;
};

list_t* new_list(char* word)
{
	list_t* p;
	p = malloc(sizeof(list_t));
	if (p == NULL) {
		printf("%s\n", "out of memory");
		exit(1);
	}
	p->n = 1;
	p->next = NULL;
	p->s = malloc(strlen(word) + 1);
	if (p->s == NULL) {
		printf("%s\n", "out of memory");
		exit(1);
	}
	p->s = word;
	return p;
}

void insert_first(list_t* p, list_t** h)
{
	if ((*h)->next == NULL) {
		(*h)->next = p;
	}
	else {
	printf("%s\n", "Lungt?" );
	p->next = (*h)->next;
	printf("%s\n", "Lungt?" );
	(*h)->next = p;
	printf("%s\n", "Lungt?" );
	}
}

void free_list(list_t* h) 
{
	list_t* p;
	list_t* q;
	while(h != NULL) {
		q = p->next;
		free(p->s);
		free(p);
		p = q;
	}
}

int main(char argc, char** argv) 
{
	list_t* h;
	bool first = 1;
	int c;
	size_t i = 0;
	char* word;
	word = malloc(SIZE);
	if (word == NULL) {
		printf("%s\n", "Out of memory");
		exit(1);
	}
	while ((c = getchar()) != EOF) {
		while (isalpha(c) != 0) {
			if (strlen(word) == i + 1) {
				char* temp;
				temp = realloc(word, SIZE*2);
				if (temp == NULL) {
					printf("%s\n", "Out of memory");
					exit(1);
				}
				word = temp;
			}
			word[i] = c;
			++i;
			c = getchar();
		}
		i = 0;
		if (first) {
			h = new_list(word);
			first = 0;
		}
		else {
			bool isalike = 0;
			while (h != NULL) {
				if (strcmp(h->s, word) == 0) {
					h->n += 1;
					isalike = 1;
					break;
				}
				h = h->next;
			}
			if (!isalike) {
				list_t* new_link = new_list(word);
				printf("%s\n", "Lungt?" );
				insert_first(new_link, &h);
				
			}
		}
	}
	while (h != NULL) {
		for (int j = 0; h->s[j] != '\0'; j++) {
			printf("%c", h->s[j]);
		}
		printf(":	%zu\n", h->n);
		h = h->next;
	}
	free(word);
	free(h);
}