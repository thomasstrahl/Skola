#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char* strdup(const char* s)
{
	if (s != NULL) {
		char* t;
		t = malloc(strlen(s) + 1);
		if (t == NULL) {
			exit(1);
		}
		int n = 0;
		while(s[n] != '\0') {
			t[n] = s[n];
			++n;
		}
		return t;
	}
}	

size_t charbi()
{
	unsigned char c = 0;
	c = ~c;
	size_t i = 1;
	while (c >> i) {
		++i;
	}
	return i;
}

int uppgift(void)
{
	signed int a;
	signed int *p;
	signed short *q;
	a = 1;
	p = &a;
	q = (signed short *)p;
	*q = 2;
	printf("a = %d\n", a);
	return 0;
}


			

int main (void) 
{
	printf("%zu\n", uppgift());
}