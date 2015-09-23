#include <stdlib.h>
#include <stdio.h>
#include <string.h>




void f( const char a[], char* p)
{
	if ( sizeof(p) == sizeof(a)) {
		printf("En array som skickas in en funktion konverteras till en pekare av effektivitets skäl. Det medför att den här jämförelsen kommer att bli sann! Eftersom båda är char pekare och på denna datorn kommer de att returnera 8 byte\n");

	} else {

		printf("de va som fan\n");
	}
}

int main()
{

	char* a = "Arrays";
	f(a, "and pointers");
	return 0;

}