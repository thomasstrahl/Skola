#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int main()
{
	char a[] = "Arrays";
	char* p;
	p = a;

	if ( sizeof(p) == sizeof(a)) {

		printf("de va som fan\n");
	} else {

		printf("Som sagt är pekare och Arrays inte samma sak i C. Sizeof på en pekare returnerar storleken av pekaren (beroende av datorn 32bit eller 64bit), men en array kommer returnera längden på det som står i vektorn + 1 (null pekaren i slutet av strängen)\n");
		printf("%d\n", sizeof(a));
		printf("%d\n", sizeof(p));
	}
	return 0;

}