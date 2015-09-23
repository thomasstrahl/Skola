#include <stdio.h>

int* issigned(size_t n)
{
	static int (*a)[n];
	return a[1];
}

int main(int argc, char* argv[])
{
	char** param;
	for (param = argv; *param; param++)
	{
		printf("%s\n", *param);
	}
	return 0;
}

