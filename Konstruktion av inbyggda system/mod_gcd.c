#include <stdio.h>

int main()
{
	int a;
	int b;
	printf("Enter two integers on form A B\n");
	scanf("%d %d", &a, &b);

	if (a == 0)
		printf("%d\n",b);

	if (b == 0)
		printf("%d\n",a);


	int t = 0;
	long start = clock();
	long diff = 0;
	while (b != 0) {
		t = b;
		b = a % b;
		a = t;
	}
		
		diff = clock() - start;

		printf("%d %lu\n",a, diff);
}