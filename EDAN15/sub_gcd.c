#include <stdio.h>
#include <time.h>

int main()
{
	int loops = 0;
	int a;
	int b;
	printf("Enter two integers on form A B\n");
	scanf("%d %d", &a, &b);

	if (a == 0)
		xil_printf("%d\n",b);

	if (b == 0)
		xil_printf("%d\n",a);

	while (a != b) {
		++loops;
		if (a > b) {
			a = a - b;
		} else {
			b = b - a;
		}

	}
	xil_printf("%d %d %lu\n", a, loops);

}