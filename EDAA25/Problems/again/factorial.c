#include <stdio.h>

#define SIZE	(1000)

int main(int argc, char const *argv[])
{
	int res[SIZE];
	int num = 0;
	scanf("%d", &num);
	int count = 1;
	res[0] = 1;
	int overflow = 0;
	int temp = 0;
	int i, j;
	for (i = 1; i <= num; ++i)
	{
		for (j = 0; j < count; ++j)
		{
			temp = res[j] * i + overflow;
			res[j] = temp % 10;
			overflow = temp / 10;
		}
		while (overflow > 0) {
			res[count] = overflow % 10;
			overflow = overflow / 10;
			++count;
		}
	}
	for (i = count -1; i >= 0; --i)
	{
		printf("%d", res[i]);
	}
	printf("\n");
	return 0;
}