//Thomas Strahl, 910612-5835. 
#include<stdio.h>

#define SIZE (158)
#define RADIX (10)

int factorail(int number)
{
	int result[SIZE];
	int overflow = 0; 
	int temp = 0;
	result[0] = 1;
	int count = 1;
	int j, i;
	for (j = 1; j <= number; j++){
		for (i = 0; i < count; i++){
			temp = result[i]*j + overflow;
			result[i] = temp%RADIX;
			overflow = temp/RADIX;
		}
		while(overflow > 0){
			result[count] = overflow%RADIX;
			overflow = overflow/RADIX; 
			count++;
		}	
	}
	for(i = count-1; i >=0; --i){
		printf("%d", result[i]);
	}
	printf("\n");
}

int main(int argc, char** argv)
{
	printf("Factorial!\nEnter number, (0 <= number <= 100)\n");
	int number;
	scanf("%d", &number);
	factorail(number);
	return 0;
}