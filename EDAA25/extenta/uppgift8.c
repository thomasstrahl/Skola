# include <stdio.h>
# include <math.h>

#define my_isnan(a)	(!((a) == (a)))


int main(void)
{	
	if (my_isnan(NAN))
	{
		printf("Yes a is a NaN\nThis works because all comperisons with a nan are false!\n");
	} else {
		printf("No a is NOT a NaN\n");
	}
	return 0;
}