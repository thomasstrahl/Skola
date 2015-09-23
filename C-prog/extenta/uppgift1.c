# include <string.h>
# include <stdio.h>

char* my_strcat(char* dest, char* src) 
{
	int i = 0;
	while (src[i] != '\0') {
		dest[strlen(dest) + i] = src[i];
		++i;
	}
	return dest;
}


char* my_cat(char* dest, char* src) 
{
	strcpy(dest + strlen(dest), src);

	return dest;
}


int main(void)
{
	char dest[20] = "hej";
	char src[10] = "på dig";
	char* res = my_strcat(dest, src);
	printf("gick de?\n");
	char* my_res = my_cat(dest, src);

	if (strcmp(res, my_res) == 0){
		printf("yes!\n");
		printf("res: %d\n",strlen(res));
		printf("my_res: %d\n",strlen(my_res));
		printf("%s\n", res);
		printf("%s\n", my_res);
	} else {
		printf("no!\n");
	}
	return 0;
}