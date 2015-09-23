//Thomas Strahl, 910612-5835. Resubmitted version, last examiner Andrée

#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>

#define SIZE (15)

int stack[SIZE];
int count = 0;

void push(int value)
{	
	if(count > SIZE-1){
		fprintf(stderr, "Tryed to push full stack.\n");
		exit(1);
	}else{
		stack[count] = value;
		count++;
	}	
}

int pop(void)
{
	if(count < 0){
		fprintf(stderr,"Tryed to pop empty stack.\n");
		exit(1);
	}else{	
		count--;
		int temp = stack[count];
		return temp;
	}
}

int main(int argc, char** argv)
{
	int var1 = 0;
	int var2 = 0;
	int input = getchar();
	int space =1;
	while(input != EOF){
		if(isdigit(input)){
			if(space == 1){
				push(input-'0');
				space = 0;
			}else{
				var1 = pop();
                       		var1 = (var1*10)-'0';
                      		push(var1 + input);
			}
		}else{	
			if(input == ' '){
				space = 1;		
			}	
			if(input == '\n'){
				printf("%d\n", pop());
				count = 0;
			}		
			if(input == '+'){
				var1 = pop();
				var2 = pop();
				push(var1+var2);
			}
			if(input == '-'){
				var1 = pop();
				var2 = pop();
             			push(var2-var1);
			}
			if(input == '/'){
				var1 = pop();
				var2 = pop();
				if(var1 == 0){
					fprintf(stderr, "Illegal division!\n");
					exit(1);
				}
               			push(var2/var1);
			}
			if(input == '*'){ 
				var1 = pop();
				var2 = pop();
               		       	push(var1*var2);
			}
			if(isalpha(input)){
				fprintf(stderr, "Illegal input!\n");
				exit(1);
			}
		}
		input = getchar(); 
	}
	return 0;
}
