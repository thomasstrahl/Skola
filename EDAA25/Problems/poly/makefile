CC	= gcc
CFLAGS	= -g -pedantic -std=c99 -Wall -Werror
OBJS	= main.o poly.o error.o

main: $(OBJS)
	$(CC) $(OBJS) -o a3
	./a3 | tee out
	diff out correct 

clean:
	rm -f *.o a3 out
