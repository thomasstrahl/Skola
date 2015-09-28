CC		= gcc

CFLAGS		= -Wall -pedantic -std=c99 -m32 -g
CFLAGS		= -m64 -g -Wall -pedantic -Werror -std=c99
CFLAGS		= -O3 -maltivec -Wall -pedantic -std=c99
CFLAGS		= -O3 -Wall -pedantic -std=c99

OBJS		= main.o list.o error.o random.o set.o dataflow.o

OUT		= live

LDFLAGS		= -lpthread 

S=100
V=100
U=4
A=100
T=4
P=1

all: $(OBJS)	
	$(CC) $(CFLAGS) $(OBJS) $(LDFLAGS) -o $(OUT)
	#valgrind --tool=helgrind ./$(OUT) $(S) $(V) $(U) $(A) $(T) $(P) $(L)
	./$(OUT) $(S) $(V) $(U) $(A) $(T) $(P) > result
	diff correct result
clean:
	rm -f $(OUT) $(OBJS) cfg.dot
	rm -f result
