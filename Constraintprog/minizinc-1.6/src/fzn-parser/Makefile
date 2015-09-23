EXEEXT=

EXE_FILE = 	fzp$(EXEEXT)
SRC =	        
OBJ = 		flatzinc.tab.o
GENCFILES = 	flatzinc.tab.c lex.yy.c 
OTHER = 	flatzinc.output 

# NOTE: we need to pass the `-l' flag to ensure the scanner internal
# variable `yylineno' is available.
#
LEX =		flex
LEXFLAGS =      -l
LEXLIB =	

YACC =		bison
YACCFLAGS =	-v

CC =		gcc	
CFLAGS =	-g -O
RM =		/bin/rm -f

$(EXE_FILE):	$(OBJ)
		$(CC) $(CFLAGS) -o $(EXE_FILE) $(OBJ) $(LEXLIB)

flatzinc.tab.c:	flatzinc.y lex.yy.c
		$(YACC) $(YACCFLAGS) flatzinc.y

lex.yy.c:	flatzinc.l
		$(LEX) $(LEXFLAGS) flatzinc.l

clean:
		$(RM) $(OBJ) $(GENCFILES) $(OTHER) core

clobber:	clean
		$(RM) $(EXE_FILE)

.PHONY: distclean
distclean: clobber
