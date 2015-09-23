#ifndef sh_h
#define sh_h

#define MAXBUF	(512)
#define MAX_ARG		(100)

typedef enum { false, true } 				bool;

typedef enum { 
	AMPERSAND, 		/* & */
	NEWLINE,		/* end of line reached. */
	NORMAL,			/* file name or command option. */
	PIPE,			/* | */
	SEMICOLON,		/* ; */
} token_type_t;

#endif
