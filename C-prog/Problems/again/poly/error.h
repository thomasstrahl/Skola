#ifndef error_h
#define error_h

char*	progname;	// program name. should be set in main from argv[0].

#define error(...)	(error(__FILE__, __LINE__, __func__, __VA_ARGS__))

void (error)(char* file, unsigned long line, const char* func, char* msg, ...);

#endif
