#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <pthread.h>
#include "dataflow.h"
#include "error.h"
#include "list.h"
#include "set.h"

typedef struct vertex_t	vertex_t;
typedef struct task_t	task_t;

/* cfg_t: a control flow graph. */
struct cfg_t {
	size_t			nvertex;	/* number of vertices		*/
	size_t			nsymbol;	/* width of bitvectors		*/
	vertex_t*		vertex;		/* array of vertex		*/
};

/* vertex_t: a control flow graph vertex. */
struct vertex_t {
	size_t			index;		/* can be used for debugging	*/
	set_t*			set[NSETS];	/* live in from this vertex	*/
	set_t*			prev;		/* alternating with set[IN]	*/
	size_t			nsucc;		/* number of successor vertices */
	vertex_t**		succ;		/* successor vertices 		*/
	list_t*			pred;		/* predecessor vertices		*/
	bool			listed;		/* on worklist			*/
};

static void clean_vertex(vertex_t* v);
static void init_vertex(vertex_t* v, size_t index, size_t nsymbol, size_t max_succ);

cfg_t* new_cfg(size_t nvertex, size_t nsymbol, size_t max_succ)
{
	size_t		i;
	cfg_t*		cfg;

	cfg = calloc(1, sizeof(cfg_t));
	if (cfg == NULL)
		error("out of memory");

	cfg->nvertex = nvertex;
	cfg->nsymbol = nsymbol;

	cfg->vertex = calloc(nvertex, sizeof(vertex_t));
	if (cfg->vertex == NULL)
		error("out of memory");

	for (i = 0; i < nvertex; i += 1)
		init_vertex(&cfg->vertex[i], i, nsymbol, max_succ);

	return cfg;
}

static void clean_vertex(vertex_t* v)
{
	int		i;

	for (i = 0; i < NSETS; i += 1)
		free_set(v->set[i]);
	free_set(v->prev);
	free(v->succ);
	free_list(&v->pred);
}

static void init_vertex(vertex_t* v, size_t index, size_t nsymbol, size_t max_succ)
{
	int		i;

	v->index	= index;
	v->succ		= calloc(max_succ, sizeof(vertex_t*));

	if (v->succ == NULL)
		error("out of memory");
	
	for (i = 0; i < NSETS; i += 1)
		v->set[i] = new_set(nsymbol);

	v->prev = new_set(nsymbol);
}

void free_cfg(cfg_t* cfg)
{
	size_t		i;

	for (i = 0; i < cfg->nvertex; i += 1)
		clean_vertex(&cfg->vertex[i]);
	free(cfg->vertex);
	free(cfg);
}

void connect(cfg_t* cfg, size_t pred, size_t succ)
{
	vertex_t*	u;
	vertex_t*	v;

	u = &cfg->vertex[pred];
	v = &cfg->vertex[succ];

	u->succ[u->nsucc++ ] = v;
	insert_last(&v->pred, u);
}

bool testbit(cfg_t* cfg, size_t v, set_type_t type, size_t index)
{
	return test(cfg->vertex[v].set[type], index);
}

void setbit(cfg_t* cfg, size_t v, set_type_t type, size_t index)
{
	set(cfg->vertex[v].set[type], index);
}


//skapad av mig!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
void* thread_func(void* data_struct) 
{
	cfg_t* cfg = (cfg_t*)data_struct;
	vertex_t*	u;
	vertex_t*	v;
	set_t*		prev;
	size_t		i;
	size_t		j;
	list_t*		worklist;
	list_t*		p;
	list_t*		h;

	printf("i thread_func:  vertex: %zu nsymbol: %zu \n",cfg->nvertex, cfg->nsymbol);


	worklist = NULL;

	for (i = 0; i < cfg->nvertex; ++i) {
		u = &cfg->vertex[i];

		insert_last(&worklist, u);
		u->listed = true;
	}

	while ((u = remove_first(&worklist)) != NULL) {
		u->listed = false;

		reset(u->set[OUT]);

		for (j = 0; j < u->nsucc; ++j)
			or(u->set[OUT], u->set[OUT], u->succ[j]->set[IN]);

		prev = u->prev;
		u->prev = u->set[IN];
		u->set[IN] = prev;

		/* in our case liveness information... */
		propagate(u->set[IN], u->set[OUT], u->set[DEF], u->set[USE]);

		if (u->pred != NULL && !equal(u->prev, u->set[IN])) {
			p = h = u->pred;
			do {
				v = p->data;
				if (!v->listed) {
					v->listed = true;
					insert_last(&worklist, v);
				}

				p = p->succ;

			} while (p != h);
		}
	}
	return NULL;
}

//ska vi använda liveness som thread_func eller ska vi ha en separat 
//och bara låta liveness starta trådarna?
void liveness(cfg_t* cfg)
{
	size_t		k;
	size_t		nthread = 4;
	//vad behövs i structen? kan man använda samma com cfg
	//eller måste vi skapa en egen??

	pthread_t t[nthread];
	pthread_t thread;
	for (k = 0; k < nthread ; ++k) {
		 cfg_t data_struct;
		printf("i for-loopen\n");
		 data_struct.nvertex = cfg->nvertex/nthread;
		 printf("DEtta borde vara lungt: nvertex/nactive\n");
		 data_struct.nsymbol = cfg->nsymbol;
		 data_struct.vertex = cfg->vertex + (k * cfg->nvertex/nthread);
		 printf("vår uträkning: %zu\n", (k * cfg->nvertex/nthread));
		 printf("i liveness:  k: %zu vertex: %zu nsymbol: %zu \n", k, data_struct.nvertex, data_struct.nsymbol);
		if (pthread_create(&thread, NULL, thread_func, &data_struct)) {
			printf("Failed to create thread!\n");
			exit(1);
		}
		t[k] = thread;
	}

	for (k = 0; k < nthread; ++k) {
		if (pthread_join(t[k], NULL)) {
			printf("Failed to join theads!\n");
			exit(1);
		}
	}
}

void print_sets(cfg_t* cfg, FILE *fp)
{
	size_t		i;
	vertex_t*	u;

	for (i = 0; i < cfg->nvertex; ++i) {
		u = &cfg->vertex[i]; 
		fprintf(fp, "use[%zu] = ", u->index);
		print_set(u->set[USE], fp);
		fprintf(fp, "def[%zu] = ", u->index);
		print_set(u->set[DEF], fp);
		fputc('\n', fp);
		fprintf(fp, "in[%zu] = ", u->index);
		print_set(u->set[IN], fp);
		fprintf(fp, "out[%zu] = ", u->index);
		print_set(u->set[OUT], fp);
		fputc('\n', fp);
	}
}
