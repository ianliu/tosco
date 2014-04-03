#ifndef _LU_H
#define _LU_H

#include <glib.h>

#ifndef max
#define max(x,y)         ( ((x) < (y)) ? (y) : (x) )
#endif

#ifndef min
#define min(x,y)         ( ((x) > (y)) ? (y) : (x) )
#endif

#define MAX_NRAYS        100000
#define MAX_CODEWAVES    400
#define MAX_NINTERF      100
#define NSAMPLEX         151

typedef struct{

        int n;
        float *x;
        float *z;
        float *b;
        float *c;
        float *d;
        int *iii;

} interface_t;

typedef struct {

        int n;
        int ind;

        float *x;
        float *z;

} ray_t;


typedef struct{

        int            nint;
        int            nrays;
        int            nwavecode;
        int            ns[MAX_CODEWAVES];
        ray_t          ray[MAX_NRAYS];
        interface_t    interf[MAX_NINTERF];
        float          tmax;

} lu_t;


int lu_parse(gchar *lufilename, lu_t *lu);
void lufree(lu_t *lu);


#endif
