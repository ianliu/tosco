/*  S88modeling - Seismic modeling tool
 *  Copyright (C) 2009-2014 Ricardo Biloti <biloti@ime.unicamp.br>
 * 
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _LU_H
#define _LU_H

#include <glib.h>
#include <s88.h>

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

typedef struct {

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


typedef struct {

	int nint;
	int nrays;
	int nwavecode;
	int ns[MAX_CODEWAVES];
	ray_t ray[MAX_NRAYS];
	interface_t interf[MAX_NINTERF];
	float tmax;

} lu_t;


int lu_parse (gchar * lufilename, lu_t * lu);
void lu_free (lu_t * lu);
int export_interf(lu_t * lu, struct s88 *p);
int export_velocity(lu_t * lu, struct s88 *p);
float interf(interface_t * s, float x);
int agr_write (gchar * agrfilename, lu_t * lu, struct s88 *p);

#endif
