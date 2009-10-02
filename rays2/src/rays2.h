/*  rays2 - Draw seismic rays using grace
 *  Copyright (C) 2009 Ricardo Biloti <biloti@ime.unicamp.br>
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

#ifndef RAYS2_H
#define RAYS2_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libagr.h"
#include "spline.h"
#include "cmdline.h"

#define max(x,y)         ( ((x) < (y)) ? (y) : (x) )
#define min(x,y)         ( ((x) > (y)) ? (y) : (x) )

#define TRUE             1
#define FALSE            0

#define MAX_NRAYS        10000
#define MAX_CODEWAVES    40
#define MAX_NINTERF      20

#define nsamplex         150


typedef struct {

  int n;
  int ind;

  float *x;
  float *z;
} ray_t;

typedef struct{

  int n;
  float *x;
  float *z;
  float *b;
  float *c;
  float *d;
  int *iii;
} interface_t;

void ray_read(FILE *fp, int n, int ind, ray_t *ray);
void ray_free(ray_t *ray);

void
interface_alloc(interface_t *interf, int npts);

void
interface_free(interface_t *interf);

void
interface_read(FILE *fp, interface_t *interf);

void
interface_dummy_read(FILE *fp, int nn);

void
strip   (float        xmin,
	 float        xmax, 
	 interface_t *s1,
	 interface_t *s2,
	 unsigned int cor);

void
SetAGRMyPalette();





#endif
