/***************************************************************************/
/*                                                                         */
/* optspline.h - a micro library to optimally fit a cubic spline           */
/*               to sampled points of a real function                      */
/* Copyright (C) 2001-2014 Ricardo Biloti <biloti@ime.unicamp.br>          */
/*                    http://www.ime.unicamp.br/~biloti                    */
/*                                                                         */
/* This program is free software; you can redistribute it and/or modify    */
/* it under the terms of the GNU General Public License as published by    */
/* the Free Software Foundation; either version 2, or (at your option)     */
/* any later version.                                                      */
/*                                                                         */
/* This program is distributed in the hope that it will be useful,         */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of          */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* GNU General Public License for more details.                            */
/*                                                                         */
/* You should have received a copy of the GNU General Public License       */
/* along with this program; if not, write to the                           */
/* Free Software Foundation, Inc., 59 Temple Place - Suite 330,            */
/* Boston, MA 02111-1307, USA.                                             */
/*                                                                         */
/***************************************************************************/
#ifndef OPTSPLINE_H
#define OPTSPLINE_H 1

#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_multimin.h>
#include "splined.h"

#define max(x,y)         ( ((x) < (y)) ? (y) : (x) )
#define EPS(x)           ( max( 1.0e-5*fabs((x)), 1.0e-10 ) )

double
f            (const gsl_vector *v,
  	      void             *params);

void
df           (const gsl_vector *v,
	      void             *params,
	      gsl_vector       *g);

void
fdf          (const gsl_vector *v,
	      void             *params, 
	      double           *f,
	      gsl_vector       *g);

double
feval        (int               n,
	      double           *x,
	      double           *y,
	      void             *params);

void
sort         (int               n,
	      double           *x,
	      double           *y);

double
optspline    (const size_t      M,
	      const size_t      N,
	      const double     *X,
	      const double     *Y,
	      const double     *C,
	      const int    refine,
	      const int       nno,
	      double         *nox,
	      double         *noy,
              double       tolrel,
	      const int   verbose);

typedef struct {
   size_t N;
   const double *X;
   const double *Y;
   const double *C;
   int weighted;
} data_points_t ;


#endif
