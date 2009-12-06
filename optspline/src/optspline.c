/* optspline - Optimally fit a cubic spline to sampled points
 * Copyright (C) 2001-2009 Ricardo Biloti <biloti@ime.unicamp.br>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * $Id$
 */

#include "optspline.h"

/*
 * Function: optspline
 * Optimal spline fit.
 *
 * This routine fits a cubic spline with _nno_ knots to a given data
 * set, composed by _N_ pairs, which coordenates are stored in vectors
 * _X_ and _Y_. The coordenates of the obtained spline knots are
 * stored in vectors _nox_ and _noy_. The _M_ parameter sets the
 * maximum number of iterations the optimization algorithm can
 * perform.
 *
 * The optimal spline satisfies the minimizes the functional
 * >
 * sum ( |s(Xi)-Yi|^2 )
 * >
 * Optionally, each term in the objective function can be weighted by
 * factor, yielding an weighted optimal splines. In that case, the
 * vector _C_, with the same dimension of vectors _X_ and _Y_, stores
 * the weights, and the objective functions becomes
 * >
 * sum ( Ci * |s(Xi)-Yi|^2 )
 * >
 * To avoid weigths, just set C to NULL.
 *
 * Input:
 * M, N, X, Y, C, nno
 *
 * Output:
 * nox, noy
 *
 * Return:
 * Objective function value at the obtained solution.
 */
double optspline(const size_t     M,
		 const size_t     N,
		 const double    *X,
		 const double    *Y,
		 const double    *C,
		 const int   refine,
		 const int      nno,
		 double        *nox,
		 double        *noy,
		 const int  verbose)
{
   
   data_points_t data;
   double tol, norm;

   size_t iter = 0;
   int status, i, j;
   double residue;

   const gsl_multimin_fdfminimizer_type *T;
   gsl_multimin_fdfminimizer *s;
   
   gsl_vector *v;
   gsl_multimin_function_fdf F;

   F.f = &f;
   F.df = &df;
   F.fdf = &fdf;
   F.n = 2*nno;
   F.params = &data;

   data.N = N;
   data.X = (double *) X;
   data.Y = (double *) Y;
   data.C = (double *) C;

   data.weighted = !(data.C == NULL);

   v = gsl_vector_alloc(2*nno);
   
   if (refine){
      
      for (i=0; i<nno; i++){
      gsl_vector_set(v, i, nox[i]);
      gsl_vector_set(v, i+nno, noy[i]);
      }
   }
   else{
      /* Define a condição inicial */
      j = N/(nno-1);
      for (i=0; i<nno-1; i++){
	 gsl_vector_set(v, i, X[i*j]);
	 gsl_vector_set(v, i+nno, Y[i*j]);
      }
      
      gsl_vector_set(v, nno-1,   X[N-1]);
      gsl_vector_set(v, 2*nno-1, Y[N-1]);
   }
   
   /*T = gsl_multimin_fdfminimizer_steepest_descent;*/
   /*T = gsl_multimin_fdfminimizer_conjugate_pr;*/
   /*T = gsl_multimin_fdfminimizer_conjugate_fr;*/
   T = gsl_multimin_fdfminimizer_vector_bfgs;

   s = gsl_multimin_fdfminimizer_alloc(T, 2*nno);

   gsl_multimin_fdfminimizer_set(s, &F, v, 1, 1e-4);

   fdf(s->x, &data, &s->f, s->gradient);
   norm = gsl_blas_dnrm2(s->gradient);
   tol = 1.0e-2 * norm;

   if (verbose){
      fprintf(stderr, "convergence test: |g| < %e\n", tol);
      fprintf(stderr, "iteration   %4i: |g| = %e, f = %e\n", 0, norm, s->f);
   }
   do{
   
      iter++;
      status = gsl_multimin_fdfminimizer_iterate(s);
      
      if (status)
	 break;
      
      norm = gsl_blas_dnrm2(s->gradient);

   }
   while ((norm > tol) && iter < M);

   if (verbose){
      fprintf(stderr, "iteration   %4i: |g| = %e, f = %e\n", 
	      (int) iter, norm, s->f);

      if (norm > tol)
	 fprintf(stderr, "%s\n", gsl_strerror(status));
      else
	 fprintf(stderr, "convergence\n");
   }

 
   for (i=0; i<nno; i++){
      nox[i] = gsl_vector_get(s->x, i);
      noy[i] = gsl_vector_get(s->x, i+nno);
   }

   residue = s->f;
   
   gsl_vector_free(v);
   gsl_multimin_fdfminimizer_free(s);

   return residue;

}

/***********************************************************************/


double f(const gsl_vector *v, void *params)
{

  int nno;
  double *nox, *noy;

  nno = v->size/2;

  nox = v->data;
  noy = &(v->data[nno]);

  sort(nno,nox,noy);

  return feval(nno, nox, noy, params);

}

/***********************************************************************/

void df(const gsl_vector *v, void *params, gsl_vector *g)
{
  int j, nno;
  double *nox, *noy, temp, eps;
  double aux1, aux2;

  nno = v->size/2;

  nox = v->data;
  noy = &(v->data[nno]);
  
  sort(nno, nox, noy);
  
  for (j=0; j < nno; j++)
    {
      eps    = EPS(nox[j]);
      temp   = nox[j];
      nox[j] = temp + eps;
      aux1   = feval(nno,nox,noy, params);
      nox[j] = temp - eps;
      aux2   = feval(nno,nox,noy, params);
      g->data[j]   = (aux1-aux2)/(2*eps);
      nox[j] = temp;

      eps      = EPS(noy[j]);
      temp     = noy[j];
      noy[j]   = temp + eps;
      aux1     = feval(nno,nox,noy, params);
      noy[j]   = temp - eps;
      aux2     = feval(nno,nox,noy, params);
      g->data[nno+j] = (aux1-aux2)/(2*eps);
      noy[j]   = temp;
    }

  /* boundary points remain fix in x */
  g->data[0]=0;
  g->data[nno-1]=0;
  
}

void fdf(const gsl_vector *v, void *params, 
	 double *F, gsl_vector *g)
{
   
   *F = f(v, params);
   df(v, params, g);

}


/***********************************************************************/

double feval(int n, double *x, double *y, void *params)
{
  size_t i;
  double *xx, *yy, *b, *c, *d;
  double aux2;
  data_points_t *p;


  p = (data_points_t *) params;

  b = (double *) malloc(n * sizeof(double));
  c = (double *) malloc(n * sizeof(double));
  d = (double *) malloc(n * sizeof(double));

  spline(&n, x, y, b, c, d);

  aux2 = 0;

  xx = (double *) p->X;
  yy = (double *) p->Y;

  if (p->weighted){

     for (i=0; i<p->N; i++)
	{
	   double aux1 = 0; 

	   aux1 = seval(&n, xx++, x, y, b, c, d) - *(yy++);
	   aux2 += p->C[i] * aux1*aux1;
	}
  }
  else{

     for (i=0; i<p->N; i++)
	{
	   double aux1 = 0;

	   aux1 = seval(&n, xx++, x, y, b, c, d) - *(yy++);
	   aux2 += aux1 * aux1;
	}
  }
  
  free(b);
  free(c);
  free(d);

  return aux2;
}

/***********************************************************************/

void sort(int n, double *x, double *y)
{
  int i, j;
  double temp;

  for (i=0; i<n-1; i++)
    for (j=0; j<n-1-i; j++)
      if (x[j] > x[j+1])
	{
	  
	  temp = x[j];
	  x[j] = x[j+1];
	  x[j+1] = temp;

	  temp = y[j];
	  y[j] = y[j+1];
	  y[j+1] = temp;

	}
}
