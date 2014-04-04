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

int spline (int *n, float *x, float *y, float *b, float *c, float *d)
{
        /* System generated locals */
        int i1;
        float d1;

        /* Local variables */
        static int i;
        static float t;
        static int ib, nm1;

    /************************************************************************
      the coefficients b(i), c(i), and d(i), i=1,2,...,n are computed 
      for a cubic interpolating spline 
    	s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3 
    	for  x(i) .le. x .le. x(i+1) 
    
      input.. 
    
    	n = the number of data points or knots (n.ge.2) 
    	x = the abscissas of the knots in strictly increasing order 
    	y = the ordinates of the knots 
    
      output.. 
    	b, c, d  = arrays of spline coefficients as defined above. 
      using  p  to denote differentiation, 
    	y(i) = s(x(i)) 
    	b(i) = sp(x(i)) 
    	c(i) = spp(x(i))/2 
    	d(i) = sppp(x(i))/6  (derivative from the right) 
      the accompanying function subprogram  seval  can be used 
      to evaluate the spline.
    *************************************************************************/

        /* Parameter adjustments */
        --d;
        --c;
        --b;
        --y;
        --x;

        /* Function Body */
        nm1 = *n - 1;
        if (*n < 2) {
                return 0;
        }
        if (*n < 3) {
                goto L50;
        }

        /*  set up tridiagonal system */
        /*  b = diagonal, d = offdiagonal, c = right hand side. */

        d[1] = x[2] - x[1];
        c[2] = (y[2] - y[1]) / d[1];
        i1 = nm1;
        for (i = 2; i <= i1; ++i) {
                d[i] = x[i + 1] - x[i];
                b[i] = (d[i - 1] + d[i]) * (float) 2.;
                c[i + 1] = (y[i + 1] - y[i]) / d[i];
                c[i] = c[i + 1] - c[i];
                /* L10: */
        }

        /*  end conditions.  third derivatives at  x(1)  and  x(n) */
        /*  obtained from divided differences */

        b[1] = -d[1];
        b[*n] = -d[*n - 1];
        c[1] = (float) 0.;
        c[*n] = (float) 0.;
        if (*n == 3) {
                goto L15;
        }
        c[1] = c[3] / (x[4] - x[2]) - c[2] / (x[3] - x[1]);
        c[*n] = c[*n - 1] / (x[*n] - x[*n - 2]) - c[*n - 2] / (x[*n - 1] - x[*n - 3]);
        /* Computing 2nd power */
        d1 = d[1];
        c[1] = c[1] * (d1 * d1) / (x[4] - x[1]);
        /* Computing 2nd power */
        d1 = d[*n - 1];
        c[*n] = -c[*n] * (d1 * d1) / (x[*n] - x[*n - 3]);

        /*  forward elimination */

      L15:
        i1 = *n;
        for (i = 2; i <= i1; ++i) {
                t = d[i - 1] / b[i - 1];
                b[i] -= t * d[i - 1];
                c[i] -= t * c[i - 1];
                /* L20: */
        }

        /*  back substitution */

        c[*n] /= b[*n];
        i1 = nm1;
        for (ib = 1; ib <= i1; ++ib) {
                i = *n - ib;
                c[i] = (c[i] - d[i] * c[i + 1]) / b[i];
                /* L30: */
        }

        /*  c(i) is now the sigma(i) of the text */

        /*  compute polynomial coefficients */

        b[*n] = (y[*n] - y[nm1]) / d[nm1] + d[nm1] * (c[nm1] + c[*n] * (float) 2.);
        i1 = nm1;
        for (i = 1; i <= i1; ++i) {
                b[i] = (y[i + 1] - y[i]) / d[i] - d[i] * (c[i + 1] + c[i] * (float) 2.);
                d[i] = (c[i + 1] - c[i]) / d[i];
                c[i] *= (float) 3.;
                /* L40: */
        }
        c[*n] *= (float) 3.;
        d[*n] = d[*n - 1];
        return 0;

      L50:
        b[1] = (y[2] - y[1]) / (x[2] - x[1]);
        c[1] = (float) 0.;
        d[1] = (float) 0.;
        b[2] = b[1];
        c[2] = (float) 0.;
        d[2] = (float) 0.;
        return 0;
}                               /* spline */

/***************************************************************/

double seval (int *n, float *u, float *x, float *y, float *b, float *c, float *d)
{
        /* Initialized data */
        static int i = 1;

        /* System generated locals */
        float ret_val;

        /* Local variables */
        static int j, k;
        static float dx;


    /***********************************************************************
      this subroutine evaluates the cubic spline function 
    
    	seval = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3 
    
    	where  x(i) .lt. u .lt. x(i+1), using horner's rule 
    
      if  u .lt. x(1) then  i = 1  is used. 
      if  u .ge. x(n) then  i = n  is used. 
    
      input.. 
    
    	n = the number of data points 
    	u = the abscissa at which the spline is to be evaluated 
    	x,y = the arrays of data abscissas and ordinates 
    	b,c,d = arrays of spline coefficients computed by spline 
    
      if  u  is not in the same interval as the previous call, then a 
      binary search is performed to determine the proper interval. 
    ***********************************************************************/

        /* Parameter adjustments */
        --d;
        --c;
        --b;
        --y;
        --x;

        /* Function Body */
        if (i >= *n) {
                i = 1;
        }
        if (*u < x[i]) {
                goto L10;
        }
        if (*u <= x[i + 1]) {
                goto L30;
        }

        /*  binary search */

      L10:
        i = 1;
        j = *n + 1;
      L20:
        k = (i + j) / 2;
        if (*u < x[k]) {
                j = k;
        }
        if (*u >= x[k]) {
                i = k;
        }
        if (j > i + 1) {
                goto L20;
        }

        /*  evaluate spline */

      L30:
        dx = *u - x[i];
        ret_val = y[i] + dx * (b[i] + dx * (c[i] + dx * d[i]));
        return ret_val;
}                               /* seval */

/***************************************************************/

int sgheval (int *n, float *u, float *x, float *y,
             float *b, float *c, float *d, float *s, float *g, float *h)
{
        /* Initialized data */
        static int i = 1;

        /* Local variables */
        static int j, k;
        static float dx;


    /***********************************************************************
      this subroutine evaluates the cubic spline function 
      and its first and second derivatives
    
    	 s = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3 
	 g = s'(u)
	 h = s''(u)
    
    	where  x(i) .lt. u .lt. x(i+1), using horner's rule 
    
      if  u .lt. x(1) then  i = 1  is used. 
      if  u .ge. x(n) then  i = n  is used. 
    
      input.. 
    
    	n = the number of data points 
    	u = the abscissa at which the spline is to be evaluated 
    	x,y = the arrays of data abscissas and ordinates 
    	b,c,d = arrays of spline coefficients computed by spline 
    
      if  u  is not in the same interval as the previous call, then a 
      binary search is performed to determine the proper interval. 
    ***********************************************************************/

        /* Parameter adjustments */
        --d;
        --c;
        --b;
        --y;
        --x;

        /* Function Body */
        if (i >= *n) {
                i = 1;
        }
        if (*u < x[i]) {
                goto L10;
        }
        if (*u <= x[i + 1]) {
                goto L30;
        }

        /*  binary search */

      L10:
        i = 1;
        j = *n + 1;
      L20:
        k = (i + j) / 2;
        if (*u < x[k]) {
                j = k;
        }
        if (*u >= x[k]) {
                i = k;
        }
        if (j > i + 1) {
                goto L20;
        }

        /*  evaluate spline */

      L30:
        dx = *u - x[i];
        *s = y[i] + dx * (b[i] + dx * (c[i] + dx * d[i]));
        *g = b[i] + dx * (2 * c[i] + 3 * dx * d[i]);
        *h = 2 * c[i] + 6 * dx * d[i];

        return 0;
}                               /* seval */
