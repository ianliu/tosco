/***************************************************************************/
/*                                                                         */
/* optsp.c - an example of use of optspline                                */
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
#include "optsp.h"

int main(int argc, char **argv)
{
  double *x, *y, *X, *Y;
  double *b, *c, *d;
  double residue;
  int n, N, i;

  FILE *fp;

  struct gengetopt_args_info arg;
  
  /* Leitura dos parâmetros usando o GenGetOpt */
  if (cmdline_parser(argc, argv, &arg) != 0)
     exit(EXIT_FAILURE);
  
  n = arg.nknots_arg;

  /***************************************/
  x = (double *) malloc(n * sizeof(double));
  y = (double *) malloc(n * sizeof(double));
  
  
  if ( (fp = fopen(arg.data_arg, "r")) == NULL){
     fprintf(stderr, "Unable to open %s file.\nAborting.\n",
	     arg.data_arg);
     return EXIT_FAILURE;
  }

  if ( arg.maxitr_arg <= 0 ){
     fprintf(stderr, "maxitr parameter should be positiv.\nAborting.\n");
     return EXIT_FAILURE;
  }

  /* Descobre quantos pontos ha no arquivo de dados */
  N = 0;
  while (!feof(fp)){
     double xx, yy;
     fscanf(fp, "%lf %lf\n", &xx, &yy);
     if ( (!isnan(xx)) && (!isnan(yy)) )
	N++;
  }
  fclose(fp);

  X = (double *) malloc(N * sizeof(double));
  Y = (double *) malloc(N * sizeof(double));

  if ( (fp = fopen(arg.data_arg, "r")) == NULL){
     fprintf(stderr, "Unable to open %s file.\nAborting.\n",
	     arg.data_arg);
     return EXIT_FAILURE;
  }

  i = 0;
  while (!feof(fp)){
     double xx, yy;
     fscanf(fp, "%lf %lf\n", &xx, &yy);
     if ( (!isnan(xx)) && (!isnan(yy)) ){
	X[i]   = xx;
	Y[i++] = yy;
     }
  }

  fclose(fp);
  
  if (arg.refine_flag){
     if (!arg.knots_given){
	fprintf(stderr,"To refine an input knot file is required.\n"
		"Try again.\n");
	return EXIT_FAILURE;
     }
     else{
	if ( (fp = fopen(arg.knots_arg, "r")) == NULL){
	   fprintf(stderr, "Unable to open %s file.\nGiving up.\n",
		   arg.knots_arg);
	   return EXIT_FAILURE;

	}
	else{
	   for (i=0; i < n; i++){
	      if (fscanf(fp,"%lf %lf", &(x[i]), &(y[i])) != 2){
		 fprintf(stderr, "Corrupted knot file.\n"
			 "Giving up.\n");
		 return EXIT_FAILURE;
	      }
	   }

	   if (arg.verbose_flag)
	      fprintf(stderr, "Refining previous knots\n");

	   fclose(fp);
	}
     }
  }
  
  residue = optspline((size_t) arg.maxitr_arg, N, X, Y, NULL,
		      arg.refine_flag, n, x, y, arg.tolrel_arg, 
		      arg.verbose_flag);
  
  b = (double *) malloc(N * sizeof(double));
  c = (double *) malloc(N * sizeof(double));
  d = (double *) malloc(N * sizeof(double));

  spline(&n, x, y, b, c, d);
      
  for (i=0; i<N; i++)
     fprintf(stdout, "% 16.12e   % 16.12e\n", 
	     X[i], seval(&n, &X[i], x, y, b, c, d));

  free(b);
  free(c);
  free(d);

  if (arg.knots_given){
     if ( (fp = fopen(arg.knots_arg, "w")) == NULL){
	fprintf(stderr, "Unable to open %s file.\nGiving up.\n",
		arg.knots_arg);
     }
     else{
	for (i=0; i < n; i++)
	   fprintf(fp,"%e %e\n", x[i], y[i]);
	
	fclose(fp);
     }
  }
  
  free(X);
  free(Y);
  free(x);
  free(y);

  cmdline_parser_free(&arg);
  
  return (EXIT_SUCCESS);
}
