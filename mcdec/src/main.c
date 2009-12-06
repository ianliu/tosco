/*  mcdec - Direct multichannel predictive deconvolution
 *  Copyright (c) 2009 Milton J. Porsani <porsani@cpgg.ufba.br>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "header.h"

extern mc_dec(int, int, int, int, int, float*, float*);

int main(int argc, char **argv){

   /* Variables to hold command-line parameters */
   /* and their default values                  */

   static gint    ntr     = 0;
   static gint    ncf     = 0;
   static gint    L       = 1;
   static gint    nc      = 1;
   static gint    ns      = 1;
   static gint    jtarget = 1;
   static gchar*  pfile;
   static gdouble perc    = 0.2;
   static gdouble dt      = 0.004;

   static float *x;
   static float *xdec;
   static int *p;

   /* Command-line parameters definition */
   static GOptionEntry entries[] = 
      {
	 { "ntr",     0, 0, G_OPTION_ARG_INT,      &ntr,     "number of traces",                               "N"    },
	 { "nc",      0, 0, G_OPTION_ARG_INT,      &nc,      "number of channels",                             "N"    },
	 { "jtarget", 0, 0, G_OPTION_ARG_INT,      &jtarget, "number of the selected trace to be deconvolved", "1"    },
	 { "pfile",   0, 0, G_OPTION_ARG_FILENAME, &pfile,   "multiple period file",                           "file" },
	 { "perc",    0, 0, G_OPTION_ARG_DOUBLE,   &perc,    "percentage of the period",                       "0.2"  },
	 { NULL }
      };

  GError *error = NULL;
  GOptionContext *context;

  /* Local variables */
  gint jwend; /* actual end of the window, where the newest trace should be put */
  gint jtarg; /* target trace index */
  gint jout;  /* header of the output trace */
  gint j;    
  gint wsize;
  FILE *fp;
  su_header_t *hdr;
  double lperc;

  /* Set a short description for the program */
  context = g_option_context_new ("- Direct multichannel predictive deconvolution");

  /* Summary */
  g_option_context_set_summary (context,
        "A group of nc traces (or nc channels) into the matrix x\n"
        "is used to deconvolve one trace (trace jtarget)\n"
        "Method: direct multichannel predictive deconvolution."
				);
  /* Description */
  g_option_context_set_description (context,
        "Copyright (c) 2007 by the Society of Exploration Geophysicists.\n"
        "For more information, go to http://software.seg.org/2007/0003 .\n"
        "You must read and accept usage terms at:\n"
        "http://software.seg.org/disclaimer.txt before use.\n"
        "Author: Milton J. Porsani, porsani@cpgg.ufba.br Salvador, Brazil /10/2006"
				    );

  g_option_context_add_main_entries (context, entries, NULL);

  /* Complain about unknown options */
  g_option_context_set_ignore_unknown_options (context, FALSE);

  /* Parse command line */
  if (g_option_context_parse (context, &argc, &argv, &error) == FALSE){
     fprintf(stderr, "%s: syntax error\n", argv[0]);
     fprintf(stderr, "Try %s --help\n", argv[0]);
     return EXIT_FAILURE;
  }

  g_option_context_free (context);

  /*-------------------------------------------------------------*/
  /* Main code */

  /* Memory allocation to store nc headers */
  hdr = (su_header_t*) malloc(sizeof(su_header_t) * nc);

  /* half window size */
  wsize = (nc - 1)/2;

  /* Read just the first header */
  fread(&hdr[0], SIZEOF_SEGYHDR, 1, stdin);
  ns = hdr[0].ns;
  dt = hdr[0].dt / 1.0e6;
  
  /* Minimal memory allocation */
  x = (float *) malloc(sizeof(float) * nc * ns);
  xdec = (float *) malloc(sizeof(float) * ns);
  
  /* Complete the reading of the first trace */
  fread(x, sizeof(float), ns, stdin);
  
  /* Read following (nc-1) traces */
  for (j=1; j < nc; j++){
     fread(&hdr[j],  SIZEOF_SEGYHDR, 1, stdin);
     fread(&x[j*ns], sizeof(float), ns, stdin);
  }

  /* Period vector read */
  p = (int *) malloc(sizeof(int) * ntr);
  fp = fopen(pfile, "r");
  for (j=0; j<ntr; j++){
     double paux;
     fscanf(fp, "%lf", &paux);
     p[j] = (int) (paux / dt + 0.5);
  }
  fclose(fp);

  jwend = 0;
  lperc = (1 - perc/2);

  jtarg = 1;
  /* First traces */
  for (j=0; j < jtarget-1; j++){

     ncf = perc * p[j];
     L =  lperc * p[j];

     mc_dec_(&ncf, &L, &nc, &ns, &jtarg, x, xdec);

     fwrite (&hdr[j], SIZEOF_SEGYHDR, 1, stdout);
     fwrite (xdec,    sizeof(float), ns, stdout);

     //fprintf (stderr, "*end = %3i, targ = %3i, jout = %3i\n", jwend, jtarg, j);

     jtarg++;
  }

  jout = jtarget-1;
  /* Middle traces */
  for (j=jtarget-1; j < ntr - (nc-jtarget); j++){
     
     /* Overwrite the oldiest header and data trace */ 
     fread (&hdr[jwend],   SIZEOF_SEGYHDR, 1, stdin);
     fread (&x [jwend*ns], sizeof(float), ns, stdin);

     ncf = perc * p[j];
     L =  lperc * p[j];

     mc_dec_(&ncf, &L, &nc, &ns, &jtarg, x, xdec);

     /* Write the header and data trace */
     fwrite (&hdr[jout], SIZEOF_SEGYHDR, 1, stdout);
     fwrite (xdec,       sizeof(float), ns, stdout);

     //fprintf (stderr, " end = %3i, targ = %3i, jout = %3i\n", jwend, jtarg, jout);

     jtarg = (div(++jtarg - 1, nc)).rem + 1;
     jwend = (div(++jwend, nc)).rem;
     jout  = (div(++jout, nc)).rem;
  }

  /* End traces */
  for (j=ntr - (nc-jtarget); j < ntr; j++){

     ncf = perc * p[j];
     L =  lperc * p[j];

     mc_dec_(&ncf, &L, &nc, &ns, &jtarg, x, xdec);

     fwrite (&hdr[jout], SIZEOF_SEGYHDR, 1, stdout);
     fwrite (xdec,       sizeof(float), ns, stdout);

     //fprintf (stderr, "*end = %3i, targ = %3i, jout = %3i\n", jwend, jtarg, jout);
     jtarg = (div(++jtarg - 1, nc)).rem + 1;
     jout  = (div(++jout, nc)).rem;
  }

  /* Memory release */
  free(x);
  free(xdec);
  free(hdr);

  return EXIT_SUCCESS;
}
