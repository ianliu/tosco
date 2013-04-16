/*  S88modeling - Seismic modeler by rays theory
 *  Copyright (C) 2009-2013 Ricardo Biloti <biloti@ime.unicamp.br>
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
#include <math.h>
#include <glib.h>

#include "s88.h"
#include "parser.h"

#define check_required(xx,yy)       if (xx == NULL){fprintf(stderr, "%s required.\n", yy); return EXIT_FAILURE;}
#define convert_double(aa,bb,nn,cc) { check_required(aa,cc);            \
        if (list_to_double (aa, bb, nn) != (nn)){                       \
        fprintf (stderr, "%s should have %i elements.\n", cc, nn);\
        return EXIT_FAILURE; }}

gint list_to_int (gchar *list, gint *values, gint max);
gint list_to_double (gchar *list, gdouble *values, gint max);
gint list_size (gchar *list);
gint check_parameters (struct s88 *p, struct parse_params *pp);
gint fill_in_s88(struct s88 *p, struct parse_params *pp);

struct s88* parse_command_line(int argc, char** argv)
{

        static struct parse_params pp;
        static struct s88 p;

        static GOptionEntry entries_interface[] = {
                { "xcoord",0, 0, G_OPTION_ARG_STRING_ARRAY, &pp.xcoord, "List of x coordinates of interface knots", "x1,x2,...,xn" },
                { "zcoord",0, 0, G_OPTION_ARG_STRING_ARRAY, &pp.zcoord, "List of z coordinates of interface knots", "z1,z2,...,zn" },
                { "iii",   0, 0, G_OPTION_ARG_STRING_ARRAY, &pp.iii, "List of interface knots flags (-2,-1,0,k)", "i1,i2,...,in or \"auto\"" },
                { NULL }
        };
        
        static GOptionEntry entries_velocities[] = {
                { "v1",    0, 0, G_OPTION_ARG_STRING, &pp.v1,   "P-wave velocities below i-th interfaces", "v1,v2,...,vN" },
                { "v2",    0, 0, G_OPTION_ARG_STRING, &pp.v2,   "P-wave velocities above (i+1)-th interfaces", "v1,v2,...,vN" },
                { "ptos",  0, 0, G_OPTION_ARG_STRING, &pp.ptos, "Ratio P/S-wave velocities per layer", "1.732,...,1.732" },
                { "rho1",  0, 0, G_OPTION_ARG_STRING, &pp.rho1, "Constant factors for densities", "c1,c2,...,c(N-1)" },
                { "rho2",  0, 0, G_OPTION_ARG_STRING, &pp.rho2, "Angular factors for densities", "d1,d2,...,d(N-1)" },
                { NULL }
        };

        static GOptionEntry entries_quality_factors[] = { 
                { "nqp", 0, 0, G_OPTION_ARG_STRING_ARRAY, &pp.nqp, "0 for qp = qp1+qp2*vp+qp3*vp^2, or 1 for qp = qp1+qp2*vp+qp3/vp", "0 or 1" }, 
                { "nqs", 0, 0, G_OPTION_ARG_STRING_ARRAY, &pp.nqs, "0 for qs = qs1+qs2*vs+qs3*vs^2, or 1 for qp = qs1+qs2*vs+qs3/vs", "0 or 1" },
                { "qps",  0, 0, G_OPTION_ARG_STRING_ARRAY, &pp.qps, "Quality factors for each layer for P and S wave", "qp1,qp2,qp3,qs1,qs2,qs3" },
                { NULL }
        };

        static GOptionEntry entries_experiment[] = {
                { "mdim",  0, 0, G_OPTION_ARG_INT,     &p.mdim,  "Source type (0, 1, 2, or 3)", "3" },
                { "sxmin", 0, 0, G_OPTION_ARG_DOUBLE,  &p.sxmin, "Initial x coordinate of sources", "" },
                { "sxstep",0, 0, G_OPTION_ARG_DOUBLE,  &p.sxstep,"Step in source positions", "" },
                { "sz",    0, 0, G_OPTION_ARG_DOUBLE,  &p.sz,    "Source depth", "0.0" },
                { "nshots",0, 0, G_OPTION_ARG_INT,     &p.nshots,"Number of shots", "1"},
                { "nrec",   0, 0, G_OPTION_ARG_INT,    &p.mep,   "Number of receivers", "10" },
                { "rxmin",  0, 0, G_OPTION_ARG_DOUBLE, &p.rxmin, "Initial relative receiver position", "" },
                { "rxstep", 0, 0, G_OPTION_ARG_DOUBLE, &p.rxstep,"Step in receiver position", "" },
                { "tsour", 0, 0, G_OPTION_ARG_DOUBLE,  &p.tsour, "Initial time", "0.0" },
                { NULL }
        };

        
        static GOptionEntry entries_waves[] = {
                { "ibp",   0, 0, G_OPTION_ARG_INT,  &p.ibp,  "Primary reflected waves started as P-waves (0 = no, 1 = only PP, 2 = PP and PS)", "<0,1,2>" },
                { "ibs",   0, 0, G_OPTION_ARG_INT,  &p.ibs,  "Primary reflected waves started as S-waves (0 = no, 1 = only SS, 2 = SS and SP)", "<0,1,2>" },
                { "mltp",  0, 0, G_OPTION_ARG_NONE, &p.mltp, "Simple multiples", NULL},
                { "sghost",0, 0, G_OPTION_ARG_NONE, &p.sghost, "Ghosts at sources (sources must be buried in the first layer)", NULL},
                // { "rghost",0, 0, G_OPTION_ARG_NONE, &p.rghost, "Ghosts at receivers", NULL},
                /*
                { "iup",   0, 0, G_OPTION_ARG_INT,  &p.iup, "Primary reflected waves started as P-waves (reclection above)", "<0,1,2>" },
                { "ius",   0, 0, G_OPTION_ARG_INT,  &p.ius, "Primary reflected waves started as S-waves (reclection above)", "<0,1,2>" },
                { "idp",   0, 0, G_OPTION_ARG_NONE, &p.idp, "Direct P wave", NULL },
                { "ids",   0, 0, G_OPTION_ARG_NONE, &p.ids, "Direct S wave", NULL },
                */
                { NULL }
        };
        
        static GOptionEntry entries_config[] = {
                { "method",0, 0, G_OPTION_ARG_INT,      &p.method,  "Method selection (0, 1, 2, or 3)", "0" },
                { "itmax", 0, 0, G_OPTION_ARG_INT,      &p.itmax,   "Number of permited iterations in two-point ray tracing", "20" },
                { "reps",  0, 0, G_OPTION_ARG_DOUBLE,   &p.reps,    "Radius of the vicinity of a receveir", "5.0e-4" },
                { "reps1", 0, 0, G_OPTION_ARG_DOUBLE,   &p.reps1,   "Tolerance for boundary rays", "2.0e-5" },
                { "dtint", 0, 0, G_OPTION_ARG_DOUBLE,   &p.dtint,   "Time step in integration", "0.1" },
                { "amin1", 0, 0, G_OPTION_ARG_DOUBLE,   &p.amin1,   "Initial angle for primary reflections", "-3.1415" },
                { "astep1",0, 0, G_OPTION_ARG_DOUBLE,   &p.astep1,  "Angle step for primary reflections", "0.0010" },
                { "amax1", 0, 0, G_OPTION_ARG_DOUBLE,   &p.amax1,   "Final angle for primary reflections", "3.1415" },
                { "amin2", 0, 0, G_OPTION_ARG_DOUBLE,   &p.amin2,   "Initial angle for direct wave", "-3.1415" },
                { "astep2",0, 0, G_OPTION_ARG_DOUBLE,   &p.astep2,  "Angle step for direct wave", "0.0010" },
                { "amax2", 0, 0, G_OPTION_ARG_DOUBLE,   &p.amax2,   "Final angle for direct wave", "3.1415" },
                { "ac",    0, 0, G_OPTION_ARG_DOUBLE,   &p.ac,      "Accuracy for ray tracing integration", "1.0e-5" },
                { "spath", 0, 0, G_OPTION_ARG_FILENAME, &p.spath,   "Seis88 binary command", "seis" },
                { "sypath",0, 0, G_OPTION_ARG_FILENAME, &p.sypath,  "Syntpl binary command", "syntpl" },
                { NULL }
        };

        static GOptionEntry entries_synt[] = {
                { "tmin",  0, 0, G_OPTION_ARG_DOUBLE,   &p.tmin,    "Minimum modeled time", "0.0" }, 
                { "dt",    0, 0, G_OPTION_ARG_DOUBLE,   &p.dt,      "Sampling rate", "0.0040" },
                { "tmax",  0, 0, G_OPTION_ARG_DOUBLE,   &p.tmax,    "Maximum modeled time", "4.0" },
                { "freq",  0, 0, G_OPTION_ARG_DOUBLE,   &p.freq,    "Source frequence", "25.0" },
                { "gamma", 0, 0, G_OPTION_ARG_DOUBLE,   &p.gamma,   "Source scale parameter", "3.5" },
                { "psi",   0, 0, G_OPTION_ARG_DOUBLE,   &p.psi,     "Source phase parameter", "0.0" },
                { "implosive", 0,0,G_OPTION_ARG_NONE,   &p.implos,  "Implosive source", NULL },
                { "mag",   0, 0,G_OPTION_ARG_DOUBLE,    &p.mag,     "Source magnitude", "1.0" },
                { NULL }
        };

        static GOptionEntry entries_main[] = {
                { "showrays", 0, 0, G_OPTION_ARG_NONE,  &p.showrays,"Preserve information for ray diagram generation", NULL },
                { "workdir",0,0, G_OPTION_ARG_FILENAME, &p.workdir, "Working directory", "/tmp" },
                { "verbose", 'v',0, G_OPTION_ARG_NONE,  &p.verbose, "Verbose output", NULL },
                { "debug",   'd',0, G_OPTION_ARG_NONE,  &p.debug,   "Debug output", NULL },
                { "dry-run",0,0, G_OPTION_ARG_NONE,     &p.dryrun,  "Perform a try run only", NULL },
                { NULL }
        };

        GError         *error = NULL;
        GOptionContext *parser;
        GOptionGroup   *group;
        

        /* Set a short description for the program */
        parser = g_option_context_new ("- Ray tracing modeling through Seis88");
        
        /* Summary */
        g_option_context_set_summary (parser,
                                      "This modeling tool is designed to generate synthetic seismograms\n"
                                      "from a 2D aquisition line, through ray tracing."
                                      );
        
        /* Description */
        g_option_context_set_description (parser,
                                          "This tool models a 2D acquisition. Receivers are regularly distributed along the\n"
                                          "the surface. The source move step-by-step through the acquisition line, also\n"
                                          "regularly.\n\n"
                                          "Interfaces are represented by cubic splines. Each interface is described by its\n"
                                          "knots, provided by --xcoord, --zcoord and --iii parameters. It must be provided\n"
                                          "one group of these three parameters per interface.\n\n"
                                          "The meaning of --iii parameter is described in Seis88 documentation.\n"
                                          "Briefly, it is a list of integers (one per knot), describing how that knot\n"
                                          "should be used in spline interpolation. -1 represents a corner, 0 for smooth\n"
                                          "knot, and a positive number to collapse that knot with the corresponding one of\n"
                                          "interface below. For smooth interfaces, it is easier to set --iii=auto.\n\n"
                                          "Velocities are linearly interpolated between interfaces. Densities can be\n"
                                          "provided per layer or automatically computed from velocities.\n\n"
                                          "To specify layer densities, both --rho1 and --rho2 should be set. If so,\n"
                                          "density of i-th layer is given by: rho1(i) + rho2(i) * vp(i), where states\n"
                                          "for i-th layer P-wave velocity. Otherwise, density is given by 1.7 + 0.2*vp(i).\n\n"
                                          "Slight absortion can be considered by providing quality factors.\n"
                                          "For each layer, six coefficients (three for P and other three for S wave) must\n"
                                          "be given. The quality factor for a layer is determined by one of the two\n"
                                          "expressions:\n"
                                          "    QP = qp1 + qp2 * vp + qp3 * vp^2 (when nqp is 0)\n"
                                          "or\n"
                                          "    QP = qp1 + qp2 * vp + qp3 / vp (when nqp is 1)\n"
                                          "Analogous expressions are used to determine QS (quality factor for S wave).\n"
                                          "Note that for each layer, all three parameters nqp, nqs and qps must be provided.\n"
                                          "Futhermore this set of three paramters must be provided for all layers of\n"
                                          "the model. It is not allowed to specify quality factors only for some layers.\n\n"
                                          "Acceptable types of source are: 0 (only rays and traveltime, but no amplitudes,\n"
                                          "are computed), 1 (geometrical spreading is not taking into account), 2 (line\n"
                                          "source) and 3 (point source).\n\n" 
                                          "Primaries for PP and PS waves are able to be computed. Also some simple\n"
                                          "multiple are eligible. By simple multiples, we mean a wave that propagates directly\n"
                                          "from the surface to the reflector, back straight to the surface, once more down to\n"
                                          "the reflector, and then up to surface, where it will be recorded.\n\n"
                                          "The wavelet is given by:\n\n"
                                          "     f(t) = exp { -(omega * t / gamma)^2 } * cos {omega * t + psi},\n\n"
                                          "where omega = 2 * PI * freq.\n\n"
                                          "The output produced is a binary data, with nshots * nrec traces, with\n"
                                          "ns = (tmax - tmin)/dt samples per trace, printed to stdout.\n\n"
                                          "All temporary files are stored in the directory specified by --workdir\n"
                                          "parameter. Ray files (lu1-????.dat) are stored in that directory as well,\n"
                                          "if --showrays flag is on.\n\n"
                                          "s88modeling relies on Seis88, a robust software developed by\n"
                                          "Vlastislav Cerveny, from Institute of Geophysics - Charles University, and\n"
                                          "Ivan Psencik, from Geophysical Institute - Czechosl. Acad. Sci.\n\n"
                                          "Only a small subset of Seis88 features is available through s88modeling.\n\n"
                                          "S88modeling is hosted by Tools for Scientific Computing (ToSCo) Project.\n"
                                          "Visit its site (http://codes.google.com/p/tosco) to obtain the latest version.\n"
                                          "Also from ToSCo project, there is a program, called Rays, which is able to\n"
                                          "parse lu1-????.dat files produced here and depicts ray diagrams.\n\n"
                                          "Copyright (c) 2009-2010 Ricardo Biloti <biloti@ime.unicamp.br>\n"
                                          "Department of Applied Mathematics, IMECC, UNICAMP -- Brazil"
                                          );
        
        group = g_option_group_new("geom", "Knots for cubic spline representation of interfaces:\n(one instance per interface)", "Show model geometry options ", NULL, NULL);
        g_option_group_add_entries(group, entries_interface);
        g_option_context_add_group(parser, group);

        group = g_option_group_new("vel", "Velocities and densities:", "Show velocities and densities options", NULL, NULL);
        g_option_group_add_entries(group, entries_velocities);
        g_option_context_add_group(parser, group);

        group = g_option_group_new("qp", "Quality factors (for each layer):", "Show quality factor options", NULL, NULL);
        g_option_group_add_entries(group, entries_quality_factors);
        g_option_context_add_group(parser, group);

        group = g_option_group_new("exp", "Experiment setup:", "Show experiment setup options", NULL, NULL);
        g_option_group_add_entries(group, entries_experiment);
        g_option_context_add_group(parser, group);

        group = g_option_group_new("wave", "Wave selection:", "Show wave selection options", NULL, NULL);
        g_option_group_add_entries(group, entries_waves);
        g_option_context_add_group(parser, group);

        group = g_option_group_new("synt", "Synthetic seismogram:", "Show synthetic seismogram options", NULL, NULL);
        g_option_group_add_entries(group, entries_synt);
        g_option_context_add_group(parser, group);

        group = g_option_group_new("config", "Detailed Seis88 setup:", "Show Seis88 setup options", NULL, NULL);
        g_option_group_add_entries(group, entries_config);
        g_option_context_add_group(parser, group);

        group = g_option_group_new(NULL, NULL, NULL, NULL, NULL);
        g_option_group_add_entries(group, entries_main);
        g_option_context_set_main_group(parser, group);

        //g_option_context_add_main_entries (parser, entries, NULL);
        
        /* Complain about unknown options */
        g_option_context_set_ignore_unknown_options (parser, FALSE);
        
        /* Required */
        p.rxmin  = 0.0 / 0.0;
        p.rxstep = 0.0 / 0.0;
        p.sxmin  = 0.0 / 0.0;
        p.sxstep = 0.0 / 0.0;
        p.sz     = 0.0 / 0.0;
        p.nshots = 1;

        /* Default values */
        p.mdim = 3;
        p.mep = 10;
        p.method = 0;
        p.itmax = 20;
        p.tsour = 0;
        p.reps =  0.0005;
        p.reps1 = 0.00002;
        p.dtint = 0.1;
        p.amin1 = -3.1415;
        p.astep1 = 1.0e-3;
        p.amax1 = M_PI;
        p.amin2 = -3.1415;
        p.astep2 = .001;
        p.amax2 = M_PI;
        p.ac = 1.0e-5;
        p.ibp = 1;
        p.ibs = 0;
        p.mltp = FALSE;
        p.sghost = FALSE;
        p.rghost = FALSE;

        p.tmin = 0;
        p.dt = 0.004;
        p.tmax = 4.0;
        p.freq = 25;
        p.gamma = 3.5;
        p.psi = 0;
        p.mag = 1;
        p.showrays = FALSE;
        p.verbose = FALSE;
        p.debug = FALSE;
        p.dryrun = FALSE;
        
        /* Parse command line */
        if (g_option_context_parse (parser, &argc, &argv, &error) == FALSE){
                fprintf(stderr, "%s: syntax error\n", argv[0]);
                fprintf(stderr, "Try %s --help\n", argv[0]);
                return NULL;
        }
        
        g_option_context_free (parser);
        
        /* Check for required parameters */
        if (check_parameters(&p, &pp))
                return NULL;

        /* Filling in vectors */
        if (fill_in_s88(&p, &pp))
                return NULL;

        return &p;
}

/* ======== Private functions ======== */
gint list_to_int (gchar *list, gint *values, gint max)
{
        gint n = 0;
        gchar *strvalue;

        strvalue = strtok(list, ",");
        values[n++] = atoi(strvalue);
        
        while ((strvalue = strtok(NULL, ",")) != NULL && n < max)
                values[n++] = atoi(strvalue);

        return (strvalue == NULL ? n : -1);
}

gint list_to_double (gchar *list, gdouble *values, gint max)
{
        gint n = 0;
        gchar *strvalue;

        strvalue = strtok(list, ",");
        values[n++] = atof(strvalue);
        
        while ((strvalue = strtok(NULL, ",")) != NULL && n < max)
                values[n++] = atof(strvalue);

        return (strvalue == NULL ? n : -1);

}

gint list_size (gchar *list)
{
        gint n;
        gchar *aux;
        gchar *iter;

        if (list == NULL)
                return 0;
        
        aux = (gchar *) malloc (sizeof(char) * (strlen(list)+1));
        memcpy(aux, list, (strlen(list)+1));
        
        iter = strtok(aux, ",");
        n = 1;

        while ((iter = strtok(NULL, ",")) != NULL)
                n++;

        free(aux);

        return n;
}


gint check_parameters (struct s88 *p, struct parse_params *pp)
{
        
        static gchar *seis  = "seis";
        static gchar *synt = "syntpl";
        static gchar *tmp = "/tmp";

        if (p->spath == NULL)
                p->spath = seis;

        if (p->sypath == NULL)
                p->sypath = synt;

        if (p->workdir == NULL)
                p->workdir = tmp;

        if (pp->xcoord == NULL){
                fprintf (stderr, "xcoord should be provided.\n");
                return 1;
        }

        if (pp->zcoord == NULL){
                fprintf (stderr, "zcoord should be provided.\n");
                return 1;
        }

        if (pp->v1 == NULL){
                fprintf (stderr, "v1 should be provided.\n");
                return 1;
        }

        if (pp->v2 == NULL){
                fprintf (stderr, "v2 should be provided.\n");
                return 1;
        }

        if ((pp->rho1 != NULL) && (pp->rho2 == NULL)){
                fprintf (stderr, "rho2 should be provided, whenever rho1 is provided.\n");
                return 1;
        }

        if ((pp->rho1 == NULL) && (pp->rho2 != NULL)){
                fprintf (stderr, "rho1 should be provided, whenever rho2 is provided.\n");
                return 1;
        }
        
        if (p->mep < 2){
                fprintf (stderr, "mep should be provided and greater than 1.\n");
                return 1;
        }

        if ((p->method < 0) || (p->method>4)){
                fprintf (stderr, "method should be 0, 1, 2 or 3.\n");
                return 1;
        }

        if (p->itmax < 0){
                fprintf (stderr, "itmax should be greater than or equal to 0.\n");
                return 1;
        }

        if (isnan(p->rxmin)){
                fprintf (stderr, "rxmin should be provided.\n");
                return 1;
        }

        if (isnan(p->rxstep)){
                fprintf (stderr, "rxstep should be provided.\n");
                return 1;
        }

        if (isnan(p->sxmin)){
                fprintf (stderr, "sxmin should be provided.\n");
                return 1;
        }

        if (isnan(p->sxstep)){
                fprintf (stderr, "sxstep should be provided.\n");
                return 1;                
        }

        if (isnan(p->sz)){
                fprintf (stderr, "sz should be provided.\n");
                return 1;
        }


        if (!( p->ibp || p->ibs )){
                fprintf (stderr, "No wave code selected for generation.\n");
                return 1;
        }

        if ( (p->ibp < 0) || (p->ibp >2) ){
                fprintf (stderr, "ibp out of range.\n");
                return 1;
        }

        if ( (p->ibs < 0) || (p->ibs >2) ){
                fprintf (stderr, "ibs out of range.\n");
                return 1;
        }
        
        if (p->debug)
                p->verbose = TRUE;

        return 0;
}

gint fill_in_s88(struct s88 *p, struct parse_params *pp)
{

        gint ii;

        /* Figure out how many interfaces are provided */
        p->nint = 0;
        while (pp->xcoord[p->nint] != NULL)
                p->nint++;

        p->npnt = (gint *) malloc (sizeof (gint) * p->nint);
        p->x = (gdouble **) malloc (sizeof (gdouble*) * p->nint);
        p->z = (gdouble **) malloc (sizeof (gdouble*) * p->nint);
        p->iii = (gint **)  malloc (sizeof (gint*)    * p->nint);
        for (ii=0; ii<p->nint; ii++){

                p->npnt[ii] = list_size(pp->xcoord[ii]);

                if (pp->xcoord[ii] == NULL){
                        fprintf(stderr, "Missing xcoord for interface %i.\n", ii+1);
                        return 1;
                }
                p->x[ii] = (gdouble *) malloc (sizeof (gdouble) * p->npnt[ii]);
                if (list_to_double(pp->xcoord[ii], p->x[ii], p->npnt[ii])!= p->npnt[ii]){
                        fprintf(stderr, "xcoord should have %i elements for interface %i.\n", p->npnt[ii], ii+1);
                        return 1;
                }

                if (pp->zcoord[ii] == NULL){
                        fprintf(stderr, "Missing zcoord for interface %i.\n", ii+1);
                        return 1;
                }
                p->z[ii] = (gdouble *) malloc (sizeof (gdouble) * p->npnt[ii]);
                if (list_to_double(pp->zcoord[ii], p->z[ii], p->npnt[ii])!= p->npnt[ii]){
                        fprintf(stderr, "zcoord should have %i elements for interface %i.\n", p->npnt[ii], ii+1);
                        return 1;
                }

                if (pp->iii[ii] == NULL){
                        fprintf(stderr, "Missing iii for interface %i.\n", ii+1);
                        return 1;
                }                
                p->iii[ii] = (gint *)  malloc (sizeof (gint)    * p->npnt[ii]);
                if (strcmp(pp->iii[ii], "auto") == 0){
                        gint jj;
                        p->iii[ii][0] = -1;
                        for (jj=1; jj<p->npnt[ii]-1; jj++)
                                p->iii[ii][jj] = 0;
                        p->iii[ii][p->npnt[ii]-1] = -1;
                }
                else{
                        if (list_to_int (pp->iii[ii], p->iii[ii], p->npnt[ii])!= p->npnt[ii]){
                                fprintf (stderr, "iii should have %i elements for interface %i or be set to \"auto\".\n", p->npnt[ii], ii+1);
                                return 1;
                        }
                }
        }

        p->v1 = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));
        p->v2 = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));
        convert_double (pp->v1, p->v1, (p->nint-1), "v1");
        convert_double (pp->v2, p->v2, (p->nint-1), "v2");

        if ((pp->rho1 != NULL) && (pp->rho2 != NULL)){
                p->nro = TRUE;
                p->rho1 = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));
                p->rho2 = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));

                convert_double (pp->rho1, p->rho1, p->nint-1, "rho1");
                convert_double (pp->rho2, p->rho2, p->nint-1, "rho2");
        }
        else{
                p->nro = FALSE;
        }
        
        if (pp->qps != NULL){
                p->nabs = TRUE;

                p->nqp = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));
                p->nqs = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));
                p->qps = (gdouble **) malloc (sizeof (gdouble*) * (p->nint-1));

                for (ii=0; ii < p->nint-1; ii++){

                        p->qps[ii]  = (gdouble *) malloc (sizeof (gdouble*) * 6);

                        convert_double(pp->nqp[ii], &(p->nqp[ii]), 1, "nqp");
                        convert_double(pp->nqs[ii], &(p->nqs[ii]), 1, "nqs");
                        convert_double(pp->qps[ii], p->qps[ii], 6, "qps");
                }
        }
        else{
                p->nabs = FALSE;
        }


        p->ptos = (gdouble *) malloc (sizeof (gdouble) * (p->nint-1));
        if (pp->ptos == NULL){
                gint jj;
                for (jj=0; jj<p->nint-1; jj++)
                                p->ptos[jj] = 1.732;
        }
        else{
                convert_double (pp->ptos, p->ptos, p->nint-1, "ptos");
        }

        return 0;

}
