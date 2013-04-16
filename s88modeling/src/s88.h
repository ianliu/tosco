/*  S88modeling - Seismic modeler by rays theory
 *  Copyright (C) 2009-2012 Ricardo Biloti <biloti@ime.unicamp.br>
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

#ifndef _S88_H
#define _S88_H

#include <stdlib.h>
#include <stdio.h>
#include <glib.h>

struct s88 {
        gint               nint;
        gint              *npnt;
        gdouble          **x;
        gdouble          **z;
        gint             **iii;
        gdouble           *v1;
        gdouble           *v2;
        gboolean           nro;
        gboolean           nabs;
        gdouble           *rho1;
        gdouble           *rho2;
        gdouble           *nqp;
        gdouble           *nqs;
        gdouble          **qps;
        gdouble           *ptos;
        gint               mep;
        gint               mdim;
        gint               method;
        gint               ibp;
        gint               ibs;
        gboolean           mltp;
        gboolean           sghost;
        gboolean           rghost;
        /*
        gint               iup;
        gint               ius;
        gboolean           idp;
        gboolean           ids;
        */
        gint               mreg;
        gint               itmax;
        gdouble            rmin;
        gdouble            rxmin;
        gdouble            rxstep;
        gdouble            sxmin;
        gdouble            sxstep;
        gdouble            sz;
        gint               nshots;
        gdouble            xsour;
        gdouble            zsour;
        gdouble            tsour;
        gdouble            reps;
        gdouble            reps1;
        gdouble            dtint;
        gdouble            amin1;
        gdouble            astep1;
        gdouble            amax1;
        gdouble            amin2;
        gdouble            astep2;
        gdouble            amax2;
        gdouble            ac;
        gdouble            tmin;
        gdouble            dt;
        gdouble            tmax;
        gdouble            freq;
        gdouble            gamma;
        gdouble            psi;
        gboolean           implos;
        gdouble            mag;
        gchar             *spath;
        gchar             *sypath;
        gboolean           showrays;
        gchar             *workdir;
        gboolean           verbose;
        gboolean           debug;
        gboolean           dryrun;
};


void s88_run(struct s88 *p);

#endif /* _S88_H */
