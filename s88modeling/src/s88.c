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

#include <stdlib.h>
#include <unistd.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "s88.h"
#include "lu.h"
#include "libagr.h"

#define STARTUP -1
#define STARTDOWN 1

#define PWAVE  1
#define SWAVE -1

#define CONVERTED -1
#define UNCONVERTED 1

void write_s88_config (FILE * fp, struct s88 *p, int sourcelayer);
void write_synt_config (FILE * fp, struct s88 *p);
void synt2bin (struct s88 *p);

void write_double_vector (FILE * fp, gdouble * vec, gint N, gchar * format, gint auxin, gint max);
void write_int_vector (FILE * fp, gint * vec, gint N, gchar * format, gint auxin, gint max);
GString *make_unique_filename (const gchar * template);
void writecode (FILE * fp, gint updown, gint mult, gint srclayer, gint rlayer, gint ps,
                gint convert);
gchar *gebr_iso_date(void);

void s88_run (struct s88 *p)
{
        FILE *fp;
        GString *cmd1;
        GString *cmd2;
        GString *fname1;
        GString *fname2;
        GString *newname;
        lu_t lu;
        gint ishot;

        if (!p->dryrun) {
                fname1 = make_unique_filename ("XXXXXX.ray");
                fname2 = make_unique_filename ("XXXXXX.sis");
                cmd1 = g_string_new (NULL);
                cmd2 = g_string_new (NULL);
                g_string_printf (cmd1, "%s >/dev/null <<EOF\n"
                                 "1\n%s\n"
                                 "2\nseis.out\n"
                                 "7\nlu1.dat\n" "8\nlu2.dat\n" "0\n0\nEOF\n", p->spath,
                                 fname1->str);

                g_string_printf (cmd2, "%s >/dev/null <<EOF\n"
                                 "1\n%s\n"
                                 "2\nsynt.out\n"
                                 "3\nlu2.dat\n"
                                 "4\nlu3.dat\n" "0\n0\nEOF\n", p->sypath, fname2->str);

                fp = fopen (fname2->str, "w");
                write_synt_config (fp, p);
                fclose (fp);
        }

        if (p->debug) {
                fprintf (stderr, "\nSyntpl config:\n");
                write_synt_config (stderr, p);
                fprintf (stderr, "\n");
        }

        /*-------------------------------------------------------------------------*/
        /* Warming-up phase */

        /* place source at surface */
        p->zsour = 0;
        p->xsour = p->sxmin;
        p->rmin = p->xsour + p->rxmin;

        if (p->verbose)
                fprintf (stderr, "Warming up\n");
        if (p->debug) {
                fprintf (stderr, "\nSeis config:\n");
                write_s88_config (stderr, p, 1);
                fprintf (stderr, "\n");
        }

        if (!p->dryrun) {
                fp = fopen (fname1->str, "w");
                write_s88_config (fp, p, 1);
                fclose (fp);

                if (system (cmd1->str)) {
                        fprintf (stderr, "\nseis with problem\n");
                }

                lu_parse ("lu1.dat", &lu);

                if (p->keeprays) {
                        g_rename ("lu1.dat", "model.dat");
                } else {
                        g_unlink ("lu1.dat");
                }

                if (p->raydiag) {
                        gboolean norays = p->norays;
                        p->norays = TRUE;
                        InitAGR ();
                        sprintf (AGR_title, "Model");
                        agr_write ("model.agr", &lu, p, TRUE);

                        p->norays = norays;
                }
        }

        /* Export interfaces */
        if (p->interf != NULL) {
                if (p->verbose)
                        fprintf (stderr, "  Interfaces saved to.......: %s\n", p->interf);
                export_interf (&lu, p);
        }

        /* Export the velocity model */
        if (p->vel != NULL) {
                if (p->verbose)
                        fprintf (stderr, "  Velocity model saved to...: %s\n\n", p->vel);
                export_velocity (&lu, p);
        }

        /*-------------------------------------------------------------------------*/
        /* De facto modeling */
        p->zsour = p->sz;
        for (ishot = 0; ishot < p->nshots; ishot++) {
                int slayer;

                newname = g_string_new (NULL);

                p->xsour = p->sxmin + p->sxstep * ishot;
                if (p->szrel) {
                        p->zsour = interf (&(lu.interf[0]), p->xsour) + p->sz;
                }
                p->rmin = p->xsour + p->rxmin;
                slayer = which_layer (p->xsour, p->zsour, &lu);

                if (p->verbose)
                        fprintf (stderr, "Modeling for source at (%.4f, %.4f): ", p->xsour,
                                 p->zsour);
                if (p->debug) {
                        fprintf (stderr, "\nSeis config:\n");
                        write_s88_config (stderr, p, slayer);
                        fprintf (stderr, "\n");
                }

                if (!p->dryrun) {
                        fp = fopen (fname1->str, "w");
                        write_s88_config (fp, p, slayer);
                        fclose (fp);

                        if (!system (cmd1->str)) {
                                if (!system (cmd2->str)) {
                                        synt2bin (p);
                                        if (p->verbose)
                                                fprintf (stderr, "done\n");
                                } else {
                                        fprintf (stderr, "\nsyntpl with problem\n");
                                }
                        } else {
                                fprintf (stderr, "\nseis with problem\n");
                        }

                        if (p->keeprays) {
                                g_string_printf (newname, "lu1-%04i.dat", ishot + 1);
                                g_rename ("lu1.dat", newname->str);
                        }

                        if (p->raydiag && !p->norays) {
                                if (p->keeprays)
                                        lu_parse (newname->str, &lu);
                                else
                                        lu_parse ("lu1.dat", &lu);

                                g_string_printf (newname, "shot-%04i.agr", ishot + 1);

                                InitAGR ();
                                sprintf (AGR_title, "Shot %i at (%.4f, %.4f)", ishot + 1, p->xsour,
                                         p->zsour);
                                agr_write (newname->str, &lu, p, FALSE);
                        }

                }

        }
        g_string_free (newname, TRUE);

        if (!p->dryrun) {
                if (!p->debug) {
                        g_unlink (fname1->str);
                        g_unlink (fname2->str);
                        if (!p->keeprays) {
                                g_unlink ("lu1.dat");
                        }
                        g_unlink ("lu2.dat");
                        g_unlink ("lu3.dat");
                        g_unlink ("seis.out");
                        g_unlink ("synt.out");
                        g_unlink ("fort.10");
                        g_unlink ("curv.dat");
                        g_unlink ("qp.dat");
                }

                g_string_free (fname1, TRUE);
                g_string_free (fname2, TRUE);
                g_string_free (cmd1, TRUE);
                g_string_free (cmd2, TRUE);
        }

        lu_free (&lu);
}


void write_s88_config (FILE * fp, struct s88 *p, int sourcelayer)
{
        gint ii, jj, aux, imult;
        gdouble vmin, vmax, bmin, bmax, bleft, bright;

        bmin = p->z[0][0];
        bmax = p->z[0][0];
        bleft = p->x[0][0];
        bright = p->x[0][0];

        // CARD 1
        fprintf (fp, "%2i%2i%2i%68s  %2i%2i\n", 0, 2, 0, "", 0, 1);

        // CARD 2
        fprintf (fp, "%5i", p->nint);
        write_int_vector (fp, p->npnt, p->nint, "%5i", 1, 16);

        // CARD 3
        for (ii = 0; ii < p->nint; ii++) {
                aux = 0;
                for (jj = 0; jj < p->npnt[ii]; jj++) {
                        fprintf (fp, "%10.5f%10.5f%5i", p->x[ii][jj], p->z[ii][jj], p->iii[ii][jj]);

                        bmin = MIN (bmin, p->z[ii][jj]);
                        bmax = MAX (bmax, p->z[ii][jj]);
                        bleft = MIN (bleft, p->x[ii][jj]);
                        bright = MAX (bright, p->x[ii][jj]);

                        aux++;
                        if (aux == 3) {
                                fprintf (fp, "\n");
                                aux = 0;
                        }
                }
                if (aux != 0)
                        fprintf (fp, "\n");
        }

        // CARD 4 (MM=1 only)
        write_double_vector (fp, p->v1, p->nint - 1, "%10.5f", 0, 8);
        write_double_vector (fp, p->v2, p->nint - 1, "%10.5f", 0, 8);

        // CARD 5
        fprintf (fp, "%5i", (p->nro ? 1 : 0));

        vmin = p->v1[0];
        vmax = p->v2[0];

        aux = 1;
        for (ii = 0; ii < p->nint - 1; ii++) {
                fprintf (fp, "%5i", 0); /* nvs = 0: p-wave velocities */
                aux++;

                if (aux == 16) {
                        fprintf (fp, "\n");
                        aux = 0;
                }

                vmin = MIN (vmin, p->v1[ii]);
                vmin = MIN (vmin, p->v2[ii]);
                vmin = MIN (vmin, p->v1[ii]);

                vmax = MAX (vmax, p->v1[ii]);
                vmax = MAX (vmax, p->v2[ii]);

        }

        fprintf (fp, "%5i\n", (p->nabs ? 1 : 0));

        // CARD 6A
        if (p->nro) {
                aux = 0;
                for (ii = 0; ii < p->nint - 1; ii++) {
                        fprintf (fp, "%10.5f%10.5f", p->rho1[ii], p->rho2[ii]);
                        aux++;

                        if (aux == 4) {
                                fprintf (fp, "\n");
                                aux = 0;
                        }
                }
                if (aux != 0)
                        fprintf (fp, "\n");
        }
        // CARD 6B
        if (p->nabs) {
                for (ii = 0; ii < p->nint - 1; ii++) {
                        fprintf (fp,
                                 "%5i%5i%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                                 (int) p->nqp[ii], (int) p->nqs[ii],
                                 p->qps[ii][0], p->qps[ii][1],
                                 p->qps[ii][2], p->qps[ii][3], p->qps[ii][4], p->qps[ii][5]);
                }
        }
        // CARD 7
        write_double_vector (fp, p->ptos, p->nint - 1, "%10.5f", 0, 8);

        // CARD 8
        fprintf (fp, "%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                 vmin, vmax, bmin, bmax, bleft, bright);

        // CARD 9
        fprintf (fp, "%3i%3i%3i%3i%3i", 1, p->mep, 0, p->mdim, p->method);

        // No automatic generation of wavepath codes
        fprintf (fp, "%3i%3i%3i%3i%3i%3i", 0, 0, 0, 0, 0, 0);

        // Turn on manual generation of wavepath codes
        fprintf (fp, "%3i%3i%3i%3i%3i%3i", 1, 1, 0, 1, 7, 8);

        fprintf (fp, "%3i%3i%3i\n", 0, 0, 0);

        // CARD 10 (MEP > 0)
        fprintf (fp, "%10.5f%10.5f%10.5f\n", p->rmin, p->rxstep, 0.0);

        // CARD 11
        fprintf (fp, "%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                 p->xsour, p->zsour, p->tsour, p->reps, p->reps1, bleft, bright);

        // CARD 12 
        fprintf (fp, "%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                 p->dtint, p->amin1, p->astep1, p->amax1, p->amin2, p->astep2, p->amax2, p->ac);


        // CARD 13 (Wave codes)
        // FIX-ME: Se a linha for muito comprida, tem que ser cortada (24I3)

        for (imult = 0; imult <= (gint) p->mltp; imult++) {     /* Multiples */
                for (ii = sourcelayer; ii <= p->nint - 2; ii++) {       /* Each layer */

                        if (p->ibp) {   /* P Primary down unconverted */
                                //writecode(stderr, STARTDOWN, imult, sourcelayer, ii, PWAVE, UNCONVERTED);
                                writecode (fp, STARTDOWN, imult, sourcelayer, ii, PWAVE,
                                           UNCONVERTED);
                        }

                        if (p->ibp == 2) {      /* P Primary down converted */
                                //writecode(stderr, STARTDOWN, imult, sourcelayer, ii, PWAVE, CONVERTED);
                                writecode (fp, STARTDOWN, imult, sourcelayer, ii, PWAVE, CONVERTED);
                        }

                        if (p->ibp && p->sghost) {      /* P Primary up (source ghost) unconverted */
                                //writecode(stderr, STARTUP, imult, sourcelayer, ii, PWAVE, UNCONVERTED);
                                writecode (fp, STARTUP, imult, sourcelayer, ii, PWAVE, UNCONVERTED);
                        }

                        if (p->ibp == 2 && p->sghost) { /* P Primary up (source ghost) converted */
                                //writecode(stderr, STARTUP, imult, sourcelayer, ii, PWAVE, CONVERTED);
                                writecode (fp, STARTUP, imult, sourcelayer, ii, PWAVE, CONVERTED);
                        }

                        if (p->ibs) {   /* S Primary down unconverted */
                                //writecode(stderr, STARTDOWN, imult, sourcelayer, ii, SWAVE, UNCONVERTED);
                                writecode (fp, STARTDOWN, imult, sourcelayer, ii, SWAVE,
                                           UNCONVERTED);
                        }

                        if (p->ibs == 2) {      /* S Primary down converted */
                                //writecode(stderr, STARTDOWN, imult, sourcelayer, ii, SWAVE, CONVERTED);
                                writecode (fp, STARTDOWN, imult, sourcelayer, ii, SWAVE, CONVERTED);
                        }

                        if (p->ibs && p->sghost) {      /* S Primary up (source ghost) unconverted */
                                //writecode(stderr, STARTUP, imult, sourcelayer, ii, SWAVE, UNCONVERTED);
                                writecode (fp, STARTUP, imult, sourcelayer, ii, SWAVE, UNCONVERTED);
                        }

                        if (p->ibs == 2 && p->sghost) { /* S Primary up (source ghost) converted */
                                //writecode(stderr, STARTUP, imult, sourcelayer, ii, SWAVE, CONVERTED);
                                writecode (fp, STARTUP, imult, sourcelayer, ii, SWAVE, CONVERTED);
                        }
                }
        }

        fprintf (fp, "\n");

        // CARD 9
        fprintf (fp, "%3i%3i%3i%3i%3i", 0, p->mep, 0, p->mdim, p->method);

        fprintf (fp, "%3i%3i%3i%3i%3i%3i", 0, 0, 0, 0, 0, 0);

        fprintf (fp, "%3i%3i%3i%3i%3i%3i", 1, 1, 0, 1, 7, 8);

        fprintf (fp, "%3i%3i%3i\n", 0, 0, 0);

        fflush (fp);

}

void write_synt_config (FILE * fp, struct s88 *p)
{

        // CARD 1
        fprintf (fp, "%3i%3i%3i%3i%3i\n", 2, 3, 1, 7, 0);

        // CARD 2
        fprintf (fp, "%3i%3i%3i%3i%3i%3i%3i\n", 0, 0, 0, 0, 0, (p->nabs ? 1 : 0), 3);

        // CARD 3
        fprintf (fp, "%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                 p->tmin, p->dt, p->tmax, p->freq, p->gamma, p->psi, 8.0, 0.0);

        // CARD 6
        if (p->nabs) {
                fprintf (fp, "%10.5f%10.5f\n", 1.0, 1.0);
        }
        // CARD 7
        fprintf (fp, "%5i%5i%5i%5i%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                 (p->implos ? -1 : 1), 0, 0, 0, p->mag, 0.0, 0.0, 0.0, 0.0, 0.0);

        fprintf (fp, "%3i%3i%3i%3i%3i\n", 0, 0, 0, 0, 0);
        fprintf (fp, "%3i\n", 0);

        fflush (fp);
}

/************************************************************/

void write_double_vector (FILE * fp, gdouble * vec, gint N, gchar * format, gint auxin, gint max)
{
        gint ii, aux;

        aux = auxin;

        for (ii = 0; ii < N; ii++) {
                fprintf (fp, format, vec[ii]);
                aux++;

                if (aux == max) {
                        fprintf (fp, "\n");
                        aux = 0;
                }
        }

        if (aux != 0)
                fprintf (fp, "\n");
}

/************************************************************/

void write_int_vector (FILE * fp, gint * vec, gint N, gchar * format, gint auxin, gint max)
{
        gint ii, aux;

        aux = auxin;

        for (ii = 0; ii < N; ii++) {
                fprintf (fp, format, vec[ii]);
                aux++;

                if (aux == max) {
                        fprintf (fp, "\n");
                        aux = 0;
                }
        }

        if (aux != 0)
                fprintf (fp, "\n");
}

GString *make_unique_filename (const gchar * template)
{
        GString *path;

        /* assembly file path */
        path = g_string_new (NULL);
        g_string_printf (path, "%s", template);

        /* create a temporary file. */
        close (g_mkstemp (path->str));

        return path;
}


void synt2bin (struct s88 *p)
{

        FILE *fp;
        gint i, npts, ndst;
        gfloat tmin, dt, dist, rstep;
        gint bytes;
        size_t nwritten = 0;
        static gfloat *samples = NULL;

        if ((fp = fopen ("fort.10", "rb")) == NULL)
                goto out;

        if (fread (&bytes, sizeof (gint), 1, fp) < 1)
                goto out;
        if (fread (&npts, sizeof (gint), 1, fp) < 1)
                goto out;
        if (fread (&tmin, sizeof (gfloat), 1, fp) < 1)
                goto out;
        if (fread (&dt, sizeof (gfloat), 1, fp) < 1)
                goto out;
        if (fread (&ndst, sizeof (gint), 1, fp) < 1)
                goto out;
        if (fread (&dist, sizeof (gfloat), 1, fp) < 1)
                goto out;
        if (fread (&rstep, sizeof (gfloat), 1, fp) < 1)
                goto out;

        if (p->debug) {
                fprintf (stderr, "\nbytes = %i\n", bytes);
                fprintf (stderr, "npts  = %i\n", npts);
                fprintf (stderr, "tmin  = %f\n", tmin);
                fprintf (stderr, "dt    = %f\n", dt);
                fprintf (stderr, "ndst  = %i\n", ndst);
                fprintf (stderr, "dist  = %f\n", dist);
                fprintf (stderr, "rstep = %f\n", rstep);
        }

        if (fread (&bytes, sizeof (gint), 1, fp) < 1)
                goto out;

        if (samples == NULL)
                samples = (gfloat *) malloc (sizeof (float) * npts);

        for (i = 0; i < ndst; i++) {
                if (fread (&bytes, sizeof (gint), 1, fp) < 1)
                        goto out;

                if (fread (samples, sizeof (gfloat), npts, fp) < npts)
                        goto out;
                nwritten += fwrite (samples, sizeof (gfloat), npts, stdout);

                if (fread (&bytes, sizeof (gint), 1, fp) < 1)
                        goto out;
        }

        fclose (fp);

        if (p->debug) {
                fprintf (stderr, "%lu floats written\n", (long unsigned int) nwritten);
        }

        return;


      out:fprintf (stderr, "Problem with syntpl output.\nAborting.\n");
        exit (EXIT_FAILURE);

}

void writecode (FILE * fp, gint updown, gint mult, gint srclayer, gint rlayer, gint ps,
                gint convert)
{

        int ii;
        int count;
        int segments;
        int *code;
        int factor;

        factor = ps;

        /* Count segments up to reflector */
        if (updown == STARTDOWN) {
                segments = rlayer - srclayer + 1;
        } else {
                segments = srclayer + rlayer;
        }

        /* All way up */
        segments += rlayer;

        /* If is multiple */
        if (mult)
                segments += 2 * rlayer;

        code = (int *) malloc (sizeof (int) * segments);


        count = 0;
        if (updown == STARTDOWN) {

                for (ii = srclayer; ii <= rlayer; ii++)
                        code[count++] = factor * ii;
        } else {
                for (ii = srclayer; ii >= 1; ii--)
                        code[count++] = factor * ii;
                for (ii = 1; ii <= rlayer; ii++)
                        code[count++] = factor * ii;
        }

        /* First reflection */
        factor = convert * factor;

        /* All way up */
        for (ii = rlayer; ii >= 1; ii--)
                code[count++] = factor * ii;

        /* If is multiple */
        if (mult) {
                for (ii = 1; ii <= rlayer; ii++)
                        code[count++] = factor * ii;
                for (ii = rlayer; ii >= 1; ii--)
                        code[count++] = factor * ii;
        }

        count = 1;
        fprintf (fp, "%3i%3i", updown, segments);

        for (ii = 0; ii < segments; ii++) {
                fprintf (fp, "%3i", code[ii]);
                count++;
                if (count == 24) {
                        fprintf (fp, "\n");
                        count = 0;
                }
        }
        if (count > 0)
                fprintf (fp, "\n");

        free (code);

}
