/*  rays2 - Draw seismic rays using grace
 *  Copyright (C) 2014 Ricardo Biloti <biloti@ime.unicamp.br>
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
#include "libagr.h"
#include "spline.h"
#include "s88.h"
#include "lu.h"

/* ---------------------------------------------------- */

void ray_read (FILE *fp, int n, int ind, ray_t *ray);
void ray_free (ray_t *ray);
void interface_alloc (interface_t *interf, int npts);
void interface_free (interface_t *interf);
void interface_read (FILE *fp, interface_t *interf);
void interface_dummy_read (FILE *fp, int nn);
void SetAGRMyPalette(void);
void strip (FILE         *agrfp,
            float         xmin,
            float         xmax, 
            interface_t * s1,
            interface_t * s2,
            unsigned int  cor);

/* ---------------------------------------------------- */

size_t line = 0;

/* ---------------------------------------------------- */
int lu_parse(gchar *lufilename, lu_t *lu){

        FILE          *lufp;
        int            i;
        int            icont;
        size_t         line;


        lufp = fopen(lufilename, "r");
        if (lufp == NULL){
                fprintf(stderr, "Unable to open %s\n", lufilename);
                return EXIT_FAILURE;
        }

        lu = (lu_t *) malloc(sizeof(lu_t));

        lu->nrays =   0;
        lu->nwavecode = 0;
        lu->nint = 0;

        line = 0;

        /** BLOCK 1

            ICONT = 0 indicates termination of the file.
            ICONT = 1 indicates that further rays follows.

        */        
        line++;
        if (fscanf(lufp, "%3i%*3i", &icont) != 1){
                fprintf(stderr,"Block 1 corrupted in input file.\n"
                        "Expecting 2 integers on line %i\n"
                        "Aborting.\n", (int) line);
                return EXIT_FAILURE;
        }
        

        /*--------------------------------------------------------------*
         *  lu1.dat parse
         *--------------------------------------------------------------*/
        while (icont>0){
                int n, ind, namp, status;
                
                /** BLOCK 2
                    Number of interfaces 
                */
                line++;
                if (fscanf(lufp,"%d", &lu->nint) != 1){
                        fprintf(stderr,"Block 2 corrupted in input file.\n"
                                "Expecting at least one integer on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
      
                /** BLOCK 3
                    
                    Interfaces
                 */
                for (i=0; i<lu->nint; i++){
                        int npts;
	
                        /* Number of points for the interface i */
                        if (fscanf(lufp,"%i", &npts) != 1){
                                fprintf(stderr,"Block 3 corrupted in input file.\n"
                                        "Expecting %i integers on line %i.\n"
                                        "Aborting.\n", lu->nint+1, (int) line);
                                return EXIT_FAILURE;
                        }

                        if (lu->nwavecode == 0){
                                interface_alloc(&(lu->interf[i]), npts);
                        }
                }

                /* Read interfaces only once */
                if (lu->nwavecode == 0){
                        for (i=0; i<lu->nint; i++){
                                interface_read(lufp, &(lu->interf[i]));
                        }
                }
                else{
                        for (i=0; i<lu->nint; i++){
                                interface_dummy_read(lufp, lu->interf[i].n);
                        }
                }

                /** BLOCK 4

                  x and z source coordinates
                  density, p- and s-wave velocities ate source
                */
                line++;
                if (fscanf(lufp,"%*f %*f %*f %*f %*f") != 0){
                        fprintf(stderr,"Block 4 corrupted in input file.\n"
                                "Expecting five floats on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }	    

                /** BLOCK 5

                  N is the number of points along the ray
                  int is a termination code
                */
                line++;
                if (fscanf(lufp,"%5i%5i", &n, &ind) != 2){
                        fprintf(stderr,"Block 5 corrupted in input file.\n"
                                "Expecting two floats on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
      
                lu->ns[lu->nwavecode] = 0;
                while (n>0){
                        lu->nrays++;
                        lu->ns[lu->nwavecode]++;
                        ray_read(lufp, n, ind, &(lu->ray[lu->nrays]));
	 
                        line++;
                        if (fscanf(lufp,"%5i%5i",&n,&ind) != 2){
                                fprintf(stderr,"Block 6 corrupted in input file.\n"
                                        "Expecting two integers on line %i.\n"
                                        "Aborting.\n", (int) line);
                                return EXIT_FAILURE;
                        }

                }

                /** BLOCK 7

                    Number of just read rays for this code wave 
                */
                line++;
                if (fscanf(lufp,"%i", &namp) != 1){
                        fprintf(stderr,"Block 7 corrupted in input file.\n"
                                "Expecting one integer on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
                namp = abs(namp);
     
                fprintf(stderr,"%3i rays corresponding to wave code %2i (%5i)\n",
                        lu->ns[lu->nwavecode], lu->nwavecode+1, lu->nrays);

                /** BLOCK 8

                    Other kinematic and dynamic quantities associated to each ray 
                */
                for (i=0;i<namp;i++){
                        float traveltime;
                        line++;
                        if (fscanf(lufp,"%*i %*f %f %*f %*f %*f %*f %*f %*f %*f %*f",
                                   &traveltime) != 1){
                                fprintf(stderr,"Block 8 corrupted in input file.\n"
                                        "Expecting one integer and ten floats on line %i.\n"
                                        "Aborting.\n", (int) line);
                                return EXIT_FAILURE;
                        }
                        lu->tmax = max(lu->tmax, traveltime);
                }
                                
                /** BLOCK 1
                 */
                line++;
                status = fscanf(lufp,"%i%*3i", &icont);
                if ( status != 1 && status != EOF){
                        fprintf(stderr,"Block 1 corrupted in input file.\n"
                                "Expecting two integers on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
                
                if (status == EOF)
                        icont = 0;

                lu->nwavecode++;
        }
        fclose(lufp);

        return EXIT_SUCCESS;
}


void lufree(lu_t *lu){
        
        int i;
        
        for (i=0; i<lu->nrays; i++){
                ray_free(&(lu->ray[i]));
        }

        for (i=0; i<lu->nint; i++){
                interface_free(&(lu->interf[i]));
        }
        
        free(lu);
}

/*-----------------------------------------------------------------------------*/
int agr_write(gchar *agrfilename, lu_t *lu, struct s88 *p){

        int          i;
        FILE         *agrfp;

        const int cor[] = {   1, 36,  9, 41, 28,  8, 14,  5,  4,
                              31,  6, 29,  7,  9, 10, 11, 12, 13,
                              16, 17, 18, 19, 20, 21, 22, 23, 24,
                              25, 26, 27, 28, 32, 33, 34, 35,  0};

        /*--------------------------------------------------------------*
         *                Writing the Gace Project File
         *--------------------------------------------------------------*/
   
        agrfp = fopen(agrfilename, "w");
        if (agrfp == NULL){
                fprintf(stderr, "Unable to open %s for writing\n", agrfilename);
                return EXIT_FAILURE;
        }
                
        InitAGR();
  
        if (p->palette != NULL)
                ImportAGRPalette(p->palette);
        else
                SetAGRMyPalette();

        WriteAGRHeader(agrfp);

        sprintf(AGR_yaxesinvert, "on");
        sprintf(AGR_title, "S88Modeling");
        sprintf(AGR_subtitle, "(Seis88)");
        sprintf(AGR_xaxislabel, "Distance [km]");
        sprintf(AGR_yaxislabel, "Depth [km]");
        AGR_xaxislabelcharsize = 1.4;
        AGR_yaxislabelcharsize = 1.4;

        WriteAGRGraph(agrfp, p->xmin, p->xmax, p->zmin, p->zmax);

        /**** Writing Layers ****/
        AGR_filltype = (1-p->nofill);
        AGR_linecolor = 1; /* Interfaces in black */

        for (i=0; i<lu->nint-1; i++)
                strip(agrfp, p->xmin, p->xmax, &(lu->interf[i]), &(lu->interf[i+1]), i+2+p->land);

        if (!p->norays){
                /**** Writing Rays ****/
                int naux, ni, nr;

                naux = 0;
                AGR_filltype = 0;
                for (ni=lu->nwavecode-1; ni>=0; ni--){

                        AGR_linecolor = (p->allblack ? 1 : cor[ni]);

                        for (nr=0; nr<lu->ns[ni]; nr++, naux++){
	    
                                WriteAGRDataXY(agrfp,
                                               lu->ray[lu->nrays-naux].x,
                                               lu->ray[lu->nrays-naux].z,
                                               lu->ray[lu->nrays-naux].n);
                        }
                }
        }

        /* Export the interfaces */
        if (p->interf != NULL){
          FILE *fp;
          int ii;

          fp = fopen(p->interf, "w");
          
          for (ii=0; ii <lu->nint; ii++){
            int ix;
            float x;

            for (ix = 0; ix < p->nx; ix++){

              x = p->xmin + ix * (p->xmax - p->xmin) / (p->nx-1);

              fprintf(fp, "% .4f %.4f\n", x, 
                      seval((int*) &(lu->interf[ii].n), &x,
                            lu->interf[ii].x, lu->interf[ii].z,
                            lu->interf[ii].b, lu->interf[ii].c, lu->interf[ii].d));
            }
            fprintf(fp, "\n");
          }
          fclose(fp);
        }

        /* Export the velocity model */
        if (p->vel != NULL){
          float *v;
          int ix, iz;
          FILE *fp;
          int ilayer;

          fp = fopen(p->vel, "w");
          if (fp == NULL){
                  fprintf(stderr, "Unavble to open %s for writing\n", p->vel);
                  return EXIT_FAILURE;
          }

          v = (float *) malloc(sizeof(float)*p->nz);

          for (ix = 0; ix < p->nx; ix++){
            float x;
            double depth[MAX_NINTERF];

            x = p->xmin + ix * (p->xmax - p->xmin) / (p->nx-1);

            for (ilayer=0; ilayer <lu->nint; ilayer++){
              depth[ilayer] = seval((int*) &(lu->interf[ilayer].n), &x,
                                    lu->interf[ilayer].x, lu->interf[ilayer].z,
                                    lu->interf[ilayer].b, lu->interf[ilayer].c, lu->interf[ilayer].d);
            }

            ilayer = 0;
            for (iz=0; iz<p->nz; iz++){
              double z;
              z = p->zmin+ iz * (p->zmax - p->zmin) / (p->nz-1);
              
              while ((z >= depth[ilayer]) && (ilayer < lu->nint)){
                ilayer++;
              }

              if (ilayer < lu->nint){
                v[iz] = p->v1[ilayer-1] + (z - depth[ilayer-1])/(depth[ilayer]-depth[ilayer-1])*(p->v2[ilayer-1]-p->v1[ilayer-1]);
              }else{
                v[iz] = p->v2[lu->nint -2];
              }
            }
            
            fwrite(v, sizeof(float), p->nz, fp);
                     
          }
          free(v);
          fclose(fp);
          fprintf(stderr, "Velocity model was exported\n");
        }

        fclose(agrfp);

        FreeAGRPalette();

        return EXIT_SUCCESS;
}

/*--------------------------------------------------------------*/
/*--------------------------------------------------------------*/

void
ray_read(FILE *fp, int n, int ind, ray_t *ray)
{
        int i;

        ray->n   = n;
        ray->ind = ind;
        ray->x   = malloc(sizeof(float) * n);
        ray->z   = malloc(sizeof(float) * n);

        for (i=0; i<n; i++){
                line++;
                if (fscanf(fp, "%f %f", &(ray->x[i]), &(ray->z[i])) != 2){
                        fprintf(stderr,"Block 6 corrupted in input file.\n"
                                "Expecting two floats on line %i\n."
                                "Aborting.\n", (int) line);
                        exit(EXIT_FAILURE);
                }
        }
}

/*--------------------------------------------------------------*/
void
ray_free(ray_t *ray)
{
        free(ray->x);
        free(ray->z);
}

/*--------------------------------------------------------------*/
void
interface_alloc(interface_t *interf, int npts)
{

        interf->n = npts -1;
        interf->x = (float *) malloc( sizeof(float) * interf->n);
        interf->z = (float *) malloc( sizeof(float) * interf->n);
        interf->b = (float *) malloc( sizeof(float) * interf->n);
        interf->c = (float *) malloc( sizeof(float) * interf->n);
        interf->d = (float *) malloc( sizeof(float) * interf->n);
        interf->iii = (int *) malloc( sizeof(int) * interf->n);
}

/*--------------------------------------------------------------*/
void
interface_free(interface_t *interf)
{
        interf->n = 0;
        free(interf->x);
        free(interf->z);
        free(interf->b);
        free(interf->c);
        free(interf->d);
        free(interf->iii);
}

/*--------------------------------------------------------------*/
void
interface_read(FILE *fp, interface_t *interf)
{
        int j;

        for (j=0; j<interf->n; j++){
                line++;
                if (fscanf(fp,"%f %f %f %f %f %i",
                           &(interf->z[j]), &(interf->b[j]),
                           &(interf->c[j]), &(interf->d[j]),
                           &(interf->x[j]), &(interf->iii[j])) != 6){
                        fprintf(stderr,"Block 3 corrupted in input file.\n"
                                "Expecting six integers on line %i.\n"
                                "Aborting.\n", (int) line);
                        exit(EXIT_FAILURE);
                }
        }
}

/*--------------------------------------------------------------*/
void
interface_dummy_read(FILE *fp, int nn)
{
        int j;

        for (j=0; j<nn; j++){
                line++;
                if(fscanf(fp,"%*f %*f %*f %*f %*f %*i")){
                        fprintf(stderr,"Block 3 corrupted in input file.\n"
                                "Expecting six integers on line %i.\n"
                                "Aborting.\n", (int) line);
                        exit(EXIT_FAILURE);
                }
        }
}

/*--------------------------------------------------------------*/
void
strip   (FILE        *agrfp,
         float        xmin,
         float        xmax, 
         interface_t *s1,
         interface_t *s2,
         unsigned int cor)
{
        int n, i;
        float *x, *z, lambda, xx;
        
        n = 2*NSAMPLEX + 1;

        x = (float *) malloc(n * sizeof(float));
        z = (float *) malloc(n * sizeof(float));
  
        for (i=0; i<NSAMPLEX; i++){
                lambda = (i * 1.0/(NSAMPLEX-1));

                xx = lambda * xmax + (1 - lambda) * xmin;
                x[i] = xx;
                z[i] = seval((int*) &(s1->n), &xx,
                             s1->x, s1->z, s1->b, s1->c, s1->d);

                xx = lambda * xmin + (1 - lambda) * xmax;
                x[NSAMPLEX+i] = xx;
                z[NSAMPLEX+i] = seval((int*) &(s2->n), &xx,
                                      s2->x, s2->z, s2->b, s2->c, s2->d);
        }

        xx = xmin;
        x[n-1] = xmin;
        z[n-1] = seval((int*) &(s1->n), &xx,
                       s1->x, s1->z, s1->b, s1->c, s1->d);

        AGR_fillcolor=cor;
        WriteAGRDataXY(agrfp, x, z, n);

        free(x);
        free(z);
}

/*--------------------------------------------------------------*/

void
SetAGRMyPalette(void)
{
        int i;

        AGR_numberofcolors = 52;
  
        AGR_palette = (unsigned short **)
                malloc(AGR_numberofcolors *sizeof(unsigned short*));
        AGR_colorname = (char **) malloc(AGR_numberofcolors * sizeof(char*));

        for (i=0; i<AGR_numberofcolors; i++){
                AGR_palette[i] = (unsigned short *) malloc(3 * sizeof(unsigned short));
                AGR_colorname[i] = (char *) malloc(25 * sizeof(char));
        }

        AGR_palette[ 0][0]=255; AGR_palette[ 0][1]=255; AGR_palette[ 0][2]=255;
        AGR_palette[ 1][0]=  0; AGR_palette[ 1][1]=  0; AGR_palette[ 1][2]=  0;
        AGR_palette[ 2][0]=135; AGR_palette[ 2][1]=206; AGR_palette[ 2][2]=235;
        AGR_palette[ 3][0]=255; AGR_palette[ 3][1]=250; AGR_palette[ 3][2]=205;
        AGR_palette[ 4][0]=238; AGR_palette[ 4][1]=232; AGR_palette[ 4][2]=170;
        AGR_palette[ 5][0]=240; AGR_palette[ 5][1]=230; AGR_palette[ 5][2]=140;
        AGR_palette[ 6][0]=222; AGR_palette[ 6][1]=184; AGR_palette[ 6][2]=135;
        AGR_palette[ 7][0]=205; AGR_palette[ 7][1]=133; AGR_palette[ 7][2]= 63;
        AGR_palette[ 8][0]=160; AGR_palette[ 8][1]= 82; AGR_palette[ 8][2]= 45;
        AGR_palette[ 9][0]=139; AGR_palette[ 9][1]= 69; AGR_palette[ 9][2]= 19;
        AGR_palette[10][0]=244; AGR_palette[10][1]=164; AGR_palette[10][2]= 96;
        AGR_palette[11][0]=205; AGR_palette[11][1]= 92; AGR_palette[11][2]= 92;
        AGR_palette[12][0]=233; AGR_palette[12][1]=150; AGR_palette[12][2]= 12;
        AGR_palette[13][0]=240; AGR_palette[13][1]=248; AGR_palette[13][2]=255;
        AGR_palette[14][0]=250; AGR_palette[14][1]=235; AGR_palette[14][2]=215;
        AGR_palette[15][0]=255; AGR_palette[15][1]=228; AGR_palette[15][2]=181;
        AGR_palette[16][0]=255; AGR_palette[16][1]=218; AGR_palette[16][2]=185;
        AGR_palette[17][0]=245; AGR_palette[17][1]=245; AGR_palette[17][2]=220;
        AGR_palette[18][0]=245; AGR_palette[18][1]=222; AGR_palette[18][2]=179;
        AGR_palette[19][0]=210; AGR_palette[19][1]=180; AGR_palette[19][2]=140;
        AGR_palette[20][0]=220; AGR_palette[20][1]=220; AGR_palette[20][2]=220;
        AGR_palette[21][0]= 47; AGR_palette[21][1]= 79; AGR_palette[21][2]= 79;
        AGR_palette[22][0]=105; AGR_palette[22][1]=105; AGR_palette[22][2]=105;
        AGR_palette[23][0]=112; AGR_palette[23][1]=128; AGR_palette[23][2]=144;
        AGR_palette[24][0]=119; AGR_palette[24][1]=136; AGR_palette[24][2]=153;
        AGR_palette[25][0]=190; AGR_palette[25][1]=190; AGR_palette[25][2]=190;
        AGR_palette[26][0]=205; AGR_palette[26][1]=201; AGR_palette[26][2]=201;
        AGR_palette[27][0]=139; AGR_palette[27][1]=137; AGR_palette[27][2]=137;
        AGR_palette[28][0]=255; AGR_palette[28][1]=246; AGR_palette[28][2]= 14;
        AGR_palette[29][0]=238; AGR_palette[29][1]=230; AGR_palette[29][2]= 13;
        AGR_palette[30][0]=205; AGR_palette[30][1]=198; AGR_palette[30][2]= 11;
        AGR_palette[31][0]=139; AGR_palette[31][1]=134; AGR_palette[31][2]= 78;
        AGR_palette[32][0]= 70; AGR_palette[32][1]=130; AGR_palette[32][2]=180;
        AGR_palette[33][0]=100; AGR_palette[33][1]=149; AGR_palette[33][2]=237;
        AGR_palette[34][0]= 72; AGR_palette[34][1]= 61; AGR_palette[34][2]=139;
        AGR_palette[35][0]= 46; AGR_palette[35][1]=139; AGR_palette[35][2]= 87;
        AGR_palette[36][0]= 85; AGR_palette[36][1]=107; AGR_palette[36][2]= 47;
        AGR_palette[37][0]=189; AGR_palette[37][1]=183; AGR_palette[37][2]=107;
        AGR_palette[38][0]=250; AGR_palette[38][1]=250; AGR_palette[38][2]=210;
        AGR_palette[39][0]=255; AGR_palette[39][1]=215; AGR_palette[39][2]=  0;
        AGR_palette[40][0]=218; AGR_palette[40][1]=165; AGR_palette[40][2]= 32;
        AGR_palette[41][0]=184; AGR_palette[41][1]=134; AGR_palette[41][2]= 11;
        AGR_palette[42][0]=255; AGR_palette[42][1]=165; AGR_palette[42][2]=  0;
        AGR_palette[43][0]=255; AGR_palette[43][1]=140; AGR_palette[43][2]=  0;
        AGR_palette[44][0]=255; AGR_palette[44][1]=  0; AGR_palette[44][2]=  0;
        AGR_palette[45][0]=  0; AGR_palette[45][1]=255; AGR_palette[45][2]=  0;
        AGR_palette[46][0]=  0; AGR_palette[46][1]=  0; AGR_palette[46][2]=255;
        AGR_palette[47][0]=255; AGR_palette[47][1]=255; AGR_palette[47][2]=  0;
        AGR_palette[48][0]=  0; AGR_palette[48][1]=255; AGR_palette[48][2]=255;
        AGR_palette[49][0]=255; AGR_palette[49][1]=  0; AGR_palette[49][2]=255;
        AGR_palette[50][0]=148; AGR_palette[50][1]=  0; AGR_palette[50][2]=211;
        AGR_palette[51][0]= 64; AGR_palette[51][1]=224; AGR_palette[51][2]=208;

        strcpy(AGR_colorname[ 0], "white");
        strcpy(AGR_colorname[ 1], "black");
        strcpy(AGR_colorname[ 2], "sky_blue");
        strcpy(AGR_colorname[ 3], "lemon_chiffon");
        strcpy(AGR_colorname[ 4], "pale_goldenrod");
        strcpy(AGR_colorname[ 5], "khaki");
        strcpy(AGR_colorname[ 6], "burlywood");
        strcpy(AGR_colorname[ 7], "peru");
        strcpy(AGR_colorname[ 8], "sienna");
        strcpy(AGR_colorname[ 9], "saddle_brown");
        strcpy(AGR_colorname[10], "sandy_brown");
        strcpy(AGR_colorname[11], "indian_red");
        strcpy(AGR_colorname[12], "dark_salmon");
        strcpy(AGR_colorname[13], "alice_blue");
        strcpy(AGR_colorname[14], "antique_white");
        strcpy(AGR_colorname[15], "moccasin");
        strcpy(AGR_colorname[16], "peach_puff");
        strcpy(AGR_colorname[17], "beige");
        strcpy(AGR_colorname[18], "wheat");
        strcpy(AGR_colorname[19], "tan");
        strcpy(AGR_colorname[20], "gainsboro");
        strcpy(AGR_colorname[21], "dark_slate_gray");
        strcpy(AGR_colorname[22], "dim_gray");
        strcpy(AGR_colorname[23], "slate_gray");
        strcpy(AGR_colorname[24], "light_slate_gray");
        strcpy(AGR_colorname[25], "gray");
        strcpy(AGR_colorname[26], "snow3");
        strcpy(AGR_colorname[27], "snow4");
        strcpy(AGR_colorname[28], "khaki1");
        strcpy(AGR_colorname[29], "khaki2");
        strcpy(AGR_colorname[30], "khaki3");
        strcpy(AGR_colorname[31], "khaki4");
        strcpy(AGR_colorname[32], "steel_blue");
        strcpy(AGR_colorname[33], "cornflower_blue");
        strcpy(AGR_colorname[34], "dark_slate_blue");
        strcpy(AGR_colorname[35], "sea_green");
        strcpy(AGR_colorname[36], "dark_olive_green");
        strcpy(AGR_colorname[37], "dark_khaki");
        strcpy(AGR_colorname[38], "light_goldenrod_yellow");
        strcpy(AGR_colorname[39], "gold");
        strcpy(AGR_colorname[40], "goldenrod");
        strcpy(AGR_colorname[41], "dark_goldenrod");
        strcpy(AGR_colorname[42], "orange");
        strcpy(AGR_colorname[43], "dark_orange");
        strcpy(AGR_colorname[44], "red");
        strcpy(AGR_colorname[45], "green");
        strcpy(AGR_colorname[46], "blue");
        strcpy(AGR_colorname[47], "yellow");
        strcpy(AGR_colorname[48], "cyan");
        strcpy(AGR_colorname[49], "magenta");
        strcpy(AGR_colorname[50], "violet");
        strcpy(AGR_colorname[51], "turquoise");
}

