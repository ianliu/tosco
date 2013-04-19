/*  rays2 - Draw seismic rays using grace
 *  Copyright (C) 2002-2013 Ricardo Biloti <biloti@ime.unicamp.br>
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
#include "cmdline.h"

#define max(x,y)         ( ((x) < (y)) ? (y) : (x) )
#define min(x,y)         ( ((x) > (y)) ? (y) : (x) )

#define TRUE             1
#define FALSE            0

#define MAX_NRAYS        100000
#define MAX_CODEWAVES    400
#define MAX_NINTERF      100
#define NSAMPLEX         151

/* ---------------------------------------------------- */

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

/* ---------------------------------------------------- */

void ray_read (FILE *fp, int n, int ind, ray_t *ray);
void ray_free (ray_t *ray);
void interface_alloc (interface_t *interf, int npts);
void interface_free (interface_t *interf);
void interface_read (FILE *fp, interface_t *interf);
void interface_dummy_read (FILE *fp, int nn);
void SetAGRMyPalette(void);
void strip (float         xmin,
            float         xmax, 
            interface_t * s1,
            interface_t * s2,
            unsigned int  cor);

/* ---------------------------------------------------- */

size_t line = 0;
unsigned int nx;
unsigned int nz;

/* ---------------------------------------------------- */
int main(int argc, char ** argv){

        struct gengetopt_args_info arg;

        const int cor[] = {   1, 36,  9, 41, 28,  8, 14,  5,  4,
                              31,  6, 29,  7,  9, 10, 11, 12, 13,
                              16, 17, 18, 19, 20, 21, 22, 23, 24,
                              25, 26, 27, 28, 32, 33, 34, 35,  0};

        int            i;
        int            icont;
        int            nint;
        int            nrays;
        int            nwavecode;
        int            ns[MAX_CODEWAVES];
        ray_t          ray[MAX_NRAYS];
        interface_t    interf[MAX_NINTERF];
        float          tmax = 0;

        if (cmdline_parser(argc, argv, &arg) != 0)
                exit(EXIT_FAILURE);

        nx = arg.nx_arg;
        nz = arg.nz_arg;

        /** BLOCK 1

            ICONT = 0 indicates termination of the file.
            ICONT = 1 indicates that further rays follows.

        */        
        line++;
        if (fscanf(stdin,"%3i%*3i", &icont) != 1){
                fprintf(stderr,"Block 1 corrupted in input file.\n"
                        "Expecting 2 integers on line %i\n"
                        "Aborting.\n", (int) line);
                return EXIT_FAILURE;
        }
        
        nrays =   0;
        nwavecode = 0;
        nint = 0;

        /*--------------------------------------------------------------*
         *  lu1.dat parse
         *--------------------------------------------------------------*/
        while (icont>0){
                int n, ind, namp, status;
                
                /** BLOCK 2
                    Number of interfaces 
                */
                line++;
                if (fscanf(stdin,"%d", &nint) != 1){
                        fprintf(stderr,"Block 2 corrupted in input file.\n"
                                "Expecting at least one integer on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
      
                /** BLOCK 3
                    
                    Interfaces
                 */
                for (i=0; i<nint; i++){
                        int npts;
	
                        /* Number of points for the interface i */
                        if (fscanf(stdin,"%i", &npts) != 1){
                                fprintf(stderr,"Block 3 corrupted in input file.\n"
                                        "Expecting %i integers on line %i.\n"
                                        "Aborting.\n", nint+1, (int) line);
                                return EXIT_FAILURE;
                        }

                        if (nwavecode == 0){
                                interface_alloc(&(interf[i]), npts);
                        }
                }

                /* Read interfaces only once */
                if (nwavecode == 0){
                        for (i=0; i<nint; i++){
                                interface_read(stdin, &(interf[i]));
                        }
                }
                else{
                        for (i=0; i<nint; i++){
                                interface_dummy_read(stdin, interf[i].n);
                        }
                }

                /** BLOCK 4

                  x and z source coordinates
                  density, p- and s-wave velocities ate source
                */
                line++;
                if (fscanf(stdin,"%*f %*f %*f %*f %*f") != 0){
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
                if (fscanf(stdin,"%5i%5i", &n, &ind) != 2){
                        fprintf(stderr,"Block 5 corrupted in input file.\n"
                                "Expecting two floats on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
      
                ns[nwavecode] = 0;
                while (n>0){
                        nrays++;
                        ns[nwavecode]++;
                        ray_read(stdin, n, ind, &(ray[nrays]));
	 
                        line++;
                        if (fscanf(stdin,"%5i%5i",&n,&ind) != 2){
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
                if (fscanf(stdin,"%i", &namp) != 1){
                        fprintf(stderr,"Block 7 corrupted in input file.\n"
                                "Expecting one integer on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
                namp = abs(namp);
     
                fprintf(stderr,"%3i rays corresponding to wave code %2i (%5i)\n",
                        ns[nwavecode], nwavecode+1, nrays);

                /** BLOCK 8

                    Other kinematic and dynamic quantities associated to each ray 
                */
                for (i=0;i<namp;i++){
                        float traveltime;
                        line++;
                        if (fscanf(stdin,"%*i %*f %f %*f %*f %*f %*f %*f %*f %*f %*f",
                                   &traveltime) != 1){
                                fprintf(stderr,"Block 8 corrupted in input file.\n"
                                        "Expecting one integer and ten floats on line %i.\n"
                                        "Aborting.\n", (int) line);
                                return EXIT_FAILURE;
                        }
                        tmax = max(tmax, traveltime);
                }
      
                /** BLOCK 1
                 */
                line++;
                status = fscanf(stdin,"%i%*3i", &icont);
                if ( status != 1 && status != EOF){
                        fprintf(stderr,"Block 1 corrupted in input file.\n"
                                "Expecting two integers on line %i.\n"
                                "Aborting.\n", (int) line);
                        return EXIT_FAILURE;
                }
                
                if (status == EOF)
                        icont = 0;

                nwavecode++;
        }

        fprintf(stderr, "Maximum traveltime: %.3fs\n", tmax);

        if (!arg.xmin_given){
                arg.xmin_arg = interf[0].x[0];
                for (i=1; i<nint; i++)
                        arg.xmin_arg = min(arg.xmin_arg, interf[i].x[0]);
        }

        if (!arg.zmin_given){
                arg.zmin_arg = interf[0].z[0];
                for (i=1; i<interf[0].n; i++)
                        arg.zmin_arg = min(arg.zmin_arg, interf[0].z[i]);
        }

        if (!arg.zmax_given){
                arg.zmax_arg = interf[nint-1].z[0];
                for (i=1; i<interf[nint-1].n; i++)
                        arg.zmax_arg = max(arg.zmax_arg, interf[nint-1].z[i]);
        }

        /*--------------------------------------------------------------*
         *                Writing the Gace Project File
         *--------------------------------------------------------------*/
   
        InitAGR();
  
        if (arg.palette_given)
                ImportAGRPalette(arg.palette_arg);
        else
                SetAGRMyPalette();

        WriteAGRHeader(stdout);

        sprintf(AGR_yaxesinvert, "on");
        sprintf(AGR_title, "Rays2");
        sprintf(AGR_subtitle, "(Seis88)");
        sprintf(AGR_xaxislabel, "Distance [km]");
        sprintf(AGR_yaxislabel, "Depth [km]");
        AGR_xaxislabelcharsize = 1.4;
        AGR_yaxislabelcharsize = 1.4;

        WriteAGRGraph(stdout,
                      arg.xmin_arg, arg.xmax_arg,
                      arg.zmin_arg, arg.zmax_arg);

        /**** Writing Layers ****/
        AGR_filltype = (1-arg.nofill_flag);
        AGR_linecolor = 1; /* Interfaces in black */

        for (i=0; i<nint-1; i++)
                strip(arg.xmin_arg, arg.xmax_arg, 
                      &(interf[i]), &(interf[i+1]), i+2+arg.land_flag);

        if (!arg.norays_flag){
                /**** Writing Rays ****/
                int naux, ni, nr;

                naux = 0;
                AGR_filltype = 0;
                for (ni=nwavecode-1; ni>=0; ni--){

                        AGR_linecolor = (arg.blackrays_flag ? 1 : cor[ni]);

                        for (nr=0; nr<ns[ni]; nr++, naux++){
	    
                                WriteAGRDataXY(stdout,
                                               ray[nrays-naux].x,
                                               ray[nrays-naux].z,
                                               ray[nrays-naux].n);
                        }
                }
        }

        /* Export the velocity model */
        if (arg.vfile_given){
          float *v;
          int ix, iz;
          FILE *fp;
          float v1[MAX_NINTERF], v2[MAX_NINTERF];
          int ilayer;

          fp=fopen(arg.layervel_arg, "r");
          for (ilayer=0; ilayer <nint; ilayer++){
            fscanf(fp, "%f %f\n", &v1[ilayer], &v2[ilayer]);
          }
          fclose(fp);
          fp=fopen(arg.vfile_arg, "w");

          v = (float *) malloc(sizeof(float)*nz);

          for (ix = 0; ix < nx; ix++){
            float x;
            double depth[MAX_NINTERF];

            x = arg.xmin_arg + ix * (arg.xmax_arg - arg.xmin_arg) / (nx-1);

            for (ilayer=0; ilayer <nint; ilayer++){
              depth[ilayer] = seval((int*) &(interf[ilayer].n), &x,
                                    interf[ilayer].x, interf[ilayer].z,
                                    interf[ilayer].b, interf[ilayer].c, interf[ilayer].d);
            }

            ilayer = 0;
            for (iz=0; iz<nz; iz++){
              double z;
              z = arg.zmin_arg + iz * (arg.zmax_arg - arg.zmin_arg) / (nz-1);
              
              while ((z >= depth[ilayer]) && (ilayer < nint)){
                ilayer++;
              }

              if (ilayer < nint){
                v[iz] = v1[ilayer-1] + (z - depth[ilayer-1])/(depth[ilayer]-depth[ilayer-1])*(v2[ilayer-1]-v1[ilayer-1]);
              }else{
                v[iz] = v2[nint -2];
              }
            }
            
            fwrite(v, sizeof(float), nz, fp);
                     
          }
          free(v);
          fclose(fp);
          printf("Velocity model should be exported\n");
        }

        for (i=0; i<nrays; i++){
                ray_free(&(ray[i]));
        }

        for (i=0; i<nint; i++){
                interface_free(&(interf[i]));
        }

        cmdline_parser_free(&arg);

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
strip   (float        xmin,
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
        WriteAGRDataXY(stdout, x, z, n);

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

