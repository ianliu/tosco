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

#ifndef _LIBLGM_H
#define _LIBLGM_H 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
  Variaveis globais que controlam a geracao do arquivo
  de projeto do Grace
*/

/******************************************************************/
/* HEADER Variables */

static int AGR_plotindex = 0;
static int AGR_graphindex = -1;

int AGR_numberofcolors;
unsigned short **AGR_palette;
char **AGR_colorname;

/* Defaults */
float AGR_defaultlinewidth;
short AGR_defaultlinestyle;
int AGR_defaultcolor;
short AGR_defaultpattern;
short AGR_defaultfont;
int AGR_defaultbackgroundcolor;

/* Variaveis de inicializacao dos eixos */

float AGR_viewxmin;
float AGR_viewxmax;
float AGR_viewymin;
float AGR_viewymax;

char AGR_xaxesinvert[4];
char AGR_yaxesinvert[4];

char AGR_xaxis[4];
char AGR_xaxisbar[4];
char AGR_xaxistick[4];
char AGR_xaxistickdirection[5];
char AGR_xaxistickplace[9];
char AGR_xaxisticklabelplace[9];

char AGR_yaxis[4];
char AGR_yaxisbar[4];
char AGR_yaxistick[4];
char AGR_yaxistickdirection[5];
char AGR_yaxistickplace[9];
char AGR_yaxisticklabelplace[9];

short AGR_frametype;
short AGR_framelinestyle;
float AGR_framelinewidth;
short AGR_framecolor;
short AGR_framepattern;
short AGR_framebackgroundcolor;
short AGR_framebackgroundpattern;

char AGR_title[100];
short AGR_titlefont;
float AGR_titlesize;
int AGR_titlecolor;

char AGR_subtitle[100];
short AGR_subtitlefont;
float AGR_subtitlesize;
int AGR_subtitlecolor;

/* X Axis */
int AGR_xaxislinecolor;
short AGR_xaxislinestyle;
float AGR_xaxislinewidth;

char AGR_xaxislabel[100];
float AGR_xaxislabelcharsize;
short AGR_xaxislabelfont;
int AGR_xaxislabelcolor;

float AGR_xaxistickmajor;

/* Y Axis */
int AGR_yaxislinecolor;
short AGR_yaxislinestyle;
float AGR_yaxislinewidth;

char AGR_yaxislabel[100];
float AGR_yaxislabelcharsize;
short AGR_yaxislabelfont;
int AGR_yaxislabelcolor;

float AGR_yaxistickmajor;

/* Variaveis que controlam a criacao de graficos */

short AGR_symbol;
float AGR_symbolsize;
int AGR_symbolcolor;
short AGR_symbolpattern;
int AGR_symbolfillcolor;
short AGR_symbolfillpattern;
float AGR_symbollinewidth;
short AGR_symbollinestyle;
short AGR_symbolchar;
short AGR_symbolcharfont;
int AGR_symbolskip;

short AGR_linetype;
short AGR_linestyle;
float AGR_linewidth;
int AGR_linecolor;
short AGR_linepattern;

short AGR_baselinetype;
char AGR_baseline[4];
char AGR_dropline[4];

short AGR_filltype;
short AGR_fillrule;
int AGR_fillcolor;
short AGR_fillpattern;

char AGR_comment[200];


/******************************************************************/
void InitAGR ();

void WriteAGRHeader (FILE * fp);

void WriteAGRGraph (FILE * fp, float xmin, float xmax,
		    float ymin, float ymax);

void WriteAGRDataXY (FILE * fp, float *x, float *y, long int n);

void WriteAGRDataXYCOLOR (FILE * fp, float *x, float *y,
			  int *color, long int n);

void WriteAGRPalette (FILE * fp);

void SetAGRDefaultPalette ();

void ImportAGRPalette (char *palette_file);

void FreeAGRPalette ();
/******************************************************************/

#endif				/* ifndef _LIBAGR_H */
