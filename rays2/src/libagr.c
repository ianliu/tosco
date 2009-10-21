/*  rays2 - Draw seismic rays using grace
 *  Copyright (C) 2002-2009 Ricardo Biloti <biloti@ime.unicamp.br>
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

#ifndef _LIBAGR_H 
#include "libagr.h"
#endif

int ReadLine(char *string, int n, FILE *fp);

void InitAGR()
{

  AGR_defaultlinewidth = 1.0;
  AGR_defaultlinestyle = 1;
  AGR_defaultcolor = 1;
  AGR_defaultpattern = 1;
  AGR_defaultfont = 4;             /* Helvetica */
  AGR_defaultbackgroundcolor = 0;

  /* Variaveis de inicilizacao dos eixos */

  AGR_viewxmin = 0.15;
  AGR_viewxmax = 1.15;
  AGR_viewymin = 0.15;
  AGR_viewymax = 0.85;

  strcpy(AGR_xaxesinvert, "off");
  strcpy(AGR_yaxesinvert, "off");

  strcpy(AGR_xaxis,"on");
  strcpy(AGR_xaxisbar,"on");
  strcpy(AGR_xaxistick,"on");
  strcpy(AGR_xaxistickdirection,"in");
  strcpy(AGR_xaxistickplace,"normal");
  strcpy(AGR_xaxisticklabelplace,"normal");

  strcpy(AGR_yaxis,"on");
  strcpy(AGR_yaxisbar,"on");
  strcpy(AGR_yaxistick,"on");
  strcpy(AGR_yaxistickdirection,"in");
  strcpy(AGR_yaxistickplace,"normal");
  strcpy(AGR_yaxisticklabelplace,"normal");

  AGR_frametype = 0;
  AGR_framelinestyle = 1;
  AGR_framelinewidth = 1.0;
  AGR_framecolor = 1;
  AGR_framepattern = 1;
  AGR_framebackgroundcolor = 0;
  AGR_framebackgroundpattern = 0;

  strcpy(AGR_title,"");
  AGR_titlefont = 4;
  AGR_titlesize = 1.5;
  AGR_titlecolor = 1;

  strcpy(AGR_subtitle,"");
  AGR_subtitlefont = 4;
  AGR_subtitlesize = 1.0;
  AGR_subtitlecolor = 1;

  /* X Axis */
  AGR_xaxislinecolor = 1;
  AGR_xaxislinestyle = 1;
  AGR_xaxislinewidth = 1.0;

  strcpy(AGR_xaxislabel,"");
  AGR_xaxislabelcharsize = 1.0;
  AGR_xaxislabelfont = 4;
  AGR_xaxislabelcolor = 1;

  AGR_xaxistickmajor = 2.0;

  /* Y Axis */
  AGR_yaxislinecolor = 1;
  AGR_yaxislinestyle = 1;
  AGR_yaxislinewidth = 1.0;
	  
  strcpy(AGR_yaxislabel,"");
  AGR_yaxislabelcharsize = 1.0;
  AGR_yaxislabelfont = 4;
  AGR_yaxislabelcolor = 1;

  AGR_yaxistickmajor = 1.0;

  /* Variaveis que controlam a criacao de graficos */

  AGR_symbol = 0;
  AGR_symbolsize = 1.0;
  AGR_symbolcolor = 1;
  AGR_symbolpattern = 1;
  AGR_symbolfillcolor = 1;
  AGR_symbolfillpattern = 0;
  AGR_symbollinewidth = 1.0;
  AGR_symbollinestyle = 1;
  AGR_symbolchar = 65;
  AGR_symbolcharfont = 4;
  AGR_symbolskip = 0;

  AGR_linetype = 1;
  AGR_linestyle = 1;
  AGR_linewidth  = 1.0;
  AGR_linecolor = 1;
  AGR_linepattern = 1;
  
  AGR_baselinetype = 0;
  strcpy(AGR_baseline, "off");
  strcpy(AGR_dropline, "off");

  AGR_filltype = 0;
  AGR_fillrule = 0;
  AGR_fillcolor = 1;
  AGR_fillpattern = 1;

  strcpy(AGR_comment,"");
}

/*
  Escreve o header para um arquivo de projeto do Grace
  (http://plasma-gate.weizmann.ac.il/Grace/).

  Dados de entrada:
  FILE *   fp     : file descriptor where the dat will
                    be written
  float   xmin   : minimum x coordinate of the graph window
  float   xmax   : maximum x coordinate of the graph window
  float   ymin   : minimum y coordinate of the graph window
  float   ymax   : maximum y coordinate of the graph window
*/
void WriteAGRHeader (FILE *fp)
{

  fprintf(fp,
	  "\n# Grace project file"
	  "\n#"
	  "\n@version 50122"
	  "\n@page size 792, 612"
	  "\n@page scroll 5%%"
	  "\n@page inout 5%%"
	  "\n@link page off"
	  "\n@map font 0 to \"Times-Roman\", \"Times-Roman\""
	  "\n@map font 1 to \"Times-Italic\", \"Times-Italic\""
	  "\n@map font 2 to \"Times-Bold\", \"Times-Bold\""
	  "\n@map font 3 to \"Times-BoldItalic\", \"Times-BoldItalic\""
	  "\n@map font 4 to \"Helvetica\", \"Helvetica\""
	  "\n@map font 5 to \"Helvetica-Oblique\", \"Helvetica-Oblique\""
	  "\n@map font 6 to \"Helvetica-Bold\", \"Helvetica-Bold\""
	  "\n@map font 7 to \"Helvetica-BoldOblique\", \"Helvetica-BoldOblique\""
	  "\n@map font 8 to \"Courier\", \"Courier\""
	  "\n@map font 9 to \"Courier-Oblique\", \"Courier-Oblique\""
	  "\n@map font 10 to \"Courier-Bold\", \"Courier-Bold\""
	  "\n@map font 11 to \"Courier-BoldOblique\", \"Courier-BoldOblique\""
	  "\n@map font 12 to \"Symbol\", \"Symbol\""
	  "\n@map font 13 to \"ZapfDingbats\", \"ZapfDingbats\"");

  WriteAGRPalette(fp);

  fprintf(fp,
	  "\n@reference date 0"
	  "\n@date wrap off"
	  "\n@date wrap year 1950");

  fprintf(fp,"\n@default linewidth %3.1f", AGR_defaultlinewidth);
  fprintf(fp,"\n@default linestyle %i", AGR_defaultlinestyle);
  fprintf(fp,"\n@default color %i", AGR_defaultcolor);
  fprintf(fp,"\n@default pattern %i", AGR_defaultpattern);
  fprintf(fp,"\n@default font %i", AGR_defaultfont);
  fprintf(fp,"\n@default char size 1.000000");
  fprintf(fp,"\n@default symbol size 1.000000");
  fprintf(fp,"\n@default sformat \"%%16.8g\"");
  fprintf(fp,"\n@background color %i", AGR_defaultbackgroundcolor);

  fprintf(fp,
	  "\n@page background fill on"
	  "\n@timestamp off"
	  "\n@timestamp 0.03, 0.03"
	  "\n@timestamp color 1"
	  "\n@timestamp rot 0"
	  "\n@timestamp font 4"
	  "\n@timestamp char size 1.000000"
	  "\n@timestamp def \"*** *** -- --:--:-- ----\""
	  "\n@r0 off"
	  "\n@link r0 to g0"
	  "\n@r0 type above"
	  "\n@r0 linestyle 1"
	  "\n@r0 linewidth 1.0"
	  "\n@r0 color 1"
	  "\n@r0 line 0, 0, 0, 0"
	  "\n@r1 off"
	  "\n@link r1 to g0"
	  "\n@r1 type above"
	  "\n@r1 linestyle 1"
	  "\n@r1 linewidth 1.0"
	  "\n@r1 color 1"
	  "\n@r1 line 0, 0, 0, 0"
	  "\n@r2 off"
	  "\n@link r2 to g0"
	  "\n@r2 type above"
	  "\n@r2 linestyle 1"
	  "\n@r2 linewidth 1.0"
	  "\n@r2 color 1"
	  "\n@r2 line 0, 0, 0, 0"
	  "\n@r3 off"
	  "\n@link r3 to g0"
	  "\n@r3 type above"
	  "\n@r3 linestyle 1"
	  "\n@r3 linewidth 1.0"
	  "\n@r3 color 1"
	  "\n@r3 line 0, 0, 0, 0"
	  "\n@r4 off"
	  "\n@link r4 to g0"
	  "\n@r4 type above"
	  "\n@r4 linestyle 1"
	  "\n@r4 linewidth 1.0"
	  "\n@r4 color 1"
	  "\n@r4 line 0, 0, 0, 0");
}

void WriteAGRGraph(FILE *fp, float xmin, float xmax,
		   float ymin, float ymax)
{
  AGR_graphindex++;
  AGR_plotindex = 0;

  fprintf(fp,"\n@g%i on", AGR_graphindex);
  fprintf(fp,"\n@g%i hidden false", AGR_graphindex);
  fprintf(fp,"\n@g%i type XY", AGR_graphindex);
  fprintf(fp,"\n@g%i stacked false", AGR_graphindex);
  fprintf(fp,"\n@g%i bar hgap 0.000000", AGR_graphindex);
  fprintf(fp,"\n@g%i fixedpoint off", AGR_graphindex);
  fprintf(fp,"\n@g%i fixedpoint type 0", AGR_graphindex);
  fprintf(fp,"\n@g%i fixedpoint xy 0.000000, 0.000000", AGR_graphindex);
  fprintf(fp,"\n@g%i fixedpoint format general general", AGR_graphindex);
  fprintf(fp,"\n@g%i fixedpoint prec 6, 6", AGR_graphindex);
  fprintf(fp,"\n@with g%i", AGR_graphindex);
  fprintf(fp,"\n@    world %.5f, %.5f, %.5f, %.5f ",xmin, ymin, xmax, ymax);
  fprintf(fp,"\n@    stack world 0, 0, 0, 0");
  fprintf(fp,"\n@    znorm 1");
  fprintf(fp,"\n@    view %.3f, %.3f, %.3f, %.3f",
	  AGR_viewxmin, AGR_viewymin, AGR_viewxmax, AGR_viewymax);
  fprintf(fp,"\n@    title \"%s\"", AGR_title);
  fprintf(fp,"\n@    title font %i", AGR_titlefont);
  fprintf(fp,"\n@    title size %8.5f", AGR_titlesize);
  fprintf(fp,"\n@    title color %i", AGR_titlecolor);
  fprintf(fp,"\n@    subtitle \"%s\"", AGR_subtitle);
  fprintf(fp,"\n@    subtitle font %i", AGR_subtitlefont);
  fprintf(fp,"\n@    subtitle size %8.5f", AGR_subtitlesize);
  fprintf(fp,"\n@    subtitle color %i", AGR_subtitlecolor);
  fprintf(fp,"\n@    xaxes scale Normal");
  fprintf(fp,"\n@    yaxes scale Normal");
  fprintf(fp,"\n@    xaxes invert %s", AGR_xaxesinvert);
  fprintf(fp,"\n@    yaxes invert %s", AGR_yaxesinvert);
  fprintf(fp,"\n@    xaxis  %s", AGR_xaxis);
  fprintf(fp,"\n@    xaxis  type zero false");
  fprintf(fp,"\n@    xaxis  offset 0.000000 , 0.000000");
  fprintf(fp,"\n@    xaxis  bar %s", AGR_xaxisbar);
  fprintf(fp,"\n@    xaxis  bar color %i", AGR_xaxislinecolor);
  fprintf(fp,"\n@    xaxis  bar linestyle %i", AGR_xaxislinestyle);
  fprintf(fp,"\n@    xaxis  bar linewidth %3.1f", AGR_xaxislinewidth);
  fprintf(fp,"\n@    xaxis  label \"%s\"", AGR_xaxislabel);
  fprintf(fp,"\n@    xaxis  label layout para");
  fprintf(fp,"\n@    xaxis  label place auto");
  fprintf(fp,"\n@    xaxis  label char size %8.6f", AGR_xaxislabelcharsize);
  fprintf(fp,"\n@    xaxis  label font %i", AGR_xaxislabelfont);
  fprintf(fp,"\n@    xaxis  label color %i", AGR_xaxislabelcolor);
  fprintf(fp,"\n@    xaxis  label place normal");
  fprintf(fp,"\n@    xaxis  tick %s", AGR_xaxistick);
  fprintf(fp,"\n@    xaxis  tick major %5.3f", AGR_xaxistickmajor);
  fprintf(fp,"\n@    xaxis  tick minor ticks 1");
  fprintf(fp,"\n@    xaxis  tick default 6");
  fprintf(fp,"\n@    xaxis  tick place rounded true");
  fprintf(fp,"\n@    xaxis  tick %s", AGR_xaxistickdirection);

  fprintf(fp,
	  "\n@    xaxis  tick major size 1.000000"
	  "\n@    xaxis  tick major color 1"
	  "\n@    xaxis  tick major linewidth 1.0"
	  "\n@    xaxis  tick major linestyle 1"
	  "\n@    xaxis  tick major grid off"
	  "\n@    xaxis  tick minor color 1"
	  "\n@    xaxis  tick minor linewidth 1.0"
	  "\n@    xaxis  tick minor linestyle 1"
	  "\n@    xaxis  tick minor grid off"
	  "\n@    xaxis  tick minor size 0.500000"
	  "\n@    xaxis  ticklabel on"
	  "\n@    xaxis  ticklabel format general"
	  "\n@    xaxis  ticklabel prec 5"
	  "\n@    xaxis  ticklabel formula \"\""
	  "\n@    xaxis  ticklabel append \"\""
	  "\n@    xaxis  ticklabel prepend \"\""
	  "\n@    xaxis  ticklabel angle 0"
	  "\n@    xaxis  ticklabel skip 0"
	  "\n@    xaxis  ticklabel stagger 0");

  fprintf(fp,"\n@    xaxis  ticklabel place %s", AGR_xaxisticklabelplace);

  fprintf(fp,
	  "\n@    xaxis  ticklabel offset auto"
	  "\n@    xaxis  ticklabel offset 0.000000 , 0.010000"
	  "\n@    xaxis  ticklabel start type auto"
	  "\n@    xaxis  ticklabel start 0.000000"
	  "\n@    xaxis  ticklabel stop type auto"
	  "\n@    xaxis  ticklabel stop 0.000000"
	  "\n@    xaxis  ticklabel char size 1.000000"
	  "\n@    xaxis  ticklabel font 4"
	  "\n@    xaxis  ticklabel color 1");

  fprintf(fp,"\n@    xaxis  tick place %s", AGR_xaxistickplace);
  fprintf(fp,"\n@    xaxis  tick spec type none");

  fprintf(fp,"\n@    yaxis  %s", AGR_yaxis);
  fprintf(fp,"\n@    yaxis  type zero false");
  fprintf(fp,"\n@    yaxis  offset 0.000000 , 0.000000");
  fprintf(fp,"\n@    yaxis  bar %s", AGR_yaxisbar);
  fprintf(fp,"\n@    yaxis  bar color %i", AGR_yaxislinecolor);
  fprintf(fp,"\n@    yaxis  bar linestyle %i", AGR_yaxislinestyle);
  fprintf(fp,"\n@    yaxis  bar linewidth %3.1f", AGR_yaxislinewidth);
  fprintf(fp,"\n@    yaxis  label \"%s\"", AGR_yaxislabel);
  fprintf(fp,"\n@    yaxis  label layout para");
  fprintf(fp,"\n@    yaxis  label place auto");
  fprintf(fp,"\n@    yaxis  label char size %8.6f", AGR_yaxislabelcharsize);
  fprintf(fp,"\n@    yaxis  label font %i", AGR_yaxislabelfont);
  fprintf(fp,"\n@    yaxis  label color %i", AGR_yaxislabelcolor);
  fprintf(fp,"\n@    yaxis  label place normal");
  fprintf(fp,"\n@    yaxis  tick %s", AGR_yaxistick);
  fprintf(fp,"\n@    yaxis  tick major %5.3f", AGR_yaxistickmajor);
  fprintf(fp,"\n@    yaxis  tick minor ticks 1");
  fprintf(fp,"\n@    yaxis  tick default 6");
  fprintf(fp,"\n@    yaxis  tick place rounded true");
  fprintf(fp,"\n@    yaxis  tick %s", AGR_yaxistickdirection);

  fprintf(fp,
	  "\n@    yaxis  tick major size 1.000000"
	  "\n@    yaxis  tick major color 1"
	  "\n@    yaxis  tick major linewidth 1.0"
	  "\n@    yaxis  tick major linestyle 1"
	  "\n@    yaxis  tick major grid off"
	  "\n@    yaxis  tick minor color 1"
	  "\n@    yaxis  tick minor linewidth 1.0"
	  "\n@    yaxis  tick minor linestyle 1"
	  "\n@    yaxis  tick minor grid off"
	  "\n@    yaxis  tick minor size 0.500000"
	  "\n@    yaxis  ticklabel on"
	  "\n@    yaxis  ticklabel format general"
	  "\n@    yaxis  ticklabel prec 5"
	  "\n@    yaxis  ticklabel formula \"\""
	  "\n@    yaxis  ticklabel append \"\""
	  "\n@    yaxis  ticklabel prepend \"\""
	  "\n@    yaxis  ticklabel angle 0"
	  "\n@    yaxis  ticklabel skip 0"
	  "\n@    yaxis  ticklabel stagger 0");

  fprintf(fp,"\n@    yaxis  ticklabel place %s", AGR_yaxisticklabelplace);

  fprintf(fp,
	  "\n@    yaxis  ticklabel offset auto"
	  "\n@    yaxis  ticklabel offset 0.000000 , 0.010000"
	  "\n@    yaxis  ticklabel start type auto"
	  "\n@    yaxis  ticklabel start 0.000000"
	  "\n@    yaxis  ticklabel stop type auto"
	  "\n@    yaxis  ticklabel stop 0.000000"
	  "\n@    yaxis  ticklabel char size 1.000000"
	  "\n@    yaxis  ticklabel font 4"
	  "\n@    yaxis  ticklabel color 1");

  fprintf(fp,"\n@    yaxis  tick place %s", AGR_yaxistickplace);

  fprintf(fp,
	  "\n@    yaxis  tick spec type none"
	  "\n@    altxaxis  off"
	  "\n@    altyaxis  off"
	  "\n@    legend on"
	  "\n@    legend loctype view"
	  "\n@    legend 0.85, 0.8"
	  "\n@    legend box color 1"
	  "\n@    legend box pattern 1"
	  "\n@    legend box linewidth 1.0"
	  "\n@    legend box linestyle 1"
	  "\n@    legend box fill color 0"
	  "\n@    legend box fill pattern 1"
	  "\n@    legend font 4"
	  "\n@    legend char size 1.000000"
	  "\n@    legend color 1"
	  "\n@    legend length 4"
	  "\n@    legend vgap 1"
	  "\n@    legend hgap 1"
	  "\n@    legend invert false");

  fprintf(fp,"\n@    frame type %i", AGR_frametype);
  fprintf(fp,"\n@    frame linestyle %i", AGR_framelinestyle);
  fprintf(fp,"\n@    frame linewidth %4.2f", AGR_framelinewidth);
  fprintf(fp,"\n@    frame color %i", AGR_framecolor);
  fprintf(fp,"\n@    frame pattern %i", AGR_framepattern);
  fprintf(fp,"\n@    frame background color %i", AGR_framebackgroundcolor);
  fprintf(fp,"\n@    frame background pattern %i\n", AGR_framebackgroundpattern);
}

/****************************************************************/

/*
  Escreve um bloco de dados typo (x,y)

  Dados de entrada:
  FILE *     fp   : file descriptor

*/
void WriteAGRDataXY (FILE *fp, float *x, float *y, long int n)
{
  long int i;

  fprintf(fp,"@    s%i hidden false\n", AGR_plotindex);
  fprintf(fp,"@    s%i type xy\n", AGR_plotindex);
  fprintf(fp,"@    s%i symbol %i\n", AGR_plotindex, AGR_symbol);
  fprintf(fp,"@    s%i symbol size %8.6f\n", AGR_plotindex, AGR_symbolsize);
  fprintf(fp,"@    s%i symbol color %i\n", AGR_plotindex, AGR_symbolcolor);
  fprintf(fp,"@    s%i symbol pattern %i\n", AGR_plotindex, AGR_symbolpattern);
  fprintf(fp,"@    s%i symbol fill color %i\n", AGR_plotindex, AGR_symbolfillcolor);
  fprintf(fp,"@    s%i symbol fill pattern %i\n", AGR_plotindex, AGR_symbolfillpattern);
  fprintf(fp,"@    s%i symbol linewidth %3.1f\n", AGR_plotindex, AGR_symbollinewidth);
  fprintf(fp,"@    s%i symbol linestyle %i\n", AGR_plotindex, AGR_symbollinestyle);
  fprintf(fp,"@    s%i symbol char %i\n", AGR_plotindex, AGR_symbolchar);
  fprintf(fp,"@    s%i symbol char font %i\n", AGR_plotindex, AGR_symbolcharfont);
  fprintf(fp,"@    s%i symbol skip %i\n", AGR_plotindex, AGR_symbolskip);
  fprintf(fp,"@    s%i line type %i\n", AGR_plotindex, AGR_linetype);
  fprintf(fp,"@    s%i line linestyle %i\n", AGR_plotindex, AGR_linestyle);
  fprintf(fp,"@    s%i line linewidth %3.1f\n", AGR_plotindex, AGR_linewidth);
  fprintf(fp,"@    s%i line color %i\n", AGR_plotindex, AGR_linecolor);
  fprintf(fp,"@    s%i line pattern %i\n", AGR_plotindex, AGR_linepattern);

  fprintf(fp,"@    s%i baseline type %i\n", AGR_plotindex, AGR_baselinetype);
  fprintf(fp,"@    s%i baseline %s\n", AGR_plotindex, AGR_baseline);
  fprintf(fp,"@    s%i dropline %s\n", AGR_plotindex, AGR_dropline);
  fprintf(fp,"@    s%i fill type %i\n", AGR_plotindex, AGR_filltype);
  fprintf(fp,"@    s%i fill rule %i\n", AGR_plotindex, AGR_fillrule);
  fprintf(fp,"@    s%i fill color %i\n", AGR_plotindex, AGR_fillcolor);
  fprintf(fp,"@    s%i fill pattern %i\n", AGR_plotindex, AGR_fillpattern);

  fprintf(fp,"@    s%i avalue off\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue type 2\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue char size 1.000000\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue font 4\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue color 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue rot 0\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue format general\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue prec 3\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue prepend \"\"\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue append \"\"\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue offset 0.000000 , 0.000000\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar on\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar place both\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar color 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar pattern 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar size 1.000000\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar linewidth 1.0\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar linestyle 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser linewidth 1.0\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser linestyle 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser clip off\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser clip length 0.100000\n", AGR_plotindex);
  fprintf(fp,"@    s%i comment \"%s\"\n", AGR_plotindex, AGR_comment);

  fprintf(fp,"@target G%i.S%i\n", AGR_graphindex, AGR_plotindex);
  fprintf(fp,"@type xy\n");

  for (i=0; i<n; i++)
    fprintf(fp,"         %12.8e  %12.8e\n",x[i],y[i]);

  fprintf(fp,"&\n");

  AGR_plotindex++;
    
}

/****************************************************************/

/*
  Escreve um bloco de dados typo (x,y)

  Dados de entrada:
  FILE *     fp   : file descriptor

*/
void WriteAGRDataXYCOLOR (FILE *fp,
			  float *x, float *y, int *color, long int n)
{
  long int i;

  fprintf(fp,"@    s%i hidden false\n", AGR_plotindex);
  fprintf(fp,"@    s%i type xycolor\n", AGR_plotindex);
  fprintf(fp,"@    s%i symbol %i\n", AGR_plotindex, AGR_symbol);
  fprintf(fp,"@    s%i symbol size %8.6f\n", AGR_plotindex, AGR_symbolsize);
  fprintf(fp,"@    s%i symbol color %i\n", AGR_plotindex, AGR_symbolcolor);
  fprintf(fp,"@    s%i symbol pattern %i\n", AGR_plotindex, AGR_symbolpattern);
  fprintf(fp,"@    s%i symbol fill color %i\n", AGR_plotindex, AGR_symbolfillcolor);
  fprintf(fp,"@    s%i symbol fill pattern %i\n", AGR_plotindex, AGR_symbolfillpattern);
  fprintf(fp,"@    s%i symbol linewidth %3.1f\n", AGR_plotindex, AGR_symbollinewidth);
  fprintf(fp,"@    s%i symbol linestyle %i\n", AGR_plotindex, AGR_symbollinestyle);
  fprintf(fp,"@    s%i symbol char %i\n", AGR_plotindex, AGR_symbolchar);
  fprintf(fp,"@    s%i symbol char font %i\n", AGR_plotindex, AGR_symbolcharfont);
  fprintf(fp,"@    s%i symbol skip %i\n", AGR_plotindex, AGR_symbolskip);
  fprintf(fp,"@    s%i line type %i\n", AGR_plotindex, AGR_linetype);
  fprintf(fp,"@    s%i line linestyle %i\n", AGR_plotindex, AGR_linestyle);
  fprintf(fp,"@    s%i line linewidth %3.1f\n", AGR_plotindex, AGR_linewidth);
  fprintf(fp,"@    s%i line color %i\n", AGR_plotindex, AGR_linecolor);
  fprintf(fp,"@    s%i line pattern %i\n", AGR_plotindex, AGR_linepattern);

  fprintf(fp,"@    s%i baseline type %i\n", AGR_plotindex, AGR_baselinetype);
  fprintf(fp,"@    s%i baseline %s\n", AGR_plotindex, AGR_baseline);
  fprintf(fp,"@    s%i dropline %s\n", AGR_plotindex, AGR_dropline);
  fprintf(fp,"@    s%i fill type %i\n", AGR_plotindex, AGR_filltype);
  fprintf(fp,"@    s%i fill rule %i\n", AGR_plotindex, AGR_fillrule);
  fprintf(fp,"@    s%i fill color %i\n", AGR_plotindex, AGR_fillcolor);
  fprintf(fp,"@    s%i fill pattern %i\n", AGR_plotindex, AGR_fillpattern);

  fprintf(fp,"@    s%i avalue off\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue type 2\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue char size 1.000000\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue font 4\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue color 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue rot 0\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue format general\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue prec 3\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue prepend \"\"\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue append \"\"\n", AGR_plotindex);
  fprintf(fp,"@    s%i avalue offset 0.000000 , 0.000000\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar on\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar place both\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar color 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar pattern 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar size 1.000000\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar linewidth 1.0\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar linestyle 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser linewidth 1.0\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser linestyle 1\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser clip off\n", AGR_plotindex);
  fprintf(fp,"@    s%i errorbar riser clip length 0.100000\n", AGR_plotindex);
  fprintf(fp,"@    s%i comment \"%s\"\n", AGR_plotindex, AGR_comment);

  fprintf(fp,"@target G%i.S%i\n", AGR_graphindex, AGR_plotindex);
  fprintf(fp,"@type xycolor\n");

  for (i=0; i<n; i++)
    fprintf(fp,"         %12.8e  %12.8e   %3i\n",x[i],y[i],color[i]);

  fprintf(fp,"&\n");

  AGR_plotindex++;
    
}

/****************************************************************/

void WriteAGRPalette (FILE *fp)
{
  int i;

  for (i=0; i<AGR_numberofcolors; i++){
     fprintf(fp,"\n@map color %i to (%3i, %3i, %3i), \"%s\"",
	     i,
	     AGR_palette[i][0], 
	     AGR_palette[i][1],
	     AGR_palette[i][2],
	     AGR_colorname[i]);
  }
}

/****************************************************************/

void SetAGRDefaultPalette()
{
  int i;

  AGR_numberofcolors = 37;

  AGR_palette = 
    (unsigned short **) malloc(AGR_numberofcolors * sizeof(unsigned short*));
  AGR_colorname = (char **) malloc(AGR_numberofcolors * sizeof(char*));

  for (i=0; i<AGR_numberofcolors; i++){
     AGR_palette[i] = (unsigned short *) malloc(3 * sizeof(unsigned short));
     AGR_colorname[i] = (char *) malloc(10 * sizeof(char));
     sprintf(AGR_colorname[i],"color%i",i);
  }

  AGR_palette[ 0][0]=255; AGR_palette[ 0][1]=255; AGR_palette[ 0][2]=255;
  AGR_palette[ 1][0]=  0; AGR_palette[ 1][1]=  0; AGR_palette[ 1][2]=  0;
  AGR_palette[ 2][0]=255; AGR_palette[ 2][1]=  0; AGR_palette[ 2][2]=  0;
  AGR_palette[ 3][0]=  0; AGR_palette[ 3][1]=255; AGR_palette[ 3][2]=  0;
  AGR_palette[ 4][0]=  0; AGR_palette[ 4][1]=  0; AGR_palette[ 4][2]=255;
  AGR_palette[ 5][0]=255; AGR_palette[ 5][1]=255; AGR_palette[ 5][2]=  0;
  AGR_palette[ 6][0]=188; AGR_palette[ 6][1]=143; AGR_palette[ 6][2]=143;
  AGR_palette[ 7][0]=220; AGR_palette[ 7][1]=220; AGR_palette[ 7][2]=220;
  AGR_palette[ 8][0]=148; AGR_palette[ 8][1]=  0; AGR_palette[ 8][2]=211;
  AGR_palette[ 9][0]=  0; AGR_palette[ 9][1]=255; AGR_palette[ 9][2]=255;
  AGR_palette[10][0]=255; AGR_palette[10][1]=  0; AGR_palette[10][2]=255;
  AGR_palette[11][0]=255; AGR_palette[11][1]=165; AGR_palette[11][2]=  0;
  AGR_palette[12][0]=114; AGR_palette[12][1]= 33; AGR_palette[12][2]=188;
  AGR_palette[13][0]=103; AGR_palette[13][1]=  7; AGR_palette[13][2]= 72;
  AGR_palette[14][0]= 64; AGR_palette[14][1]=224; AGR_palette[14][2]=208;
  AGR_palette[15][0]=  0; AGR_palette[15][1]=139; AGR_palette[15][2]=  0;
  AGR_palette[16][0]=105; AGR_palette[16][1]=128; AGR_palette[16][2]=144;
  AGR_palette[17][0]=105; AGR_palette[17][1]=105; AGR_palette[17][2]=105;
  AGR_palette[18][0]= 47; AGR_palette[18][1]= 79; AGR_palette[18][2]= 79;
  AGR_palette[19][0]= 70; AGR_palette[19][1]=130; AGR_palette[19][2]=180;
  AGR_palette[20][0]= 95; AGR_palette[20][1]=158; AGR_palette[20][2]=160;
  AGR_palette[21][0]=102; AGR_palette[21][1]=205; AGR_palette[21][2]=170;
  AGR_palette[22][0]=  0; AGR_palette[22][1]=100; AGR_palette[22][2]=  0;
  AGR_palette[23][0]= 85; AGR_palette[23][1]=107; AGR_palette[23][2]= 47;
  AGR_palette[24][0]=143; AGR_palette[24][1]=188; AGR_palette[24][2]=143;
  AGR_palette[25][0]= 46; AGR_palette[25][1]=139; AGR_palette[25][2]= 87;
  AGR_palette[26][0]=189; AGR_palette[26][1]=183; AGR_palette[26][2]=107;
  AGR_palette[27][0]=238; AGR_palette[27][1]=232; AGR_palette[27][2]=170;
  AGR_palette[28][0]=218; AGR_palette[28][1]=165; AGR_palette[28][2]= 32;
  AGR_palette[29][0]=184; AGR_palette[29][1]=134; AGR_palette[29][2]= 11;
  AGR_palette[30][0]=255; AGR_palette[30][1]=140; AGR_palette[30][2]=  0;
  AGR_palette[31][0]= 58; AGR_palette[31][1]= 95; AGR_palette[31][2]=205;
  AGR_palette[32][0]=205; AGR_palette[32][1]=181; AGR_palette[32][2]=205;
  AGR_palette[33][0]=102; AGR_palette[33][1]=102; AGR_palette[33][2]=102;
  AGR_palette[34][0]=127; AGR_palette[34][1]=127; AGR_palette[34][2]=127;
  AGR_palette[35][0]=153; AGR_palette[35][1]=153; AGR_palette[35][2]=153;
  AGR_palette[36][0]=179; AGR_palette[36][1]=179; AGR_palette[36][2]=179;

}

/****************************************************************/

void ImportAGRPalette(char *palette_file)
{

  FILE *fpalette;
  int i;
  char line[80], colorname[30];

  fpalette = fopen(palette_file, "r");
  i=0;
  while (!feof(fpalette)){
     ReadLine(line, 80, fpalette);
     i++;
  }
  fclose(fpalette);

  AGR_numberofcolors = i-1;

  AGR_palette = (unsigned short **)
                malloc(AGR_numberofcolors *sizeof(unsigned short*));
  AGR_colorname = (char **) malloc(AGR_numberofcolors * sizeof(char*));

  for (i=0; i<AGR_numberofcolors; i++){
     AGR_palette[i] = (unsigned short *) malloc(3 * sizeof(unsigned short));
     AGR_colorname[i] = (char *) malloc(31 * sizeof(char));
  }

  fpalette = fopen(palette_file, "r");
  for (i=0; i<AGR_numberofcolors; i++){
     int red, green, blue;
     ReadLine(line, 80, fpalette);
     sscanf(line,"%i %i %i %s",
	    &red, &green, &blue, colorname);
     
     AGR_palette[i][0] = red;
     AGR_palette[i][1] = green;
     AGR_palette[i][2] = blue;
     
     sprintf(AGR_colorname[i],"%s", colorname);
  }
  fclose(fpalette);
}

/****************************************************************/
void FreeAGRPalette()
{
   int i;

  for (i=0; i<AGR_numberofcolors; i++){
     free(AGR_palette[i]);
     free(AGR_colorname[i]);
  }
  
  free(AGR_palette);
  free(AGR_colorname);
   
}


/****************************************************************/

int ReadLine(char *string, int n, FILE *fp)
{
   if (fgets(string, n, fp)== NULL) return(EXIT_SUCCESS);
   if (string[0]== 0) return(EXIT_FAILURE);
   if (string[strlen(string)-1]== '\n') string[strlen(string)-1]= 0;
   return(EXIT_FAILURE);
}
