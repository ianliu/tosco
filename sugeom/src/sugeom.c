/* sugeom - Set geometry parameters on SU files
 * Copyright (C) 2010 Adriano Wagner CPGG/UFBA <adrianowgs@gmail.com>
 *   
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "su.h"
#include <stdio.h>
#include "segy.h"
#include "header.h"
#include "parser.h"

/****************** self documentation *************************/
char *sdoc[] = {
	"                                                          ",
	" SUGEOM				                   ",
	"                                                          ",
	" sugeom <stdin >stdout station= shot= pattern=		   ",
	"                                                          ",
	" Required Parameters:       stdin station shot pattern    ",
	"                                                          ",
	" Optional Parameters:       stdout                        ",
	"                                                          ",
	NULL};

/* Credits:
 * Adriano Wagner (LAGEP-UFBA)
 *
 */
/**************** end self doc *********************************/

int main(int argc, char **argv)
{
	segy		tr;
	long int	prev=-1;
	long int	cur;
	int 		loaded=0;

	int 		*pattern_stations=NULL;
	StationVal 	*station_array = NULL;
	StationVal 	*cur_station = NULL;
	ShotVal		*cur_shot = NULL;

	/*file names*/
	char *pattern_file;
	char *shot_file;
	char *station_file;

	/*file descriptors*/
	FILE *pattern=NULL;
	FILE *station=NULL;
	FILE *shot=NULL;

	initargs(argc, argv);
	requestdoc(1);

	/*recebe parametros*/
	if(!getparstring("station",&station_file)) err("station must be specified");
	if(!getparstring("shot",&shot_file)) err("shot must be specified");
	if(!getparstring("pattern",&pattern_file)) err("pattern must be specified");

	/*abre arquivos*/
	pattern=fopen(pattern_file,"r");
	station=fopen(station_file,"r");
	shot=fopen(shot_file,"r");

	/*checa se os arquivos foram abertos*/
	if ((pattern!=NULL)&&(station!=NULL)&&(shot!=NULL)){
		/*carrega estacoes no vetor station_array*/
		load_stations(station,&station_array); 
		gettr(&tr);
		/*loop enquanto houver tracos*/
		do {
			cur=tr.fldr;
			/*checa se mudou de tiro*/
			if (prev!=cur){
				prev=cur;
				/*carrega tiros*/
				cur_shot = return_shot(shot,cur);
				if(cur_shot)
					/*carrega pattern*/
					loaded=load_pattern(pattern,cur_shot->npattern,cur_shot->nstation,&pattern_stations);
				else
					loaded=0;
			}
			/*checa se o pattern foi carregado corretamente*/
			if (loaded){
				cur_station =  return_station(station_array, pattern_stations[tr.tracf - 1]);
				/*preenche o header*/
				if (cur_station){
					tr.sx = cur_shot->recx;
					tr.sy = cur_shot->recy;
					tr.gx = cur_station->recx;
					tr.gy = cur_station->recy;
					tr.cdp = ((tr.sx + tr.gx)/(station_array[1].recx - station_array[0].recx))+1;
					tr.offset = tr.gx - tr.sx;
					puttr(&tr);
				}
				else {	warn("%d",tr.fldr); }
			}
		} while(gettr(&tr));
	}
	return EXIT_SUCCESS;
}
