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

#ifndef PARSER_H_
#define PARSER_H_

#include <stdio.h>
#include <stdlib.h>

/*struct que armazena as informacoes de uma estacao*/
typedef struct{
	int station;
	double recx, recy;
} StationVal;

/*struct que armazena as informacoes de um tiro*/
typedef struct{
	int nshot;
	int nstation;
	int npattern;
	double recx, recy;
} ShotVal;

void load_stations(FILE *file, StationVal **station_array);
int load_pattern(FILE *file,int id,int station,int **stations);
ShotVal *return_shot(FILE *file, int n);
StationVal *return_station(StationVal *station_array, int nchan);
#endif
