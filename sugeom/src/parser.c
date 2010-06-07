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

#include "parser.h"
#include "su.h"

#define LENGTH 200

/*retorna uma linha da tabela de um dos arquivos*/
char *return_line(FILE *file, int linha){
	int i;
	char *line = malloc(sizeof(char)*LENGTH);
	for(i=0;i<=linha-1;i++){
		fgets(line,LENGTH,file);
	}
	
	rewind(file);
	return line;
}

/*conta o numero de linhas de um arquivo*/
int count_lines(FILE *file){
	int i;
	char line[LENGTH];
	for(i = 0; fgets(line,LENGTH,file);i++);
	rewind(file);
	return i;
}	

/*valida um pattern*/
int check_pattern (FILE *file,int id, int *data){
	char c;
	char buf[8];
	int i,j,count=0;
	int acm=0;
	int error=FALSE;
	int find=FALSE,_id;
	rewind(file);
	do {
		fscanf(file,"%d",&_id);
		if (_id == id) 
			find=TRUE;
		else
			while(((c = getc(file)) != EOF)&&(c != '\n'));
	} while ((c != EOF)&&(find==FALSE));

	/*checa se o pattern existe*/
	if(find)
		getc(file);
	else {
		rewind(file);
		printf("Unknown ID\n");
		return find;
	}

	for(i=0,j=0,c=getc(file);((c != EOF)&&(c != '\n'));c=getc(file),i++,j++){
		if(c!=' ')
			buf[j]=c;
		else{
			if (j!=0){
				buf[j]='\0';
				sscanf(buf,"%d",&data[count]);
				count++;
			}
			j=-1;
		}
	}
	if (j!=0){
		buf[j]='\0';
		sscanf(buf,"%d",&data[count]);
		count++;
	}
	data[count]=-1;
	
	/*checa se o pattern esta coerente*/
	for(i=2;(i<count)&&(!error);i+=4){
		if (i+2 < count){
			acm+=data[i+2]-data[i]+1;
			if ((data[i+1]-data[i])!=(data[i+3]-data[i+2]))
				error=TRUE;
			if((data[i]>data[0])||(data[i+2]>data[0])||(data[i]<1)||(data[i+2]<1))
				error=TRUE;		
		}
		else{
			acm++;
			if(data[i]>data[0])
				error=TRUE;
		}		
	}
	if(acm!=data[0])
		error=TRUE;

	if (error) 
		printf("Invalid Pattern\n");

	return !error;
}

/*carrega as estacoes de um pattern em um vetor*/
int load_pattern(FILE *file,int id,int station,int **stations){
	int i,j,count=0;
	int dif;
	int plus;
	int st=0;
	int data[53];

	if(check_pattern(file,id,data)){
		*stations=malloc(data[0]*sizeof(int));
		for(i=0;data[i]!=-1;i++)
			count++;

		for(i=2;i<count;i+=4){
			if (i+2 < count){
				dif=data[i+2]-data[i];
				plus=data[i+1]-data[i];
				for(j=1;j<=dif+1;j++,st++)
					(*stations)[st]=st-data[1]+station+plus+1;
			}
			else {
				plus=data[i+1]-data[i];
				(*stations)[st]=st-data[1]+station+plus+1;
			}
		}
		return TRUE;
	}
	else
		return FALSE;
}

/*carrega as estcoes do arquivo em um vetor*/
void load_stations(FILE *file, StationVal **station_array){
	int i,j,k;
	char *line = malloc(sizeof(char)*LENGTH);
	j = count_lines(file);
	*station_array = malloc(j*sizeof(StationVal));
	for(i=0;i<j;i++){
		line = return_line(file,i+1);

		sscanf(&line[0],"%d",&((*station_array)[i].station));

		for(k=0; line[k]!=' '; k++);
		sscanf(&line[k],"%lf",&((*station_array)[i].recx));

		for(k++; line[k]!=' '; k++);
		sscanf(&line[k],"%lf",&((*station_array)[i].recy));
	}
	free(line);
}

/*retorna um tiro, lido do arquivo de tiros*/
ShotVal *return_shot(FILE *file, int n){
	ShotVal *val = malloc(sizeof(ShotVal));
	char *line = malloc(sizeof(char)*LENGTH);
	int k = 0;
	if (n > count_lines(file))
		return NULL;
	else{
		line = return_line(file,n);
		
		sscanf(&line[k],"%d",&(val->nshot));
		for(k=0; line[k]!=' '; k++);
		sscanf(&line[k],"%d",&(val->nstation));
		for(k++; line[k]!=' '; k++);
		sscanf(&line[k],"%d",&(val->npattern));
		for(k++; line[k]!=' '; k++);
		for(k++; line[k]!=' '; k++);
		sscanf(&line[k],"%lf",&(val->recx));
		for(k++; line[k]!=' '; k++);
		sscanf(&line[k],"%lf",&(val->recy));
		free(line);
		return val;
	}
}

/*retorna uma estacao de acordo com o numero do canal*/		
StationVal *return_station(StationVal *station_array, int nchan){
	StationVal *val = malloc(sizeof(StationVal));
	int i=nchan-station_array[0].station;
	int j;
	
	if (station_array[i].station == nchan){
		val = &station_array[i];
		return val;
	}
	else if (station_array[i].station > nchan){
		for(j=i-1;j > -1;j--){
			if(station_array[j].station == nchan){
				val = &station_array[j];
				return val;
			}
		}
		return NULL;
	}
	else return NULL;
}		
	

	
		
