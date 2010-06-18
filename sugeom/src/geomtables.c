#include "par.h"
#include "cwp.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ST_COLS 3
#define SH_COLS 6

/****************** self documentation *************************/
char *sdoc[] = {
	"                                                          		   	     ",
	" GEOMTABLES				           			   	     ",
	"                                                          		  	     ",
	" geomtables (nst=first,last)+ (recxst=first,last)+ (recyst=first,last)+   	     ",
	"	     (nsh=first,last)+ (nstsh=first,last)+ (shpatt=)+			     ",
	"	     (pattid=)+ (nchan=)+ (orig=)+ (chans=v1,v2,...)+ (pattsts=v1,v2,...)+   ",
	"	     outdir= outfile=							     ",
	"                                                          			     ",
	" Ex: geomtables nst=1,100 recxst=1,100 recyst=1,100 nst=101,200 	     	     ",
	"     recxst=101,200 recyst=101,200 nsh=1,100 nstsh=1,100 shpatt=1         	     ",
        "     nsh=101,200 nstsh=101,200 shpatt=1 pattid=1 nchan=10 orig=1 	    	     ",
        "     chans=1,10 pattsts=2,11						      	     ",
	"										     ",
	" Required Parameters:					    			     ",
	"										     ",
	" STATION PARAMETERS                                                  		     ",
	"                                                          			     ",
	" nst = first and last station numbers						     ",
	" recxst = first and last receiver x coordinates		 	             ",
	" recyst = first and last receiver y coordinates		 	             ",
	"                                                          			     ",
	" SHOT PARAMETERS                                                  		     ",
	"                                                          			     ",
	" nsh = first and last shot numbers						     ",
	" nstsh = first and last shot stations				 	             ",
	" shpatt = pattern id of the shot				 	             ",
	"                                                          			     ",
	" PATTERN PARAMETERS                                                  		     ",
	"                                                          			     ",
	" pattid = pattern id								     ",
	" nchan = number of channels					 	             ",
	" orig = shot origin, within the pattern			 	             ",
	" chans = list of channel intervals				 	             ",
	" pattsts = list of station intervals, relative to the list above             	     ",
	"                                                          			     ",
	" OUTPUT PARAMETERS                                           			     ",
	"                                                          			     ",
	" outdir = output directory                                   			     ",
	" outfile = output prefix                                   			     ",
	"                                                          			     ",
	NULL};

/* Credits:
 * Adriano Wagner (LAGEP-UFBA)
 *
 */
/**************** end self doc *********************************/

/*get the number of values of a key*/
int get_n(char *key, int lim){
	int end = 0,n=1;
	while(!end){
		int aux = countnparval(n,key);
		if(aux > 0){
			/*check if the number of values is exactly 2*/
			if(aux != lim){
				err("The key %s must have exactly %d values",key,lim);
			}
			n++;
		}
		else{
			end = 1;
		}
	}
	return n-1;
}

/*get minimum and maximum values of a key*/
void get_min_max(int n,int *min,int *max,char *key){
	int i, par[2];

	getnparint(1,key,par);
	*min = MIN(par[0],par[1]);
	*max = MAX(par[0],par[1]);

	for(i=2;i <= n; i++){
		getnparint(i,key,par);
		if(MIN(par[0],par[1]) < *min){
			*min = MIN(par[0],par[1]);
		}
		else if(MAX(par[0],par[1]) > *max){
			*max = MAX(par[0],par[1]);
		}
	}

	if(*min <= 0 || *max <= 0){
		err("Station and shot numbers must be positive");
	}
}

/*construct the stations table*/
float **calc_stations(int *min, int *max){
	int nstations = get_n("nst",2);
	float **stations;
	int i,j;
	int nst_par[2];
	float recx_par[2],recy_par[2];
	int diff;
	float incrx,incry;

	get_min_max(nstations,min,max,"nst");

	stations = alloc2float(*max-*min+1,ST_COLS);

	/*initialize nstation with -1*/
	for(i=0;i<=*max-*min;i++){
		stations[0][i] = -1;
	}

	for(i = 1; i <= nstations; i++){
		/*get parameters*/
		getnparint(i,"nst",nst_par);
		getnparfloat(i,"recxst",recx_par);
		getnparfloat(i,"recyst",recy_par);

		/*calculates the number of stations of the interval*/
		if(nst_par[1] - nst_par[0] == 0){
			diff = 1;
		}
		else{
			diff = nst_par[1] - nst_par[0];
		}

		/*calculates the increment of recx and recy*/
		incrx = (recx_par[1] - recx_par[0])/(diff*1.0);
		incry = (recy_par[1] - recy_par[0])/(diff*1.0);

		/*fills the table*/
		for(j = nst_par[0]; j <= nst_par[1]; j++){
			stations[0][j-*min] = j;
			stations[1][j-*min] = recx_par[0] + (j-nst_par[0])*incrx;
			stations[2][j-*min] = recy_par[0] + (j-nst_par[0])*incry;
		}
	}
	return stations;
}

float **calc_shots(int *min, int *max, float **stations,int stmin){
	int nshots = get_n("nsh",2);
	float **shots;
	int i,j;
	int nstsh_par[2],nsh_par[2],shpatt;
	int diff, incstsh;

	get_min_max(nshots,min,max,"nsh");

	shots = alloc2float(*max-*min+1,SH_COLS);

	for(i=0;i<=*max-*min;i++){
		shots[0][i] = -1;
	}

	for(i = 1; i <= nshots; i++){
		/*get parameters*/
		getnparint(i,"nsh",nsh_par);
		getnparint(i,"nstsh",nstsh_par);
		getnparint(i,"shpatt",&shpatt);
		
		/*calculates the number of shots of the interval*/
		if(nsh_par[1] - nsh_par[0] == 0){
			diff = 1;
		}
		else{
			diff = nsh_par[1] - nsh_par[0];
		}

		incstsh = (nstsh_par[1] - nstsh_par[0])/(diff*1.0);

		/*fills the table*/
		for(j = nsh_par[0]; j <= nsh_par[1]; j++){
			shots[0][j-*min] = j;
			shots[1][j-*min] = nstsh_par[0] + (j-nsh_par[0])*incstsh;
			shots[2][j-*min] = shpatt;
			shots[3][j-*min] = nstsh_par[0] + (j-nsh_par[0])*incstsh;
			shots[4][j-*min] = stations[1][((int)shots[3][j-*min])-stmin];
			shots[5][j-*min] = stations[2][((int)shots[3][j-*min])-stmin];
		}
	}
	return shots;
}

void save_patterns(char *outdir,char *prefix){
	int npatterns = get_n("pattid",1),npairs;
	char *file = malloc((strlen(outdir)+strlen(prefix)+15)*sizeof(char));
	FILE *fp;
	int i,j;
	int pattid,nchan,orig;
	int *chan_values;
	int *st_values;

	sprintf(file,"%s/%s-pattern.txt",outdir,prefix);

	fp = efopen(file,"w");
	for(i=1;i<=npatterns;i++){
		getnparint(i,"pattid",&pattid);
		getnparint(i,"nchan",&nchan);
		getnparint(i,"orig",&orig);
		fprintf(fp,"%d %d %d",pattid,nchan,orig);
		npairs = countnparval(i,"chans");
		/*checks if there is a station for every channel*/
		if(npairs != countnparval(i,"chans")){
			/*trata erro*/
			char *command = malloc((strlen(file)+4)*sizeof(char));
			sprintf(command,"rm %s",file);
			efclose(fp);
			system(command);
			free(command);
			err("You must specify a correspondent station for every channel");
		}
		chan_values = alloc1int(npairs);
		st_values = alloc1int(npairs);
		
		getnparint(i,"chans",chan_values);
		getnparint(i,"pattsts",st_values);

		for(j=0;j<npairs;j++){
			fprintf(fp," %d %d",chan_values[j],st_values[j]);
		}
		fprintf(fp,"\n");

		free1int(chan_values);
		free1int(st_values);
	}
	free(file);
	efclose(fp);
}

void save_stations(float **stations,int min_st,int max_st,char *outdir,char *prefix){
	int i;
	char *file = malloc((strlen(outdir)+strlen(prefix)+15)*sizeof(char));
	FILE *fp;

	sprintf(file,"%s/%s-station.txt",outdir,prefix);

	fp = efopen(file,"w");
	for(i = 0; i <= max_st-min_st; i++){
		fprintf(fp,"%d %f %f\n",(int)stations[0][i],stations[1][i],stations[2][i]);
	}
	efclose(fp);
	return;
}

void save_shots(float **shots,int min_sh,int max_sh,char *outdir,char *prefix){
	int i;
	char *file = malloc((strlen(outdir)+strlen(prefix)+15)*sizeof(char));
	FILE *fp;

	sprintf(file,"%s/%s-shot.txt",outdir,prefix);

	fp = efopen(file,"w");
	for(i = 0; i <= max_sh-min_sh; i++){
		fprintf(fp,"%d %d %d %d %f %f\n",(int)shots[0][i],(int)shots[1][i],(int)shots[2][i],(int)shots[3][i],shots[4][i],shots[5][i]);
	}
	efclose(fp);
	return;
}

int main(int argc, char **argv)
{
	float **table_stations, **table_shots;
	int min_st,max_st;
	int min_sh,max_sh;
	char *prefix,*outdir;

	initargs(argc, argv);
	requestdoc(1);

	MUSTGETPARSTRING("outfile",&prefix);
	MUSTGETPARSTRING("outdir",&outdir);

	table_stations = calc_stations(&min_st,&max_st);
	table_shots = calc_shots(&min_sh,&max_sh,table_stations,min_st);

	save_patterns(outdir,prefix);
	save_stations(table_stations,min_st,max_st,outdir,prefix);
	save_shots(table_shots,min_sh,max_sh,outdir,prefix);

	return EXIT_SUCCESS;
}
