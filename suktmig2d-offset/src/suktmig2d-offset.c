/*   suktmig2d-offset - Prestack time migration of a common-offset section
 *   Copyright (C) 2010 Reynam Pestana CPGG/UFBA <reynam@cpgg.ufba.br>
 *   
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *   
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *   
 *   You should have received a copy of the GNU General Public License
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * This program is based on source code of program suktmig2d
 * distributed in Seismic Un*x package.
 * Copyright (c) Colorado School of Mines.
 */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

char *sdoc[] = {
" 									",
" SUKTMIG2D-OFFSET - prestack time migration of a common-offset section ",
"		     with the double-square root (DSR) operator	        ",
" 									",
"  suktmig2d-offset < infile vfile= [parameters]  > outfile	        ",
"  						                        ",
" 									",
" Required Parameters:							",
" vfile=	rms velocity file (units/s) v(t,x) as a function of time",
" dx=		distance (units) between consecutive traces		",
" nvelcdp       number of consecutive cdps in velocity file 	        ",
" firstcdp      first cdp number in velocity file			",
" lastcdp  	last cdp number in velocity file			",
" offmin=	minimum offset						",
" offmax=	maximum offset						",
"									",
" Optional parameters:							",
" intoff= 	interval between offsets				",
" angmax=40	maximum aperture angle for migration (degrees)		",
" nfc=16	number of Fourier-coefficients to approximate low-pass	",
"		filters. The larger nfc the narrower the filter		",
" fwidth=5 	high-end frequency increment for the low-pass filters	",
" 		in Hz. The lower this number the more the number of	",
"		lowpass filters to be calculated for each input trace.	",
"									",
" Notes:								",
" Data must be preprocessed with sufrac to correct for the wave-shaping	",
" factor using phasefac=.25 for 2D migration.				",
"									",
" Input traces must be sorted into offset and cdp number. The velocity	",
" file consists of rms velocities for all CMPs as a function of vertical",
" time and horizontal position v(t,z) in C-style binary floating point	",
" numbers. It's easiest to supply v(t,z) that has the same dimensions as",
" the input data to be migrated.					",
"									",
" The units may be feet or meters, as long as these are consistent for	",
" Antialias filter is performed using (Gray,1992, Geoph. Prosp), using	",
" nc low- pass filtered copies of the data. The cutoff frequencies are	",
" calculated  as fractions of the Nyquist frequency.			",
"									",
" The maximum allowed angle is 80 degrees(a 10 degree taper is applied	",
" to the end of the aperture)						",
NULL};

/*
 * Credits:   
 * Serial Version: CWP, Baoniu Han, bhan@dix.mines.edu, April 19th, 1998
 * MPI Version: CPGG/UFBA, Reynam Pestana, reynam@cpgg.ufba.br 
 *
 * Trace header fields accessed: ns, dt, delrt, d2
 * Trace header fields modified: ns, dt, delrt
 */

/**************** end self doc *******************************************/

/* Prototypes for functions used internally */
#define LOOKFAC 2       /* Look ahead factor for npfaro   */
#define PFA_MAX 720720  /* Largest allowed nfft           */


/* Prototype of functions used internally */
void lpfilt(int nfc, int nfft, float dt, float fhi, float *filter);

void migkt2d ( float **data, int ntr, int nt, float dx, float dt, float tmax, 
               int nfft, int fnyq, float h, float *fc, int nf, int nc,int nfc, 
               int cdp_trace_first, float angmax, float **vel, float **mig, int dxcdp, int firstcdp);    

segy intrace; 	/* input traces */
segy outtrace;	/* migrated output traces */

int main(int argc,char **argv){
	int gottrace=1;	
	int i,it,ix;	/* counters */
	int ntr=0,nt;			/* x,t */

	int verbose=0;	/* is verbose?				*/
	int nc;		/* number of low-pass filtered versions	*/
			/*  of the data for antialiasing	*/
	int nfft,nf;	/* number of frequencies		*/
	int nfc;	/* number of Fourier coefficients for low-pass filter */
	int fwidth;	/* high-end frequency increment for the low-pass */
			/* filters 				*/
	int firstcdp=0;	/* first cdp in velocity file		*/
	int lastcdp=0;	/* last cdp in velocity file		*/

	int cdp_trace_first=0;	/* first cdp in data		*/
	int cdp_trace_last=0;	/* last cdp in data		*/
	int nvelcdp;     /* number of cdps in the velocity file */  

	int offmin,offmax;
	int intoff;
	int lastoffset=0;
	
	int oldoffset=0, curoffset=0;
	int noffset=0;
	int oldcdp=0;	/* temporary storage			*/
	int olddeltacdp=0;
	int deltacdp=0;
	int ncdp=0;	/* number of cdps in the velocity file	*/

	float dx=0.0;	/* cdp sample interval */
			/* no aliasing of the operator */
	float dt;	/* t sample interval */
	float h=0.0;	/* offset */

	float angmax;   /* maximum aperture angle */

	float tmax;	/* maximum time */

	float fnyq;	/* Nyquist frequency */
  
	float *fc=NULL;		/* cut-frequencies for low-pass filters */

	float **vel=NULL;	/* array of velocity values from vfile */
	float **data=NULL;	/* input data array*/
	float **mig=NULL;	/* output migrated data array */

	/* file names */
	char *vfile="";		/* name of velocity file */
	FILE *vfp=NULL;
	FILE *tracefp=NULL;	/* temp file to hold traces*/
	FILE *hfp=NULL;		/* temp file to hold trace headers */

	cwp_Bool check_cdp=cwp_false;	/* check cdp in velocity file	*/	
	
	initargs(argc, argv);
	requestdoc(1);
	
	MUSTGETPARFLOAT("dx",&dx);
	MUSTGETPARINT("nvelcdp",&nvelcdp);
	MUSTGETPARINT("firstcdp",&firstcdp);
	MUSTGETPARINT("lastcdp",&lastcdp); 
	MUSTGETPARINT("offmin",&offmin);
	MUSTGETPARINT("offmax",&offmax);
	MUSTGETPARSTRING("vfile",&vfile);

	if (!getparfloat("angmax",&angmax)) angmax=40;
	if (!getparint("nfc",&nfc)) nfc=16;
	if (!getparint("fwidth",&fwidth)) fwidth=5;
	if (!getparint("verbose",&verbose)) verbose=0;
	if (!getparint("intoff",&intoff)) intoff=0;

	if (!gettr(&intrace))  err("can't get first trace");
	nt=intrace.ns;
	dt=(float)intrace.dt/1000000;
	tmax=(nt-1)*dt;		
	gottrace=1;

	/* Set up FFT parameters */
	nfft = npfaro(nt, LOOKFAC*nt);

	if(nfft>= SU_NFLTS || nfft >= PFA_MAX)
	  err("Padded nt=%d -- too big",nfft);

	nf = nfft/2 + 1;

	/* Determine number of filters for antialiasing */
	fnyq= 1.0/(2*dt);
	nc=ceil(fnyq/fwidth);
	if (verbose)
		warn(" The number of filters for antialiasing is nc= %d",nc);

	vel =   alloc2float(nt,nvelcdp);
	fc  =	alloc1float(nc+1);	

	for(i=1; i<nc+1; ++i){
		fc[i]= fnyq*i/nc;
	}

	/* Zero all arrays */
/*	memset((void *) mig[0], 0,nt*ntr*FSIZE);*/

	vfp=efopen(vfile,"r");
	efread(vel[0],FSIZE,nt*nvelcdp,vfp);
	efclose(vfp);

	warn("Vel file readed");

	for(;gottrace==1;){
		tracefp = etmpfile();
		hfp = etmpfile();
		ntr = 0;

		curoffset=intrace.offset;
		do {
			oldoffset = curoffset;
			curoffset = intrace.offset;
			if(oldoffset!=curoffset){
				if(curoffset>offmax){
					gottrace=2;
					break;
				}
				if((noffset!=0)&&(ntr>0))
					break;
			}
			if (curoffset >= offmin){
				if(ntr==0){
					if(noffset>0){
						if(curoffset != (lastoffset+intoff)){
							if(!gettr(&intrace)){
								gottrace=2;
								break;
							}
							continue;
						}
						else{
							lastoffset=intrace.offset;
							noffset++;
							cdp_trace_first = intrace.cdp;
							h=.5*intrace.offset;
						}
						
					}
					else{
						lastoffset=intrace.offset;
						noffset++;
						cdp_trace_first = intrace.cdp;
						h=.5*intrace.offset;
					}
				}
				++ntr;
				/* get new deltacdp value */
				deltacdp=intrace.cdp-oldcdp;
				/* read headers and data */
				efwrite(&intrace,HDRBYTES, 1, hfp);
				efwrite(intrace.data, FSIZE, nt, tracefp);
					
				/* error trappings. */
				/* ...did cdp value interval change? */
				if ((ntr>3) && (olddeltacdp!=deltacdp)) {
					if (verbose) {
						warn("cdp interval changed in data");	
						warn("ntr=%d olddeltacdp=%d deltacdp=%d",ntr,olddeltacdp,deltacdp);
					 	check_cdp=cwp_true;
					}
				}
		
				/* save cdp and deltacdp values */
				oldcdp=intrace.cdp;
				olddeltacdp=deltacdp;
	
				cdp_trace_last = oldcdp;
	
			}
			if(!gettr(&intrace))
				gottrace=2;
		}
		while (gottrace!=2);

		if(noffset==1){
			if (intoff==0)
				intoff = curoffset - oldoffset;
			else
				if (((curoffset - oldoffset)>intoff)||((intoff%(curoffset-oldoffset))!=0)){
					gottrace = 0;
					warn("Invalid interval");
					break;
				}
		}

		if(ntr==0){
			gottrace = 0;
			warn ("Finishing Process");
			break;
		}
		
		ncdp=cdp_trace_last-cdp_trace_first+1;
		
		/* error trappings */
		if ( (cdp_trace_first==cdp_trace_last) 
			|| (deltacdp==0) 
			|| (check_cdp==cwp_true) ) warn("Check cdp values in data!");

		erewind(tracefp);
		erewind(hfp);

		if ( (ncdp > nvelcdp) )
			warn("Check ncdp values in data great than ncdpvel ");

		data = 	alloc2float(nt,ntr);
		for (ix=0; ix<ntr; ++ix)
			efread(data[ix],FSIZE,nt,tracefp);

		mig = alloc2float(nt,ntr);

		warn("Offset %d",oldoffset);
		
		migkt2d ( data, ntr, nt, dx, dt, tmax, nfft, fnyq, h, fc, nf, nc, nfc,
      		        cdp_trace_first, angmax, vel, mig, deltacdp, firstcdp);

		for (ix=0; ix<ntr; ++ix) {
			efread(&outtrace, HDRBYTES, 1, hfp);
			for (it=0; it<nt; ++it) {
				outtrace.data[it] = mig[ix][it];
			}
			puttr(&outtrace);
		}
		efclose(hfp);
		free2float(mig);

		free2float(data);

		if(gottrace==2){
			warn("Finishing Process");
			gottrace=0;
			break;
		}
	}

	return(CWP_Exit());	
}


void  migkt2d ( float **data, int ntr, int nt, float dx, float dt, float tmax, 
               int nfft, int fnyq, float h, float *fc, int nf, int nc, int nfc, 
               int cdp_trace_first,float angmax, float **vel,float **mig, int dxcdp, int firstcdp)   
{
	int k,imp,iip,it,ifc;	/* counters */

	float p=0.0;	/* horizontal slowness of the migration operator */
	float pmin=0.0;	/* maximum horizontal slowness for which there's */
			/* no aliasing of the operator */
	float x;	/* aperture distance */
	float xmax=0.0;	/* maximum aperture distance */

	float obliq;	/* obliquity factor */
	float geoms;	/* geometrical spreading factor */

	float mp,ip;	/* mid-point and image-point coordinates */
	float t;	/* time */
	float t0;	/* vertical traveltime */

	float ang;	/* aperture angle */
	float angtaper=0.0;	/* aperture-angle taper */
	float v;		/* velocity */
  
	float *filter=NULL;	/* array of low-pass filter values */

	float **lowpass=NULL;   /* low-pass filtered version of the trace */

	register float *rtin=NULL,*rtout=NULL;/* real traces */
	register complex *ct=NULL;   /* complex trace */

	float datalo[8], datahi[8];
	int itb, ite;
	float firstt, amplo, amphi;

	/* Allocate space */
	lowpass=alloc2float(nt,nc+1);
	rtin= ealloc1float(nfft);
	rtout= ealloc1float(nfft);
	ct= ealloc1complex(nf);
	filter= alloc1float(nf);

	/* Zero all arrays */
	memset((void *) rtin, 0, nfft*FSIZE);
	memset((void *) rtout, 0, nfft*FSIZE);
	memset((void *) filter, 0, nf*FSIZE);
	memset((void *) lowpass[0], 0,nt*(nc+1)*FSIZE);

	/* Start the migration process */
	/* Loop over input mid-points first */

	for(imp=0; imp<ntr; ++imp){
		float perc;

		mp=imp*dx; 
		perc=imp*100.0/(ntr-1);
/*		if(fmod(imp*100,ntr-1)==0)
			warn("migrated %g\n ",perc);*/

		/* Calculate low-pass filtered versions  */
		/* of the data to be used for antialiasing */
		for(it=0; it<nt; ++it){
			rtin[it]=data[imp][it];
		}
		for(ifc=1; ifc<nc+1; ++ifc){
			memset((void *) rtout, 0, nfft*FSIZE);
			memset((void *) ct, 0, nf*FSIZE);
			lpfilt(nfc,nfft,dt,fc[ifc],filter);
			pfarc(1,nfft,rtin,ct);

			for(it=0; it<nf; ++it){
				ct[it] = crmul(ct[it],filter[it]);
			}
			pfacr(-1,nfft,ct,rtout);
			for(it=0; it<nt; ++it){ 
				lowpass[ifc][it]= rtout[it]; 
			}
		}

		
		/* Loop over vertical traveltimes */
		for(it=0; it<nt; ++it){
			int lx,ux;

			t0=it*dt;
			v=vel[imp*dxcdp-firstcdp+cdp_trace_first][it];
/*			v=vel[imp][it];*/
			xmax=tan((angmax+10.0)*PI/180.0)*v*t0;
			lx=MAX(0,imp - ceil(xmax/dx)); 
			ux=MIN(ntr,imp + ceil(xmax/dx));
	
		
		/* loop over output image-points to the left of the midpoint */
		for(iip=imp; iip>lx; --iip){
			float ts,tr;
			int fplo=0, fphi=0;
			float ref,wlo,whi;

			ip=iip*dx; 
			x=ip-mp; 
			ts=sqrt( pow(t0/2,2) + pow((x+h)/v,2) );
			tr=sqrt( pow(t0/2,2) + pow((h-x)/v,2) );
			t= ts + tr;
			if(t>=tmax) break;
			geoms=sqrt(1/(t*v));
	  		obliq=sqrt(.5*(1 + (t0*t0/(4*ts*tr)) 
					- (1/(ts*tr))*sqrt(ts*ts - t0*t0/4)*sqrt(tr*tr - t0*t0/4)));
	  		ang=180.0*fabs(acos(t0/t))/PI;  
	  		if(ang<=angmax) angtaper=1.0;
	  		if(ang>angmax) angtaper=cos((ang-angmax)*PI/20);
	  		/* Evaluate migration operator slowness p to determine */
			/* the low-pass filtered trace for antialiasing */
			pmin=1/(2*dx*fnyq);
			p=fabs((x+h)/(pow(v,2)*ts) + (x-h)/(pow(v,2)*tr));
				if(p>0){fplo=floor(nc*pmin/p);}
				if(p==0){fplo=nc;}
				ref=fmod(nc*pmin,p);
				wlo=1-ref;
				fphi=++fplo;
				whi=ref;
				itb=MAX(ceil(t/dt)-3,0);
				ite=MIN(itb+8,nt);
				firstt=(itb-1)*dt;
				/* Move energy from CMP to CIP */
				if(fplo>=nc){
					for(k=itb; k<ite; ++k){
						datalo[k-itb]=lowpass[nc][k];
					}
					ints8r(8,dt,firstt,datalo,0.0,0.0,1,&t,&amplo);
					mig[iip][it] +=geoms*obliq*angtaper*amplo;
				} else if(fplo<nc){
					for(k=itb; k<ite; ++k){
						datalo[k-itb]=lowpass[fplo][k];
						datahi[k-itb]=lowpass[fphi][k];
					}
					ints8r(8,dt,firstt,datalo,0.0,0.0,1,&t,&amplo);
					ints8r(8,dt,firstt,datahi,0.0,0.0,1,&t,&amphi);
					mig[iip][it] += geoms*obliq*angtaper*(wlo*amplo + whi*amphi);
				}
			}

			/* loop over output image-points to the right of the midpoint */
			for(iip=imp+1; iip<ux; ++iip){
				float ts,tr;
				int fplo=0, fphi;
				float ref,wlo,whi;

				ip=iip*dx; 
				x=ip-mp; 
				t0=it*dt;	  
				ts=sqrt( pow(t0/2,2) + pow((x+h)/v,2) );
				tr=sqrt( pow(t0/2,2) + pow((h-x)/v,2) );
				t= ts + tr;
				if(t>=tmax) break;
				geoms=sqrt(1/(t*v));
				obliq=sqrt(.5*(1 + (t0*t0/(4*ts*tr)) 
					- (1/(ts*tr))*sqrt(ts*ts 
						- t0*t0/4)*sqrt(tr*tr 
								- t0*t0/4)));
				ang=180.0*fabs(acos(t0/t))/PI;   
				if(ang<=angmax) angtaper=1.0;
				if(ang>angmax) angtaper=cos((ang-angmax)*PI/20.0);

				/* Evaluate migration operator slowness p to determine the  */
				/* low-pass filtered trace for antialiasing */
				pmin=1/(2*dx*fnyq);
				p=fabs((x+h)/(pow(v,2)*ts) + (x-h)/(pow(v,2)*tr));
				if(p>0){
					fplo=floor(nc*pmin/p);
				}
				if(p==0){
					fplo=nc;
				}

				ref=fmod(nc*pmin,p);
				wlo=1-ref;
				fphi=fplo+1;
				whi=ref;
				itb=MAX(ceil(t/dt)-3,0);
				ite=MIN(itb+8,nt);
				firstt=(itb-1)*dt;

				/* Move energy from CMP to CIP */
				if(fplo>=nc){
					for(k=itb; k<ite; ++k){
						datalo[k-itb]=lowpass[nc][k];
					}
					ints8r(8,dt,firstt,datalo,0.0,0.0,1,&t,&amplo);
					mig[iip][it] +=geoms*obliq*angtaper*amplo;
				} else if(fplo<nc){
					for(k=itb; k<ite; ++k){
						datalo[k-itb]=lowpass[fplo][k];
						datahi[k-itb]=lowpass[fphi][k];
					}
					ints8r(8,dt,firstt,datalo,0.0,0.0,1,&t,&amplo);
					ints8r(8,dt,firstt,datahi,0.0,0.0,1,&t,&amphi);
					mig[iip][it] += geoms*obliq*angtaper*(wlo*amplo + whi*amphi);
				}
			}
		}
	
	} 
}

void
lpfilt(int nfc, int nfft, float dt, float fhi, float *filter)
/*******************************************************************************
lpfilt -- low-pass filter using Lanczos Smoothing 
	(R.W. Hamming:"Digital Filtering",1977)
****************************************************************************
Input: 
nfc	number of Fourier coefficients to approximate ideal filter
nfft	number of points in the fft
dt	time sampling interval
fhi	cut-frequency

Output:
filter  array[nf] of filter values
*****************************************************************************
Notes: Filter is to be applied in the frequency domain   
*****************************************************************************
Author: CWP: Carlos Pacheco   2006   
*****************************************************************************/
{
	int i,j;  /* counters */
	int nf;   /* Number of frequencies (including Nyquist) */
	float onfft;  /* reciprocal of nfft */
	float fn; /* Nyquist frequency */
	float df; /* frequency interval */
	float dw; /* frequency interval in radians */
	float whi;/* cut-frequency in radians */
	float w;  /* radian frequency */

	nf= nfft/2 + 1;
	onfft=1.0/nfft;
	fn=1.0/(2*dt);
	df=onfft/dt;
	whi=fhi*PI/fn;
	dw=df*PI/fn;

	for(i=0; i<nf; ++i){
		filter[i]= whi/PI;
		w=i*dw;

		for(j=1; j<nfc; ++j){
			float c= sin(whi*j)*sin(PI*j/nfc)*2*nfc/(PI*PI*j*j);
			filter[i] +=c*cos(j*w);
		}
	}
}

