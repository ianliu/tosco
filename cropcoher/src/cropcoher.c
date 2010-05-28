#include <stdlib.h>
#include <stdio.h>
#include <su.h>
#include <par.h>
#include <segy.h>

/*********************** self documentation **********************/
char *sdoc[]= {
"                                                                 ",
" CROPCOHER                                                       ",
"                                                                 ",
" Fornecida uma seção de coerência e um valor mínimo para a       ",
" coerência, retorna uma seção contendo 1 nos pontos onde a       ",
" coerência excede o valor mínimo e zero caso contrário.          ",
"                                                                 ",
" Parâmetros de entrada:                                          ",
"                                                                 ",
" seção de coerência : (stdin)                                    ",
" float cohermin     : valor mínimo para a coerência              ",
"                                                                 ",
"  Autor: Ricardo Biloti <biloti@mat.ufpr.br>                     ",
"         Professor Adjunto                                       ",
"         GCC - Grupo de Computação Científica                    ",
"         Departamento de Matemática                              ",
"         Universidade Federal do Paraná                          ",
"                                                                 ",
"  Versão: 28/10/2002                                             ",
"                                                                 ",
NULL};

/*
 * Author: Ricardo Biloti 28/10/2002.
 */
/**************** end self doc ********************************/ 

int main(int argc, char **argv){

  float cohermin;
  int i;
  segy tr;

  initargs(argc,argv);
  requestdoc(1);

  if(!getparfloat("cohermin",&cohermin))
    err("Especifique uma coherencia mínima.");

  while (gettr(&tr)){
    for (i=0; i<tr.ns; i++)
      if (tr.data[i] < cohermin)
	tr.data[i] = 0;
      else
	tr.data[i] = 1;

    puttr(&tr);
  }

  return EXIT_SUCCESS;
}

