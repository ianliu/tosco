/*
  File autogenerated by gengetopt version 2.22.5
  generated with the following command:
  gengetopt --file-name=cmdline 

  The developers of gengetopt consider the fixed text that goes in all
  gengetopt output files to be in the public domain:
  we make no copyright claims on it.
*/

/* If we use autoconf.  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef FIX_UNUSED
#define FIX_UNUSED(X) (void) (X) /* avoid warnings for unused params */
#endif

#include <getopt.h>

#include "cmdline.h"

const char *gengetopt_args_info_purpose = "Given a file lu1.dat (as stdin) from Seis88, it generates a\nGrace project file showing rays and interfaces.\n\nOnly --xmax parameter is mandatory.\n\nAdditionaly, it can export the velocity model sampled on a regular\ngrid of nx by nz samples, as well as, interfaces, with nx samples\neach.\n\nTo export the velocity model, the user has to provide a file\ncontaining top and bottom layer velocities. The lines of that file\nhave the top and bottom velocities for each layer, from the shallowest\nlayer to the deepest one. Such file is automatically generated by\nS88Modeling.\n\nTo export interfaces, provide a file through --interfaces parameter. A\nfile will be saved containing (x,z) pairs for each interface. A blank\nline marks the transition between different interface points.\n\nRicardo Biloti <biloti@ime.unicamp.br>\nDepartment of Applied Mathematics\nState University of Campinas";

const char *gengetopt_args_info_usage = "Usage: rays2 [OPTIONS]...";

const char *gengetopt_args_info_description = "";

const char *gengetopt_args_info_help[] = {
  "  -h, --help               Print help and exit",
  "  -V, --version            Print version and exit",
  "      --xmin=FLOAT         x left box coordinate",
  "      --zmin=FLOAT         z top box coordinate",
  "      --xmax=FLOAT         x right box coordinate",
  "      --zmax=FLOAT         z top box coordinate",
  "      --nx=INT             number samples in x  (default=`101')",
  "      --nz=INT             number samples in z  (default=`101')",
  "  -v, --vfile=STRING       file to export the velocity model",
  "      --layervel=STRING    input file with velocities at top and bottom of each \n                             layer  (default=`vel.rays2')",
  "  -i, --interfaces=STRING  file to export the interfaces",
  "      --nofill             turn off filling in of layers  (default=off)",
  "  -p, --palette=STRING     custom palette",
  "  -l, --land               land color profile  (default=off)",
  "  -b, --blackrays          rays in black  (default=off)",
  "      --norays             do not generate rays  (default=off)",
    0
};

typedef enum {ARG_NO
  , ARG_FLAG
  , ARG_STRING
  , ARG_INT
  , ARG_FLOAT
} cmdline_parser_arg_type;

static
void clear_given (struct gengetopt_args_info *args_info);
static
void clear_args (struct gengetopt_args_info *args_info);

static int
cmdline_parser_internal (int argc, char **argv, struct gengetopt_args_info *args_info,
                        struct cmdline_parser_params *params, const char *additional_error);

static int
cmdline_parser_required2 (struct gengetopt_args_info *args_info, const char *prog_name, const char *additional_error);

static char *
gengetopt_strdup (const char *s);

static
void clear_given (struct gengetopt_args_info *args_info)
{
  args_info->help_given = 0 ;
  args_info->version_given = 0 ;
  args_info->xmin_given = 0 ;
  args_info->zmin_given = 0 ;
  args_info->xmax_given = 0 ;
  args_info->zmax_given = 0 ;
  args_info->nx_given = 0 ;
  args_info->nz_given = 0 ;
  args_info->vfile_given = 0 ;
  args_info->layervel_given = 0 ;
  args_info->interfaces_given = 0 ;
  args_info->nofill_given = 0 ;
  args_info->palette_given = 0 ;
  args_info->land_given = 0 ;
  args_info->blackrays_given = 0 ;
  args_info->norays_given = 0 ;
}

static
void clear_args (struct gengetopt_args_info *args_info)
{
  FIX_UNUSED (args_info);
  args_info->xmin_orig = NULL;
  args_info->zmin_orig = NULL;
  args_info->xmax_orig = NULL;
  args_info->zmax_orig = NULL;
  args_info->nx_arg = 101;
  args_info->nx_orig = NULL;
  args_info->nz_arg = 101;
  args_info->nz_orig = NULL;
  args_info->vfile_arg = NULL;
  args_info->vfile_orig = NULL;
  args_info->layervel_arg = gengetopt_strdup ("vel.rays2");
  args_info->layervel_orig = NULL;
  args_info->interfaces_arg = NULL;
  args_info->interfaces_orig = NULL;
  args_info->nofill_flag = 0;
  args_info->palette_arg = NULL;
  args_info->palette_orig = NULL;
  args_info->land_flag = 0;
  args_info->blackrays_flag = 0;
  args_info->norays_flag = 0;
  
}

static
void init_args_info(struct gengetopt_args_info *args_info)
{


  args_info->help_help = gengetopt_args_info_help[0] ;
  args_info->version_help = gengetopt_args_info_help[1] ;
  args_info->xmin_help = gengetopt_args_info_help[2] ;
  args_info->zmin_help = gengetopt_args_info_help[3] ;
  args_info->xmax_help = gengetopt_args_info_help[4] ;
  args_info->zmax_help = gengetopt_args_info_help[5] ;
  args_info->nx_help = gengetopt_args_info_help[6] ;
  args_info->nz_help = gengetopt_args_info_help[7] ;
  args_info->vfile_help = gengetopt_args_info_help[8] ;
  args_info->layervel_help = gengetopt_args_info_help[9] ;
  args_info->interfaces_help = gengetopt_args_info_help[10] ;
  args_info->nofill_help = gengetopt_args_info_help[11] ;
  args_info->palette_help = gengetopt_args_info_help[12] ;
  args_info->land_help = gengetopt_args_info_help[13] ;
  args_info->blackrays_help = gengetopt_args_info_help[14] ;
  args_info->norays_help = gengetopt_args_info_help[15] ;
  
}

void
cmdline_parser_print_version (void)
{
  printf ("%s %s\n",
     (strlen(CMDLINE_PARSER_PACKAGE_NAME) ? CMDLINE_PARSER_PACKAGE_NAME : CMDLINE_PARSER_PACKAGE),
     CMDLINE_PARSER_VERSION);
}

static void print_help_common(void) {
  cmdline_parser_print_version ();

  if (strlen(gengetopt_args_info_purpose) > 0)
    printf("\n%s\n", gengetopt_args_info_purpose);

  if (strlen(gengetopt_args_info_usage) > 0)
    printf("\n%s\n", gengetopt_args_info_usage);

  printf("\n");

  if (strlen(gengetopt_args_info_description) > 0)
    printf("%s\n\n", gengetopt_args_info_description);
}

void
cmdline_parser_print_help (void)
{
  int i = 0;
  print_help_common();
  while (gengetopt_args_info_help[i])
    printf("%s\n", gengetopt_args_info_help[i++]);
}

void
cmdline_parser_init (struct gengetopt_args_info *args_info)
{
  clear_given (args_info);
  clear_args (args_info);
  init_args_info (args_info);
}

void
cmdline_parser_params_init(struct cmdline_parser_params *params)
{
  if (params)
    { 
      params->override = 0;
      params->initialize = 1;
      params->check_required = 1;
      params->check_ambiguity = 0;
      params->print_errors = 1;
    }
}

struct cmdline_parser_params *
cmdline_parser_params_create(void)
{
  struct cmdline_parser_params *params = 
    (struct cmdline_parser_params *)malloc(sizeof(struct cmdline_parser_params));
  cmdline_parser_params_init(params);  
  return params;
}

static void
free_string_field (char **s)
{
  if (*s)
    {
      free (*s);
      *s = 0;
    }
}


static void
cmdline_parser_release (struct gengetopt_args_info *args_info)
{

  free_string_field (&(args_info->xmin_orig));
  free_string_field (&(args_info->zmin_orig));
  free_string_field (&(args_info->xmax_orig));
  free_string_field (&(args_info->zmax_orig));
  free_string_field (&(args_info->nx_orig));
  free_string_field (&(args_info->nz_orig));
  free_string_field (&(args_info->vfile_arg));
  free_string_field (&(args_info->vfile_orig));
  free_string_field (&(args_info->layervel_arg));
  free_string_field (&(args_info->layervel_orig));
  free_string_field (&(args_info->interfaces_arg));
  free_string_field (&(args_info->interfaces_orig));
  free_string_field (&(args_info->palette_arg));
  free_string_field (&(args_info->palette_orig));
  
  

  clear_given (args_info);
}


static void
write_into_file(FILE *outfile, const char *opt, const char *arg, const char *values[])
{
  FIX_UNUSED (values);
  if (arg) {
    fprintf(outfile, "%s=\"%s\"\n", opt, arg);
  } else {
    fprintf(outfile, "%s\n", opt);
  }
}


int
cmdline_parser_dump(FILE *outfile, struct gengetopt_args_info *args_info)
{
  int i = 0;

  if (!outfile)
    {
      fprintf (stderr, "%s: cannot dump options to stream\n", CMDLINE_PARSER_PACKAGE);
      return EXIT_FAILURE;
    }

  if (args_info->help_given)
    write_into_file(outfile, "help", 0, 0 );
  if (args_info->version_given)
    write_into_file(outfile, "version", 0, 0 );
  if (args_info->xmin_given)
    write_into_file(outfile, "xmin", args_info->xmin_orig, 0);
  if (args_info->zmin_given)
    write_into_file(outfile, "zmin", args_info->zmin_orig, 0);
  if (args_info->xmax_given)
    write_into_file(outfile, "xmax", args_info->xmax_orig, 0);
  if (args_info->zmax_given)
    write_into_file(outfile, "zmax", args_info->zmax_orig, 0);
  if (args_info->nx_given)
    write_into_file(outfile, "nx", args_info->nx_orig, 0);
  if (args_info->nz_given)
    write_into_file(outfile, "nz", args_info->nz_orig, 0);
  if (args_info->vfile_given)
    write_into_file(outfile, "vfile", args_info->vfile_orig, 0);
  if (args_info->layervel_given)
    write_into_file(outfile, "layervel", args_info->layervel_orig, 0);
  if (args_info->interfaces_given)
    write_into_file(outfile, "interfaces", args_info->interfaces_orig, 0);
  if (args_info->nofill_given)
    write_into_file(outfile, "nofill", 0, 0 );
  if (args_info->palette_given)
    write_into_file(outfile, "palette", args_info->palette_orig, 0);
  if (args_info->land_given)
    write_into_file(outfile, "land", 0, 0 );
  if (args_info->blackrays_given)
    write_into_file(outfile, "blackrays", 0, 0 );
  if (args_info->norays_given)
    write_into_file(outfile, "norays", 0, 0 );
  

  i = EXIT_SUCCESS;
  return i;
}

int
cmdline_parser_file_save(const char *filename, struct gengetopt_args_info *args_info)
{
  FILE *outfile;
  int i = 0;

  outfile = fopen(filename, "w");

  if (!outfile)
    {
      fprintf (stderr, "%s: cannot open file for writing: %s\n", CMDLINE_PARSER_PACKAGE, filename);
      return EXIT_FAILURE;
    }

  i = cmdline_parser_dump(outfile, args_info);
  fclose (outfile);

  return i;
}

void
cmdline_parser_free (struct gengetopt_args_info *args_info)
{
  cmdline_parser_release (args_info);
}

/** @brief replacement of strdup, which is not standard */
char *
gengetopt_strdup (const char *s)
{
  char *result = 0;
  if (!s)
    return result;

  result = (char*)malloc(strlen(s) + 1);
  if (result == (char*)0)
    return (char*)0;
  strcpy(result, s);
  return result;
}

int
cmdline_parser (int argc, char **argv, struct gengetopt_args_info *args_info)
{
  return cmdline_parser2 (argc, argv, args_info, 0, 1, 1);
}

int
cmdline_parser_ext (int argc, char **argv, struct gengetopt_args_info *args_info,
                   struct cmdline_parser_params *params)
{
  int result;
  result = cmdline_parser_internal (argc, argv, args_info, params, 0);

  if (result == EXIT_FAILURE)
    {
      cmdline_parser_free (args_info);
      exit (EXIT_FAILURE);
    }
  
  return result;
}

int
cmdline_parser2 (int argc, char **argv, struct gengetopt_args_info *args_info, int override, int initialize, int check_required)
{
  int result;
  struct cmdline_parser_params params;
  
  params.override = override;
  params.initialize = initialize;
  params.check_required = check_required;
  params.check_ambiguity = 0;
  params.print_errors = 1;

  result = cmdline_parser_internal (argc, argv, args_info, &params, 0);

  if (result == EXIT_FAILURE)
    {
      cmdline_parser_free (args_info);
      exit (EXIT_FAILURE);
    }
  
  return result;
}

int
cmdline_parser_required (struct gengetopt_args_info *args_info, const char *prog_name)
{
  int result = EXIT_SUCCESS;

  if (cmdline_parser_required2(args_info, prog_name, 0) > 0)
    result = EXIT_FAILURE;

  if (result == EXIT_FAILURE)
    {
      cmdline_parser_free (args_info);
      exit (EXIT_FAILURE);
    }
  
  return result;
}

int
cmdline_parser_required2 (struct gengetopt_args_info *args_info, const char *prog_name, const char *additional_error)
{
  int error = 0;
  FIX_UNUSED (additional_error);

  /* checks for required options */
  if (! args_info->xmax_given)
    {
      fprintf (stderr, "%s: '--xmax' option required%s\n", prog_name, (additional_error ? additional_error : ""));
      error = 1;
    }
  
  
  /* checks for dependences among options */

  return error;
}


static char *package_name = 0;

/**
 * @brief updates an option
 * @param field the generic pointer to the field to update
 * @param orig_field the pointer to the orig field
 * @param field_given the pointer to the number of occurrence of this option
 * @param prev_given the pointer to the number of occurrence already seen
 * @param value the argument for this option (if null no arg was specified)
 * @param possible_values the possible values for this option (if specified)
 * @param default_value the default value (in case the option only accepts fixed values)
 * @param arg_type the type of this option
 * @param check_ambiguity @see cmdline_parser_params.check_ambiguity
 * @param override @see cmdline_parser_params.override
 * @param no_free whether to free a possible previous value
 * @param multiple_option whether this is a multiple option
 * @param long_opt the corresponding long option
 * @param short_opt the corresponding short option (or '-' if none)
 * @param additional_error possible further error specification
 */
static
int update_arg(void *field, char **orig_field,
               unsigned int *field_given, unsigned int *prev_given, 
               char *value, const char *possible_values[],
               const char *default_value,
               cmdline_parser_arg_type arg_type,
               int check_ambiguity, int override,
               int no_free, int multiple_option,
               const char *long_opt, char short_opt,
               const char *additional_error)
{
  char *stop_char = 0;
  const char *val = value;
  int found;
  char **string_field;
  FIX_UNUSED (field);

  stop_char = 0;
  found = 0;

  if (!multiple_option && prev_given && (*prev_given || (check_ambiguity && *field_given)))
    {
      if (short_opt != '-')
        fprintf (stderr, "%s: `--%s' (`-%c') option given more than once%s\n", 
               package_name, long_opt, short_opt,
               (additional_error ? additional_error : ""));
      else
        fprintf (stderr, "%s: `--%s' option given more than once%s\n", 
               package_name, long_opt,
               (additional_error ? additional_error : ""));
      return 1; /* failure */
    }

  FIX_UNUSED (default_value);
    
  if (field_given && *field_given && ! override)
    return 0;
  if (prev_given)
    (*prev_given)++;
  if (field_given)
    (*field_given)++;
  if (possible_values)
    val = possible_values[found];

  switch(arg_type) {
  case ARG_FLAG:
    *((int *)field) = !*((int *)field);
    break;
  case ARG_INT:
    if (val) *((int *)field) = strtol (val, &stop_char, 0);
    break;
  case ARG_FLOAT:
    if (val) *((float *)field) = (float)strtod (val, &stop_char);
    break;
  case ARG_STRING:
    if (val) {
      string_field = (char **)field;
      if (!no_free && *string_field)
        free (*string_field); /* free previous string */
      *string_field = gengetopt_strdup (val);
    }
    break;
  default:
    break;
  };

  /* check numeric conversion */
  switch(arg_type) {
  case ARG_INT:
  case ARG_FLOAT:
    if (val && !(stop_char && *stop_char == '\0')) {
      fprintf(stderr, "%s: invalid numeric value: %s\n", package_name, val);
      return 1; /* failure */
    }
    break;
  default:
    ;
  };

  /* store the original value */
  switch(arg_type) {
  case ARG_NO:
  case ARG_FLAG:
    break;
  default:
    if (value && orig_field) {
      if (no_free) {
        *orig_field = value;
      } else {
        if (*orig_field)
          free (*orig_field); /* free previous string */
        *orig_field = gengetopt_strdup (value);
      }
    }
  };

  return 0; /* OK */
}


int
cmdline_parser_internal (
  int argc, char **argv, struct gengetopt_args_info *args_info,
                        struct cmdline_parser_params *params, const char *additional_error)
{
  int c;	/* Character of the parsed option.  */

  int error = 0;
  struct gengetopt_args_info local_args_info;
  
  int override;
  int initialize;
  int check_required;
  int check_ambiguity;
  
  package_name = argv[0];
  
  override = params->override;
  initialize = params->initialize;
  check_required = params->check_required;
  check_ambiguity = params->check_ambiguity;

  if (initialize)
    cmdline_parser_init (args_info);

  cmdline_parser_init (&local_args_info);

  optarg = 0;
  optind = 0;
  opterr = params->print_errors;
  optopt = '?';

  while (1)
    {
      int option_index = 0;

      static struct option long_options[] = {
        { "help",	0, NULL, 'h' },
        { "version",	0, NULL, 'V' },
        { "xmin",	1, NULL, 0 },
        { "zmin",	1, NULL, 0 },
        { "xmax",	1, NULL, 0 },
        { "zmax",	1, NULL, 0 },
        { "nx",	1, NULL, 0 },
        { "nz",	1, NULL, 0 },
        { "vfile",	1, NULL, 'v' },
        { "layervel",	1, NULL, 0 },
        { "interfaces",	1, NULL, 'i' },
        { "nofill",	0, NULL, 0 },
        { "palette",	1, NULL, 'p' },
        { "land",	0, NULL, 'l' },
        { "blackrays",	0, NULL, 'b' },
        { "norays",	0, NULL, 0 },
        { 0,  0, 0, 0 }
      };

      c = getopt_long (argc, argv, "hVv:i:p:lb", long_options, &option_index);

      if (c == -1) break;	/* Exit from `while (1)' loop.  */

      switch (c)
        {
        case 'h':	/* Print help and exit.  */
          cmdline_parser_print_help ();
          cmdline_parser_free (&local_args_info);
          exit (EXIT_SUCCESS);

        case 'V':	/* Print version and exit.  */
          cmdline_parser_print_version ();
          cmdline_parser_free (&local_args_info);
          exit (EXIT_SUCCESS);

        case 'v':	/* file to export the velocity model.  */
        
        
          if (update_arg( (void *)&(args_info->vfile_arg), 
               &(args_info->vfile_orig), &(args_info->vfile_given),
              &(local_args_info.vfile_given), optarg, 0, 0, ARG_STRING,
              check_ambiguity, override, 0, 0,
              "vfile", 'v',
              additional_error))
            goto failure;
        
          break;
        case 'i':	/* file to export the interfaces.  */
        
        
          if (update_arg( (void *)&(args_info->interfaces_arg), 
               &(args_info->interfaces_orig), &(args_info->interfaces_given),
              &(local_args_info.interfaces_given), optarg, 0, 0, ARG_STRING,
              check_ambiguity, override, 0, 0,
              "interfaces", 'i',
              additional_error))
            goto failure;
        
          break;
        case 'p':	/* custom palette.  */
        
        
          if (update_arg( (void *)&(args_info->palette_arg), 
               &(args_info->palette_orig), &(args_info->palette_given),
              &(local_args_info.palette_given), optarg, 0, 0, ARG_STRING,
              check_ambiguity, override, 0, 0,
              "palette", 'p',
              additional_error))
            goto failure;
        
          break;
        case 'l':	/* land color profile.  */
        
        
          if (update_arg((void *)&(args_info->land_flag), 0, &(args_info->land_given),
              &(local_args_info.land_given), optarg, 0, 0, ARG_FLAG,
              check_ambiguity, override, 1, 0, "land", 'l',
              additional_error))
            goto failure;
        
          break;
        case 'b':	/* rays in black.  */
        
        
          if (update_arg((void *)&(args_info->blackrays_flag), 0, &(args_info->blackrays_given),
              &(local_args_info.blackrays_given), optarg, 0, 0, ARG_FLAG,
              check_ambiguity, override, 1, 0, "blackrays", 'b',
              additional_error))
            goto failure;
        
          break;

        case 0:	/* Long option with no short option */
          /* x left box coordinate.  */
          if (strcmp (long_options[option_index].name, "xmin") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->xmin_arg), 
                 &(args_info->xmin_orig), &(args_info->xmin_given),
                &(local_args_info.xmin_given), optarg, 0, 0, ARG_FLOAT,
                check_ambiguity, override, 0, 0,
                "xmin", '-',
                additional_error))
              goto failure;
          
          }
          /* z top box coordinate.  */
          else if (strcmp (long_options[option_index].name, "zmin") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->zmin_arg), 
                 &(args_info->zmin_orig), &(args_info->zmin_given),
                &(local_args_info.zmin_given), optarg, 0, 0, ARG_FLOAT,
                check_ambiguity, override, 0, 0,
                "zmin", '-',
                additional_error))
              goto failure;
          
          }
          /* x right box coordinate.  */
          else if (strcmp (long_options[option_index].name, "xmax") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->xmax_arg), 
                 &(args_info->xmax_orig), &(args_info->xmax_given),
                &(local_args_info.xmax_given), optarg, 0, 0, ARG_FLOAT,
                check_ambiguity, override, 0, 0,
                "xmax", '-',
                additional_error))
              goto failure;
          
          }
          /* z top box coordinate.  */
          else if (strcmp (long_options[option_index].name, "zmax") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->zmax_arg), 
                 &(args_info->zmax_orig), &(args_info->zmax_given),
                &(local_args_info.zmax_given), optarg, 0, 0, ARG_FLOAT,
                check_ambiguity, override, 0, 0,
                "zmax", '-',
                additional_error))
              goto failure;
          
          }
          /* number samples in x.  */
          else if (strcmp (long_options[option_index].name, "nx") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->nx_arg), 
                 &(args_info->nx_orig), &(args_info->nx_given),
                &(local_args_info.nx_given), optarg, 0, "101", ARG_INT,
                check_ambiguity, override, 0, 0,
                "nx", '-',
                additional_error))
              goto failure;
          
          }
          /* number samples in z.  */
          else if (strcmp (long_options[option_index].name, "nz") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->nz_arg), 
                 &(args_info->nz_orig), &(args_info->nz_given),
                &(local_args_info.nz_given), optarg, 0, "101", ARG_INT,
                check_ambiguity, override, 0, 0,
                "nz", '-',
                additional_error))
              goto failure;
          
          }
          /* input file with velocities at top and bottom of each layer.  */
          else if (strcmp (long_options[option_index].name, "layervel") == 0)
          {
          
          
            if (update_arg( (void *)&(args_info->layervel_arg), 
                 &(args_info->layervel_orig), &(args_info->layervel_given),
                &(local_args_info.layervel_given), optarg, 0, "vel.rays2", ARG_STRING,
                check_ambiguity, override, 0, 0,
                "layervel", '-',
                additional_error))
              goto failure;
          
          }
          /* turn off filling in of layers.  */
          else if (strcmp (long_options[option_index].name, "nofill") == 0)
          {
          
          
            if (update_arg((void *)&(args_info->nofill_flag), 0, &(args_info->nofill_given),
                &(local_args_info.nofill_given), optarg, 0, 0, ARG_FLAG,
                check_ambiguity, override, 1, 0, "nofill", '-',
                additional_error))
              goto failure;
          
          }
          /* do not generate rays.  */
          else if (strcmp (long_options[option_index].name, "norays") == 0)
          {
          
          
            if (update_arg((void *)&(args_info->norays_flag), 0, &(args_info->norays_given),
                &(local_args_info.norays_given), optarg, 0, 0, ARG_FLAG,
                check_ambiguity, override, 1, 0, "norays", '-',
                additional_error))
              goto failure;
          
          }
          
          break;
        case '?':	/* Invalid option.  */
          /* `getopt_long' already printed an error message.  */
          goto failure;

        default:	/* bug: option not considered.  */
          fprintf (stderr, "%s: option unknown: %c%s\n", CMDLINE_PARSER_PACKAGE, c, (additional_error ? additional_error : ""));
          abort ();
        } /* switch */
    } /* while */



  if (check_required)
    {
      error += cmdline_parser_required2 (args_info, argv[0], additional_error);
    }

  cmdline_parser_release (&local_args_info);

  if ( error )
    return (EXIT_FAILURE);

  return 0;

failure:
  
  cmdline_parser_release (&local_args_info);
  return (EXIT_FAILURE);
}
