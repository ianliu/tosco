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

#ifndef _PARSER_H
#define _PARSER_H

struct parse_params {

        gchar *npnt;
        gchar **xcoord;
        gchar **zcoord;
        gchar **iii;
        gchar *v1;
        gchar *v2;
        gchar *rho1;
        gchar *rho2;
        gchar *ptos;
        gchar **nqp;
        gchar **nqs;
        gchar **qps;
};


struct s88 *parse_command_line (int argc, char **argv);

#endif                          /* _PARSER_H */
