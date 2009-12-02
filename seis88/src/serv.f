C  seis88 - Rays tracing tool
C  Copyright (C) 2009 Ivan Psencik <ip@ig.cas.cz>
C 
C  This program is free software: you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation, either version 3 of the License, or
C  (at your option) any later version.
C 
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.
C 
C  You should have received a copy of the GNU General Public License
C  along with this program.  If not, see <http://www.gnu.org/licenses/>.


      subroutine serv(mode,lin,lou,lu1,lu2,lu3)
c
c     service routine for openning input and output data
c     files and initialization of plotting
c
      character*10 filname
c
      mode=1
c
      write(*,100)
  100 format(1x,'SPECIFY LIN'/1x)
      read(*,101)lin
  101 format(i2)
      if(lin.eq.0)go to 1
      write(*,102)
  102 format(1x,'SPECIFY FILNAME'/1x)
      read(*,103)filname
  103 format(a10)
      open(lin,file=filname,form='formatted')
c
    1 write(*,200)
  200 format(1x,'SPECIFY LOU'/1x)
      read(*,101)lou
      if(lou.eq.0)go to 2
      write(*,102)
      read(*,103)filname
      open(lou,file=filname,form='formatted')
c
    2 write(*,300)
  300 format(1x,'SPECIFY LU1; LU1.GT.0 FORMATTED, LU1.LT.0 UNFORMATTED'
     */1x)
      read(*,101)lu1
      if(lu1.eq.0)go to 3
      write(*,102)
      read(*,103)filname
      if(lu1.gt.0)open(lu1,file=filname,form='formatted')
      if(lu1.lt.0)then
        lu1=-lu1
        open(lu1,file=filname,form='unformatted')
      end if
c
    3 write(*,400)
  400 format(1x,'SPECIFY LU2; LU2.GT.0 FORMATTED, LU2.LT.0 UNFORMATTED'
     */1x)
      read(*,101)lu2
      if(lu2.eq.0)go to 4
      write(*,102)
      read(*,103)filname
      if(lu2.gt.0)open(lu2,file=filname,form='formatted')
      if(lu2.lt.0)then
        lu2=-lu2
        open(lu2,file=filname,form='unformatted')
      end if
c
    4 write(*,500)
  500 format(1x,'SPECIFY LU3; LU3.GT.0 FORMATTED, LU3.LT.0 UNFORMATTED'
     */1x)
      read(*,101)lu3
      if(lu3.eq.0)go to 5
      write(*,102)
      read(*,103)filname
      if(lu3.gt.0)open(lu3,file=filname,form='formatted')
      if(lu3.lt.0)then
        lu3=-lu3
        open(lu3,file=filname,form='unformatted')
      end if
c
    5 write(*,600)
  600 format(1x,'SPECIFY "IPL" FOR PLOTTING, OTHERWISE "0"'/1x)
      read(*,101)ipl
      if(ipl.eq.0)return
      call plots(ldum1,ldum2,ipl)
c
      return
      end
