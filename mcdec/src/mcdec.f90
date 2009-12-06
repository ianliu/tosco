
      subroutine mc_dec(ncf,L,nc,ns,jtarget,x,xdec)
!------------------------------------------------------------------!
! Copyright (c) 2007 by the Society of Exploration Geophysicists.  !
! For more information, go to http://software.seg.org/2007/0003 .  !
! You must read and accept usage terms at:                         !
! http://software.seg.org/disclaimer.txt before use.               !
!------------------------------------------------------------------!
!   A group of nc traces (or nc channels) into the matrix x
!   is used to deconvolve one trace (trace jtarget)
!
!   Method: direct multichannel predictive deconvolution
!
!   input parameters:
!       ns   - number of samples per trace
!       ncf  - number of filter coefficientes
!       L    - prediction distance in number of samples
!       nc   - number of channels (traces) being used to deconvolve one trace
!       x(1), ...   , x(ns)   - 1st channel
!       x(ns+1), ..., x(2*ns) - 2nd channel, ...
!       jtarget - number of the selected trace to be deconvolved 1<= jtrace <= nc
!
!   output
!       xdec(1), ..., xdec(ns)   trace deconvolved at position jtrace
!
!  Author: Milton J. Porsani, porsani@cpgg.ufba.br Salvador, Brazil /10/2006
!--------------------------------------------------------------------------
      dimension xdec(ns),x(nc*ns)
      allocatable ey(:), e(:), eb(:), ea(:), Qa(:), Qb(:), eh(:)
      idim=(ns+2*ncf+1)*nc
      allocate( ey(idim), e(idim),  eb(idim), ea(idim) )
      allocate( Qa(idim), Qb(idim), eh(ns+2*ncf) )
!--------------------------------------------------------------------------
      m=ns+ncf+1
      do j=1,nc            ! initialization
         j1=(j-1)*m
         j2=(j-1)*ns
         do i=1,ns
            ea(j1+i)=x(j2+i)          ! EA=X
            eb(j1+i)=x(j2+i)          ! EB=X
         enddo
      enddo
      jtgt=(jtarget-1)*ns
      eh(1:ns-L)=x(L+1+jtgt:ns+jtgt)  ! EH=XL
      eh(ns-L+1:m)=0.0
      call QN(nc,m,Eb,Qb)             ! QR factorization of EB matrix
      call error_dec(nc,m,Qb,eh,eh)   ! L-step prediction error update

      do k=1,ncf-1                 ! main loop for direct pred. error update
         mk=m+k
         do i=1,nc
            i1=(i-1)*mk
            i2=(i-1)*(mk-1)
            do j=2,mk
               e(i1+j)    = eb(i2+j-1)
               ey(i1+j-1) = ea(i2+j-1)
            enddo
            e(i1+1)   = 0.0
            ey(i1+mk) = 0.0
         enddo
         call QN(nc,mk,E,Qa)      ! QR factorization of EA and EB
         call QN(nc,mk,Ey,Qb)
         do i=1,nc
            i1=(i-1)*mk + 1
            call error_dec(nc,mk,Qa,ey(i1),ea(i1)) ! one-step forward  pred. error update
            call error_dec(nc,mk,Qb,e(i1),eb(i1))  ! one-step backward pred. error update
         enddo
         call QN(nc,mk,Eb,Qb)           ! QR factorization of EB matrix
         call error_dec(nc,mk,Qb,eh,eh) ! L-step prediction error update
      enddo

      xdec(1:L)    = x(jtgt+1:jtgt+L)   ! colecting the deconvolved channel
      xdec(L+1:ns) = eh(1:ns-L)         !
    
      deallocate(ey, e, eb, ea, Qa, Qb, eh)
      return
      end subroutine mc_dec



      subroutine error_dec(n,m,Q,y,e)
!------------------------------------------------------------------!
! Copyright (c) 2007 by the Society of Exploration Geophysicists.  !
! For more information, go to http://software.seg.org/2007/0003 .  !
! You must read and accept usage terms at:                         !
! http://software.seg.org/disclaimer.txt before use.               !
!------------------------------------------------------------------!
! Evaluate the prediction error e=y-Xh=y-QQ^Ty
! using the orthogonality of the matrix Q (X=QR)
! MJP- porsani@cpgg.ufba.br Trondheim (Munken Hotel), 09/09/2003
!--------------------------------------------------------------------
      dimension y(m),Q(M,n),e(m)
      allocatable qty(:)
      allocate (qty(n))
!--------------------------------------------------------------------
  
      do j=1,n
         qty(j)=-dot_product(q(1:m,j),y(1:m))
      enddo

      e(1:m)=y(1:m)

      do j=1,n
         e(1:m)=e(1:m)+q(1:m,j)*qty(j)
      enddo
  
      deallocate(qty)
      return
      end subroutine error_dec

      subroutine QN(n,m,X,Q)
!------------------------------------------------------------------!
! Copyright (c) 2007 by the Society of Exploration Geophysicists.  !
! For more information, go to http://software.seg.org/2007/0003 .  !
! You must read and accept usage terms at:                         !
! http://software.seg.org/disclaimer.txt before use.               !
!------------------------------------------------------------------!
! Generates the QR decomposition of the matrix X (X=QR)
! Q is a orthonormal matrix such Q^TQ=I (identity matrix)
! Only the matrix Q is returned
! MJP porsani@cpgg.ufba.br Trondheim (Munken Hotel) 04/09/2003
!--------------------------------------------------------------------
      dimension X(m,n),Q(m,n)
      allocatable qq(:)
  
      allocate(qq(n))
  
      do j=1,n
         q(1:m,j)=x(1:m,j)
         do i=1,j-1
            gamma=dot_product(q(1:m,i),X(1:m,j))/qq(i)
            q(1:m,j)=q(1:m,j)-gamma*q(1:m,i)
         enddo
         qq(j)=dot_product(q(1:m,j),Q(1:m,j))
      enddo
  
      do j=1,n
         q(1:m,j)=q(1:m,j)/sqrt(qq(j))
      enddo
  
      deallocate(qq)
  
      return
      end subroutine QN
