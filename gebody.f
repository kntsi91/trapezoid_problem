        subroutine gebody(forclo, yp1, yp2, x1, hyp, alpha, beta, n1, n2
     &  , ib)
        implicit none
        real :: x1, hyp, alpha, beta
        integer :: n1, n2, n1m, n2m
        integer :: ib, intmax
        integer :: ilp, ild, il
        integer :: i, j ,n
        real :: toll, yup, ylo
        real, dimension(2*ib) :: yl(2)
        real, dimension(n1-1) :: xbod(500)
        real, dimension(n2-1) :: ybod(500)
        real, dimension(n1-1) :: yp1(500)
        real, dimension(n2-1) :: yp2(500)
        integer, dimension(n1-1,n2-1) :: forclo(500,500)
c       coordinates and domain dimensions
        n1m=n1-1
        n2m=n2-1
        n=0
c
c       geometry
        intmax=2*ib
c
c       compute forclo
        toll=1e-4 !ftoll
        forclo=0
c
c       identification of the grid points in the body (boundary + inner part)        
        do i=1,n1m
        xbod(i)=yp1(i)
        do j=1,n2m
        ybod(j)=yp2(j)
        call geometry(yl,xbod(i),ybod(j),x1,hyp,alpha,beta,ib)
        do il=1,intmax/2
        ilp=2*il
        ild=ilp-1

        yup=yl(ilp)+toll
        ylo=yl(ild)-toll
                if(ybod(j).le.yup.and.ybod(j).ge.ylo)then
                n=n+1
                forclo(i,j)=1
                endif
        enddo
        enddo
        enddo


        return
        end subroutine

