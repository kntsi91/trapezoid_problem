        subroutine gebody(forclo, yp1, yp2, yp3, x1, b, h, lr, beta, n1,
     &  n2, n3, ib, alx3)
        implicit none
        real :: x1, b, h, lr, beta
        integer :: n1, n2, n3, n1m, n2m, n3m
        integer :: ib, intmax
        integer :: ilp, ild, il
        integer :: i, j , k, n
c
        real :: alx3
        real :: toll, zup, zlo
c        real :: toll, xup, xlo
        real, dimension(2*ib) :: zl(2)
c        real, dimension(2*ib) :: xl(2)
c        real :: toll, yup, ylo
c        real, dimension(2*ib) :: yl(2)
        real, dimension(n1-1) :: xbod(500)
        real, dimension(n2-1) :: ybod(500)
        real, dimension(n3-1) :: zbod(500)
        real, dimension(n1-1) :: yp1(500)
        real, dimension(n2-1) :: yp2(500)
        real, dimension(n3-1) :: yp3(500)
        integer, dimension(n1-1,n2-1) :: forclo(500,500)
c       coordinates and domain dimensions
        n1m=n1-1
        n2m=n2-1
        n3m=n3-1
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
        do k=1,n3m
        zbod(k)=yp3(k)
        call geometryx3(alx3,zl,xbod(i),ybod(j),x1,b,h,lr,beta,ib)
c        call geometryx1(xl,xbod(i),ybod(j),x1,b,h,lr,beta,ib)
c        call geometryx2(yl,xbod(i),ybod(j),x1,b,h,lr,beta,ib)
        do il=1,intmax/2
        ilp=2*il
        ild=ilp-1
c
        zup=zl(ilp)+toll
        zlo=zl(ild)-toll
c        xup=xl(ilp)+toll
c        xlo=xl(ild)-toll
c        yup=yl(ilp)+toll
c        ylo=yl(ild)-toll
                if(zbod(k).le.zup.and.zbod(k).ge.zlo)then
c                if(xbod(i).le.xup.and.xbod(i).ge.xlo)then
c                if(ybod(j).le.yup.and.ybod(j).ge.ylo)then
                n=n+1
c                forclo(k,j)=1
                forclo(i,j)=1
                endif
        enddo
        enddo
        enddo
        enddo


        return
        end subroutine

