        program main
        implicit none
        integer :: k, l
        integer :: u
        integer :: num1, num2
        real :: lx1, lx2
        real, dimension(num1-1) :: yp1(500)
        real, dimension(num2-1) :: yp2(500)
        integer, dimension(num1-1,num2-1) :: forclo(500,500)
        real :: x1, hyp, alpha, beta
        integer :: ib
c-----------------------------------------------------------------------
c       COMPUTATIONAL DOMAIN
c-----------------------------------------------------------------------
        num1=401.0         !number of grid points on the x1-direction
        num2=401.0         !number of grid points on the x2-direction
        lx1=40.0           !x1 dimension of the computatinal domain
        lx2=16.0           !x2 dimension of the computational domain
c-----------------------------------------------------------------------
c       BODY PARAMETERS
c-----------------------------------------------------------------------
        x1=10.0         !x1 coordinate of bottom left vertix          
        hyp=5.0         !hyp dimension of left side rectangle triangle
        alpha=30.0      !angle near the basis of the lower wall/left side
        beta=25.0       !angle away from the basis/right side
c     
        ib=1            !number of bodies
c-----------------------------------------------------------------------
c       CALLING SUBROUTINES
c-----------------------------------------------------------------------
        call coord(yp1, yp2, num1, num2, lx1, lx2)
        call gebody(forclo, yp1, yp2, x1, hyp, alpha, beta, num1, num2,
     &  ib)
c-----------------------------------------------------------------------
c       OUTPUT FILE
c-----------------------------------------------------------------------
        open(newunit=u, file='yp1_coord.dat')
        do k=1,num1-1
        write(u,*) k, yp1(k)
        enddo
        close(u)

        open(newunit=u, file='yp2_coord.dat')
        do l=1,num2-1
        write(u,*) l, yp2(l)
        enddo
        close(u)

        open(newunit=u, file='forclo.dat')
        do k=1,num1-1
                do l=1,num2-1
                write(u,*) yp1(k), yp2(l), forclo(k,l)
                enddo
        enddo
        close(u)

        end program

