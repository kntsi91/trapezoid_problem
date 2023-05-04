        program main
        implicit none
        integer :: k, l, m
        integer :: u
        integer :: num1, num2, num3
        real :: lx1, lx2, lx3
        real, dimension(num1-1) :: yp1(500)
        real, dimension(num2-1) :: yp2(500)
        real, dimension(num3-1) :: yp3(500)
        integer, dimension(num1-1,num2-1) :: forclo(500,500)
c       integer, dimension(num3-1,num2-1) :: forclo(500,500)
        real :: x1, b, h, lr, beta
        integer :: ib
c-----------------------------------------------------------------------
c       COMPUTATIONAL DOMAIN
c-----------------------------------------------------------------------
        num1=21.0         !number of grid points on the x1-direction
        num2=21.0         !number of grid points on the x2-direction
        num3=21.0         !number of grid points on the x3-direction
        lx1=40.0           !x1 dimension of the computatinal domain
        lx2=16.0           !x2 dimension of the computational domain
        lx3=10.0           !x3 dimension of the computational domain
c-----------------------------------------------------------------------
c       BODY PARAMETERS
c-----------------------------------------------------------------------
        x1=10.0         !      
        b=3.0           !
        h=4.0           !
        lr=6            !
        beta=25.0       !
c     
        ib=1            !number of bodies
c-----------------------------------------------------------------------
c       CALLING SUBROUTINES
c-----------------------------------------------------------------------
        call coord(yp1, yp2, yp3, num1, num2, num3, lx1, lx2, lx3)
        call gebody(forclo, yp1, yp2, x1, b, h, lr, beta, num1, num2,
     &  num3, ib, lx3)
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

        open(newunit=u, file='yp3_coord.dat')
        do m=1,num3-1
        write(u,*) m, yp3(m)
        enddo
        close(u)

        open(newunit=u, file='forclo.dat')
        do k=1,num1-1
                do l=1,num2-1
                write(u,*) yp1(k), yp2(l), forclo(k,l)
                enddo
        enddo
        close(u)

c       open(newunit=u, file='forclo.dat')
c       do m=1,num3-1
c               do l=1,num2-1
c               write(u,*) yp3(m), yp2(l), forclo(m,l)
c               enddo
c       enddo
c       close(u)

        end program

