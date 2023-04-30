        subroutine coord(yp1, yp2, n1, n2, alx1, alx2)
        implicit none
        real :: alx1, alx2
        integer :: n1, n2
        integer :: n1m, n2m
        integer :: i, j
        real, dimension(n1-1) :: yp1(500)
        real, dimension(n2-1) :: yp2(500)
c
c       Reference frame at bottom left corner
        n1m=n1-1
        n2m=n2-1
c       along x2 direction
                do j=1,n2
                yp2(j)=(j-1)/float(n2m)*alx2
                enddo
c
c       along x1 direction
                do i=1,n1
                yp1(i)=(i-1)/float(n1m)*alx1
                enddo

        return
        end subroutine

