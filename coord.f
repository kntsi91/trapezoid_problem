        subroutine coord(yp1, yp2, yp3, n1, n2, n3, alx1, alx2, alx3)
        implicit none
        real :: alx1, alx2, alx3
        integer :: n1, n2, n3
        integer :: n1m, n2m, n3m
        integer :: i, j, k
        real, dimension(n1-1) :: yp1(500)
        real, dimension(n2-1) :: yp2(500)
        real, dimension(n3-1) :: yp3(500)
c
c       Reference frame at bottom left corner
        n1m=n1-1
        n2m=n2-1
        n3m=n3-1
c       along x3 direction
                do k=1,n3
                yp3(k)=(k-1)/float(n3m)*alx3 - alx3/2
                enddo
c                
c       along x2 direction
                do j=1,n2
                yp2(j)=(j-1)/float(n2m)*alx2 - alx2/2
                enddo
c
c       along x1 direction
                do i=1,n1
                yp1(i)=(i-1)/float(n1m)*alx1 - alx1/2
                enddo

        return
        end subroutine

