        subroutine geometryx3(alx3, zl, x, y, x1, b, h, lr, beta, ib)
        implicit none
        real :: x1, b, h, lr, beta
        real :: alx3
        real :: x2, x3, x4
        real :: y1, y2
        real :: sigma, etha
        integer :: ib
        real :: x, y
        real :: xlinesp, xolinesp
        real, dimension(2*ib) :: zl(2)

c       compute limits
        sigma=h
        etha=sigma*(cos(beta*3.1415/180)/cos((90-beta)*3.1415/180))
        x2=x1+b
        x3=x2+lr
        x4=x3+etha
c
        y1=0
        y2=h
c
        xlinesp=b*y/h +x1
        xolinesp=(x3-x4)*y/h +x4
c       compound trapezoid x3-limits 
c                if(y.ge.y1.and.y.le.y2) then
c                zl(1)=-alx3/2 + 1.0
c                zl(2)=alx3/2 - 1.0
c                if(x.ge.xlinesp.and.x.le.xolinesp) then
c                xlinesp=b*y/h +x1
c                xolinesp=(x3-x4)*y/h +x4
                if(x.ge.xlinesp.and.x.le.xolinesp) then
                zl(1)=-alx3/2 + 1.0
                zl(2)=alx3/2 - 1.0
c                        endif
                else
                zl(1)=1000
                zl(2)=-1000
                endif


        return
        end subroutine

