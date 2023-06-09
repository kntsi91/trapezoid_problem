       subroutine geometryx1(xl, x, y, x1, b, h, lr, beta, ib)
        implicit none
        real :: x1, b, h, lr, beta
        real :: x2, x3, x4
        real :: y1, y2
        real :: sigma, etha
        integer :: ib
        real :: x, y
        real :: xlinesp, xolinesp
        real, dimension(2*ib) :: xl(2)

c       compute limits
        sigma=h
        etha=sigma*(cos(beta*3.1415/180)/cos((90-beta)*3.1415/180))
        x2=x1+b
        x3=x2+lr
        x4=x3+etha
c
        y1=0
        y2=h
c       compound trapezoid limits 
c cccccccccccccccccccccc
c
c                if(y.ge.y1.and.y.le.y2) then
c                        if(x.ge.x1.and.x.le.x2) then
c                        xlinesp=b*y/h +x1
c                        xl(1)=xlinesp
c                        xl(2)=x2
c                        elseif(x.gt.x2.and.x.le.x3) then
c                        xl(1)=x2
c                        xl(2)=x3
c                        elseif(x.gt.x3.and.x.le.x4) then
c                        xolinesp=(x3-x4)*y/h +x4
c                        xl(1)=x3
c                        xl(2)=xolinesp
c                        endif
                if(y.ge.y1.and.y.le.y2) then
                xlinesp=b*y/h +x1
                xolinesp=(x3-x4)*y/h +x4
                xl(1)=xlinesp
                xl(2)=xolinesp        
                else
                xl(1)=1000
                xl(2)=-1000
                endif

c                if(x.ge.x1.and.x.le.x2) then
c                linesp=h*((x-x1)/(x2-x1))
c                        yl(1)=0
c                        yl(2)=linesp
c                elseif(x.gt.x2.and.x.le.x3) then
c                side=abs(x2-x3)
c                        yl(1)=0
c                        yl(2)=h
c                elseif(x.gt.x3.and.x.le.x4) then
c                olinesp=h*((x-x4)/(x3-x4))
c                        yl(1)=0
c                        yl(2)=olinesp
c                else
c                yl(1)=1000
c                yl(2)=-1000
c
c                endif

        return
        end subroutine

