        subroutine geometryx2(yl, x, y, x1, b, h, lr, beta, ib)
        implicit none
        real :: x1, b, h, lr, beta
        real :: x2, x3, x4
        real :: sigma, etha
        integer :: ib
        real :: x, y
        real :: ylinesp, yolinesp
        real, dimension(2*ib) :: yl(2)

c       compute limits
        sigma=h
        etha=sigma*(cos(beta*3.1415/180)/cos((90-beta)*3.1415/180))
        x2=x1+b
        x3=x2+lr
        x4=x3+etha

c       compound trapezoid x2-limits 

                if(x.ge.x1.and.x.le.x2) then
                ylinesp=h*((x-x1)/(x2-x1))
                        yl(1)=0
                        yl(2)=linesp
                elseif(x.gt.x2.and.x.le.x3) then
c                side=abs(x2-x3)
                        yl(1)=0
                        yl(2)=h
                elseif(x.gt.x3.and.x.le.x4) then
                yolinesp=h*((x-x4)/(x3-x4))
                        yl(1)=0
                        yl(2)=olinesp
                else
                yl(1)=1000
                yl(2)=-1000

                endif

        return
        end subroutine

