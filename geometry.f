        subroutine geometry(yl, x, y, x1, hyp, alpha, beta, ib)
        implicit none
        real :: x1, hyp, alpha, beta
        real :: x2, x3, x4
        real :: sigma, etha, mu
        integer :: ib
        real :: x, y
        real :: linesp, side, olinesp
        real, dimension(2*ib) :: yl(2)

c       compute limits
        sigma=hyp*cos(alpha*3.1415/180)
        etha=hyp*sin(alpha*3.1415/180)
        mu=etha*tan(beta*3.1415/180)
        x2=x1+sigma
        x3=x2+etha
        x4=x3+mu
c    
c       compound trapezoid limits 

                if(x.ge.x1.and.x.le.x2) then
                linesp=etha*((x-x1)/(x2-x1))
                        if(y.le.linesp) then
                        yl(1)=0
                        yl(2)=linesp
                        else
                        yl(1)=1000
                        yl(2)=-1000
                        endif
                elseif(x.gt.x2.and.x.le.x3) then
                side=abs(x2-x3)
                        if(y.le.side) then
                        yl(1)=0
                        yl(2)=side
                        else
                        yl(1)=1000
                        yl(2)=-1000
                        endif
                elseif(x.gt.x3.and.x.le.x4) then
                olinesp=etha*((x-x4)/(x3-x4))
                        if(y.le.olinesp) then
                        yl(1)=0
                        yl(2)=olinesp
                        else
                        yl(1)=1000
                        yl(2)=-1000
                        endif
                else
                yl(1)=1000
                yl(2)=-1000

                endif

        return
        end subroutine

