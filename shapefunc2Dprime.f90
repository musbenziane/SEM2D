! Created by mus on 17/08/2021.
! 2D Shape functions derivatives.

subroutine shapefunc2Dprime(xi,eta,shapefuncprime)
    implicit none
    real (kind=4), intent(in)                   :: xi,eta
    real (kind=4), dimension(4,2), intent(out)  :: shapefuncprime

    ! Derivatives w.r.t xi
    shapefuncprime(1,1) =  .25 * (eta - 1.)
    shapefuncprime(2,1) = -.25 * (eta - 1.)
    shapefuncprime(3,1) = -.25 * (eta + 1.)
    shapefuncprime(4,1) =  .25 * (eta + 1.)

    ! Derivatives w.r.t eta
    shapefuncprime(1,2) =  .25 * (xi - 1.)
    shapefuncprime(2,2) = -.25 * (xi + 1.)
    shapefuncprime(3,2) = -.25 * (xi - 1.)
    shapefuncprime(4,2) =  .25 * (xi + 1.)

end subroutine shapefunc2Dprime