! Created by mus on 17/08/2021.
! 2D Shape functions for non-curved elements.
! 2nd order Lagrange polynomials will be added later on to incorporate curved elements

subroutine shapefunc2D(xi,eta,shapefunc)
    implicit none
    real (kind=4), intent(in)                 :: xi,eta
    real (kind=4), dimension(4), intent(out)  :: shapefunc

    shapefunc(1) = .25 * (xi - 1.) * (eta - 1.)     ! N1
    shapefunc(2) = .25 * (xi + 1.) * (1. - eta)     ! N2
    shapefunc(3) = .25 * (1. - xi) * (eta + 1.)     ! N3
    shapefunc(4) = .25 * (xi + 1.) * (eta + 1.)     ! N4

end subroutine shapefunc2D