! Created by mus on 19/08/2021.

subroutine jacobian2D()
implicit none

    real(kind=4), intent(in)                            :: x0, z0, h
    integer, intent(in)                                 :: N, nel1, nel2, ngll1, ngll2
    real(kind=4), dimension(nel1+1,nel2+1), intent(in)  :: mesh_x, mesh_z
    real(kind=4), dimension(ngll1,ngll2), intent(in)    :: mesh_gll1, mesh_gll2


    real(kind=4), dimension(N+1)                        :: xi, eta, wi
    real(kind=4), dimension(4,2)                        :: shapeprime
    real(kind=4), dimension(nel1,nel2,4)                :: jacobi
    real(kind=4), dimension(nel1,nel2)                  :: jacobian
    integer, dimension(N+1,nel1)                        :: C1
    integer, dimension(N+1,nel2)                        :: C2
    integer                                             :: i, j, k, l, m
    real(kind=4)                                        :: sum1, sum2

    call gll(N,xi,wi)
    call gll(N,eta,wi)
    call connectivity_matrix(N,nel1,C1)
    call connectivity_matrix(N,nel2,C2)


    do i=1,nel1
        do j=1,nel2
            do k=1,N+1
                do l=1,N+1
                    call shapefunc2Dprime(xi(l),eta(k),shapeprime)
                    sum1 = shapeprime(1,1) * mesh_x(i,j) + shapeprime(2,1) * mesh_x(i,j+1) + &
                            shapeprime(3,1) * mesh_x(i+1,j) + shapeprime(4,1) * mesh_x(i+1,j+1)

                    sum2 = shapeprime(1,2) * mesh_x(i,j) + shapeprime(2,2) * mesh_x(i,j+1) + &
                            shapeprime(3,2) * mesh_x(i+1,j) + shapeprime(4,2) * mesh_x(i+1,j+1)

                    sum3 = shapeprime(1,1) * mesh_z(i,j) + shapeprime(2,1) * mesh_z(i,j+1) + &
                            shapeprime(3,1) * mesh_z(i+1,j) + shapeprime(4,1) * mesh_z(i+1,j+1)

                    sum4 = shapeprime(1,2) * mesh_z(i,j) + shapeprime(2,2) * mesh_z(i,j+1) + &
                            shapeprime(3,2) * mesh_z(i+1,j) + shapeprime(4,2) * mesh_z(i+1,j+1)

                    jacobi(nel1,nel2,1) = sum1
                    jacobi(nel1,nel2,2) = sum2
                    jacobi(nel1,nel2,3) = sum3
                    jacobi(nel1,nel2,4) = sum4

                end do
            end do
            jacobian(nel1,nel2) = jacobi(nel1,nel2,1) * jacobi(nel1,nel2,4) - jacobi(nel1,nel2,2) * jacobi(nel1,nel2,3)
        end do
    end do



end subroutine jacobian2D