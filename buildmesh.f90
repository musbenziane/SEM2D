! Created by mus on 17/08/2021.

subroutine buildmesh(x0,z0,h,N,nel1,nel2,ngll1,ngll2,mesh_z,mesh_x,mesh_gll1,mesh_gll2)
    implicit none
    real(kind=4), intent(in)                            :: x0, z0, h
    integer, intent(in)                                 :: N, nel1, nel2, ngll1, ngll2
    real(kind=4), dimension(nel1+1,nel2+1), intent(out) :: mesh_x, mesh_z
    real(kind=4), dimension(ngll1,ngll2), intent(out)   :: mesh_gll1, mesh_gll2


    real(kind=4), dimension(N+1)                        :: xi, eta, wi
    real(kind=4), dimension(4)                          :: shape
    integer, dimension(N+1,nel1)                        :: C1
    integer, dimension(N+1,nel2)                        :: C2
    integer                                             :: i, j, k, l, m
    real(kind=4)                                        :: sum1, sum2


    call gll(N,xi,wi)
    call gll(N,eta,wi)
    call connectivity_matrix(N,nel1,C1)
    call connectivity_matrix(N,nel2,C2)

    mesh_x(:,:) = 0
    mesh_z(:,:) = 0


    ! Build regular element wise mesh for the 4 anchor points on each element

    mesh_z(:,:) = 0
    do i=1,nel2+1
        do j=1,nel1+1
            if (j==1) then
                mesh_z(j,i) = z0
                cycle
            end if
            mesh_z(j,i) = mesh_z(j-1,i) + h
        end do
    end do

    mesh_x(:,:) = 0
    do i=1,nel1+1
        do j=1,nel2+1
            if(j==1) then
                mesh_x(i,j) = x0
                cycle
            end if
            mesh_x(i,j) = mesh_x(i,j-1) + h
        end do
    end do

    ! Build actual mesh (at gll points) by means of shape functions
    mesh_gll1(:,:) = 0

    do i=1,nel1
        do j=1,nel2
            do k=1,N+1
                do l=1,N+1
                    call shapefunc2D(xi(k),eta(l),shape)

                    sum1 = shape(1) * mesh_z(i,j) + shape(2) * mesh_z(i,j+1) + shape(3) * mesh_z(i+1,j) &
                            + shape(4) * mesh_z(i+1,j+1)
                    sum2 = shape(1) * mesh_x(i,j) + shape(2) * mesh_x(i,j+1) + shape(3) * mesh_x(i+1,j) &
                            + shape(4) * mesh_x(i+1,j+1)

                    mesh_gll1(C1(l,i),C2(k,j)) = sum1
                    mesh_gll2(C1(l,i),C2(k,j)) = sum2
                end do
            end do
        end do
    end do


end subroutine buildmesh