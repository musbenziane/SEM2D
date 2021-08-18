program SEM2D
    implicit none
    character(len=40)                :: filename, filecheck
    integer                          :: N, nel1, nel2, nt, isrc, isnap
    real(kind=4)                     :: h, f0, dt, z0, x0




    write(*,*) "##########################################"
    write(*,*) "######## Reading parameters file #########"
    write(*,*) "##########################################"

    filename="parameters.in"

    print*,"Is the parameters input file (parameters.in) [Yes/no]"
    read(*,*) filecheck

    if (filecheck=="Yes" .or. filecheck=="yes" .or. filecheck=="y" .or. &
            filecheck=="Y") then
        write(*,*) "Reading simulation parameters..."

    elseif  (filecheck=="No" .or. filecheck=="no" .or. filecheck=="n" .or. &
            filecheck=="N") then
        write(*,*) "Enter simulation parameters text file name with extension"
        write(*,*) "40 characters max"
        read(*,*) filename

    else
        write(*,*) "Only: Yes/yes/Y/y & No/no/N/n are handled"
        write(*,*) "The program have been terminated, please star over"
        stop
    end if

    open (2, file=filename, status = 'old')
    read(2,*) N
    read(2,*) nel1, nel2
    read(2,*) z0, x0
    read(2,*) h
    read(2,*) f0
    read(2,*) dt
    read(2,*) nt
    read(2,*) isrc
    read(2,*) isnap
    close(2)

    print*,"Polynomial order                -> ",N
    print*,"Number of elements (1   2)      -> ",nel1, nel2
    print*,"Z0 and X0                       -> ",z0, x0
    print*,"Element size                    -> ",h
    print*,"Wavelet's peak frequency        -> ",f0
    print*,"Time step                       -> ",dt
    print*,"Number of time steps            -> ",nt
    print*,"Source location                 -> ",isrc
    print*,"Snapshot interval               -> ",isnap

end program


