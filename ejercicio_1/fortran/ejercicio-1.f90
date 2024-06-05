module funciones
    implicit none
contains
    subroutine leedatos(datos, npotencias, Max)
        implicit none
        real, dimension(10), intent(out) :: datos
        integer, dimension(10), intent(out) :: npotencias
        integer, intent(in) :: Max
        ! Implementación de la subrutina leedatos
    end subroutine leedatos

    real function potencia(x, n) result(res)
        implicit none
        real, intent(in) :: x
        integer, intent(in) :: n
        ! Implementación de la función potencia
    end function potencia
end module funciones

program C_potencia
    use funciones
    implicit none
    real, dimension(10) :: datos
    integer, dimension(10) :: npotencias
    integer :: Max = 50
    integer :: k

    do while ((Max .lt. 0) .or. (Max .gt. 10))
        print *, "¿Cuantos numeros deseas calcular su potencia (1-10)?"
        read *, Max
    end do

    call leedatos(datos, npotencias, Max)

    print *, "X | n | Y"
    print *, "--------------"
    
    do k = 1, Max
        write(*, 100) datos(k), npotencias(k), potencia(datos(k), npotencias(k))
    100 format(1x, f4.1, 1x, "|", 1x, i3, 2x, "|", 1x, f7.1)
    end do

    stop 'Programa potencias termino de forma normal'
end program C_potencia