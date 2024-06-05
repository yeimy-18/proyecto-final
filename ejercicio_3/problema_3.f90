program min_max_array
    implicit none
    integer :: array(10)
    integer :: i, min_val, max_val

    ! Leer los valores del arreglo
    print *, "Ingrese los 10 elementos del arreglo:"
    do i = 1, 10
        read *, array(i)
    end do

    ! Llamar a la función para encontrar el mínimo y el máximo
    call find_min_max(array, min_val, max_val)

    ! Mostrar el mínimo y el máximo
    print *, "El mínimo es:", min_val
    print *, "El máximo es:", max_val

contains

    ! Función para encontrar el mínimo y el máximo
    subroutine find_min_max(arr, min_val, max_val)
        integer, intent(in) :: arr(:)
        integer, intent(out) :: min_val, max_val
        integer :: i

        min_val = arr(1)
        max_val = arr(1)

        do i = 2, size(arr)
            if (arr(i) < min_val) then
                min_val = arr(i)
            end if
            if (arr(i) > max_val) then
                max_val = arr(i)
            end if
        end do
    end subroutine find_min_max

end program min_max_array