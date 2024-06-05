module sort_module
    implicit none

contains

    subroutine bubble_sort(arr, n)
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: n
        integer :: i, j, temp

        do i = 1, n-1
            do j = 1, n-i
                if (arr(j) > arr(j+1)) then
                    temp = arr(j)
                    arr(j) = arr(j+1)
                    arr(j+1) = temp
                end if
            end do
        end do
    end subroutine bubble_sort

end module sort_module

program sort_array
    use sort_module
    implicit none
    integer :: i
    integer :: array(10)

    ! Read the array
    print*, "Enter 10 integers:"
    do i = 1, 10
        read*, array(i)
    end do

    ! Call the sorting subroutine
    call bubble_sort(array, 10)

    ! Print the sorted array
    print*, "Sorted array:"
    do i = 1, 10
        print*, array(i)
    end do

end program sort_array