module fnpy_mod
    use iso_c_binding
    implicit none

    private
    public :: fnpy_t

    interface
        subroutine write_npy_float_array(filename, data, shape, ndim) bind(C)
            import :: c_char, c_float, c_size_t, c_int
            character(kind=c_char), allocatable :: filename(:)  ! <- change from scalar to rank-1

            real(c_float), dimension(*), intent(in) :: data
            integer(c_size_t), dimension(*), intent(in) :: shape
            integer(c_int), value :: ndim
        end subroutine write_npy_float_array
    end interface

    type :: fnpy_t
        character(kind=c_char), allocatable :: filename(:)  ! must be a rank-1 array
        real(c_float), allocatable :: flat_data(:)
        integer(c_size_t), dimension(:), allocatable :: shape
        integer(c_int) :: ndim
    contains
        procedure :: init_from_2d
        procedure :: write
    end type fnpy_t

contains

    subroutine init_from_2d(this, file, array)
        class(fnpy_t), intent(inout) :: this
        character(len=*), intent(in) :: file
        real(c_float), dimension(:,:), intent(in) :: array

        integer(c_size_t) :: nrows, ncols

        ! Assign filename as C string
        call to_c_string(file, this%filename)

        ! Flatten the 2D array in column-major order
        allocate(this%flat_data(size(array)))
        this%flat_data = reshape(array, [size(array)])

        ! Set shape and dimension
        ncols = size(array, 1, kind=c_size_t)
        nrows = size(array, 2, kind=c_size_t)
        this%ndim = 2
        allocate(this%shape(2))
        this%shape = [nrows, ncols]
    end subroutine init_from_2d

    subroutine write(this)
        class(fnpy_t), intent(in) :: this
        call write_npy_float_array(this%filename, this%flat_data, this%shape, this%ndim)
    end subroutine write

    subroutine to_c_string(fstr, cstr)
        character(len=*), intent(in) :: fstr
        character(kind=c_char), allocatable, intent(out) :: cstr(:)
        integer :: len, i

        len = len_trim(fstr) + 1
        allocate(cstr(len))
        do i = 1, len - 1
            cstr(i) = fstr(i:i)
        end do
        cstr(len) = c_null_char
    end subroutine to_c_string

end module fnpy_mod
