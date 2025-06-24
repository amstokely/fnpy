program fnpy_test
   use iso_c_binding
   use fort_test
   use fnpy_mod
   implicit none

   type(TestSet) :: testset_1
   type(TestSet), dimension(:), allocatable :: my_testsets

   type(fnpy_t) :: writer
   real(c_float), allocatable :: arr(:,:)

   ! Allocate and initialize test array
   allocate(arr(3,2))
   arr = reshape([1.0_c_float, 2.0_c_float, 3.0_c_float, 4.0_c_float, 5.0_c_float, 6.0_c_float], shape(arr))

   ! Initialize and write using fnpy_t derived type
   call writer%init_from_2d("output.npy", arr)
   call writer%write()

   ! Define test set
   testset_1 = new_testset( &
           (/  &
                   assert(.true.), &
                           assert_eq(2.0, 1.0 + 1.0), &
                           assert_neq(2.0, 2.0 + 2.0), &
                           assert_approx(4.d0, 4.d0 + 10.d0 * epsilon(4.d0)) &
                   /), &
           name = "Sample test set" &
           )

   ! Run test
   my_testsets = (/ testset_1 /)
   call run_and_exit(my_testsets)
end program fnpy_test
