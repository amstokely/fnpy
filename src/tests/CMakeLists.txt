add_executable(fnpy_test "fnpy.test.f90")
target_link_libraries(fnpy_test PUBLIC fort_test fnpy fnpy_cpp_interface)
add_test(
        NAME fnpy_test
        COMMAND fnpy_test
)