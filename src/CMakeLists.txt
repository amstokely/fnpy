
add_library(fnpy_cpp_interface "fnpy_cpp_interface.cpp")
set_target_properties(fnpy_cpp_interface PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(fnpy_cpp_interface PUBLIC cnpy)

add_library(fnpy "fnpy.interface.f90")
set_target_properties(fnpy PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")
target_include_directories(fnpy PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>)
target_link_libraries(fnpy PUBLIC fort_test fnpy_cpp_interface)
add_subdirectory(tests)