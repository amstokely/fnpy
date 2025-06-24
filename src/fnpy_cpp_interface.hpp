#ifndef FNPY_CPP_INTERFACE_HPP
#define FNPY_CPP_INTERFACE_HPP
#include <string>
#include <vector>
#include <cnpy.h>


void cpp_write_npy_float_array(
    const char *filename, const float *data, const size_t *shape,
    int ndim
) {
    std::vector<size_t> dims(shape, shape + ndim);
    cnpy::npy_save(filename, data, dims, "w");
}

extern "C" {
void write_npy_float_array(
    const char *filename, const float *data, const size_t *shape,
    int ndim
) {
    cpp_write_npy_float_array(filename, data, shape, ndim);
}
    void foo();
}


#endif //FNPY_CPP_INTERFACE_HPP
