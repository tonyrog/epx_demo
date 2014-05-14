//
// Calculate mandelbrot
// f(0) = x+yi
// f(n) = f(n)^2 + c
//
#if defined(CONFIG_USE_DOUBLE)

#if defined(cl_khr_fp64)  // Khronos extension available?
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
typedef double  real_t;
#elif defined(cl_amd_fp64)  // AMD extension available?
#pragma OPENCL EXTENSION cl_amd_fp64 : enable
typedef double  real_t;
#else
#warning "double is not supported"
typedef float real_t;
#endif

#else
typedef float real_t;
#endif

__kernel void z2(const real_t x, const real_t y,
		 const real_t xs, const real_t ys,
		 const uint w, const uint h,
		 const uint n,
		 __global unsigned int* out)
{
    uint i = get_global_id(0);
    uint j = get_global_id(1);
    if ((i < w) && (j < h)) {
        uint k = 0;
	real_t cx = x + i*xs;
	real_t cy = y + j*ys;
	real_t a = 0;
	real_t b = 0;
	real_t a2 = 0;
	real_t b2 = 0;

	while ((k < n) && ((a2 + b2) < 4)) {
	    b = 2*a*b + cy;
	    a = a2-b2 + cx;
	    a2 = a*a;
	    b2 = b*b;
	    k++;
    	}
	out[j*w + i] = (k>=n) ? 0 : k;
    }
}
