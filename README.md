# ROCL

rocl is an OpenCL binding for R that tries to stay true to the C interface.

This is intended to be a package that is used by other packages to deliver accelerated routines 
to R end-users without them having to worry about OpenCL programming.


This is still a work in progress, but basic OpenCL workflow already works.

## Requirements

You need the OpenCL headers and an OpenCL loader to use this package.

Ubuntu: `apt install ocl-icd-opencl-dev`

### Intel CPU

Download and install the runtime from the [Intel Website](https://software.intel.com/en-us/articles/opencl-drivers#cpu-section)

### Intel GPU

Install beignet

Ubuntu: `apt install beignet`

### Nvidia GPU

OpenCL is included in the proprietary driver.

## Installing

```
library(devtools)
install_github("caffeine-overload/rocl")
```


## Example Usage

```
library(float)
library(rocl)

adder <- "
__kernel void sum(
    __global const float *a_g, __global const float *b_g, __global float *res_g)
{
  int gid = get_global_id(0);
  res_g[gid] = a_g[gid] + b_g[gid];
}
"

devs <- cl.get.devices(cl.get.platforms()[[1]])

ctx <- cl.create.context(devs)

queue <- cl.create.queue(ctx, devs[[1]])

prog <- cl.create.program(ctx, adder)
cl.build.program(prog)

kern <- cl.create.kernel(prog, "sum")

ll <- as.integer(2 ^ 20)

l1 <- as.float(rep(5.5, ll))
l2 <- as.float(rep(10.3, ll))
l3 <- as.float(double(ll))

buf1 <- cl.create.buffer(ctx, "CL_MEM_READ_ONLY", size = ll * 4)
buf2 <- cl.create.buffer(ctx, "CL_MEM_READ_ONLY", size = ll * 4)
buf3 <- cl.create.buffer(ctx, "CL_MEM_WRITE_ONLY", size = ll * 4)

cl.enqueue.write.buffer(queue, buf1, l1)
cl.enqueue.write.buffer(queue, buf2, l2)

cl.enqueue.kernel(queue, kern, 1L, ll, list(buf1, buf2, buf3))

cl.enqueue.read.buffer(queue, buf3, l3)

stopifnot(l3 == l1 + l2)

cl.release.kernel(kern)
cl.release.program(prog)
cl.release.buffer(buf1)
cl.release.buffer(buf2)
cl.release.buffer(buf3)
cl.release.queue(queue)
cl.release.context(ctx)

```