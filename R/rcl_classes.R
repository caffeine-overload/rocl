
`$.opencl_device` <- cl.device.getattr

print.opencl_device <- function(x, ...){
  str <- paste0("OpenCL Device\n",
               "\tName:\t", x$CL_DEVICE_NAME, "\n",
               "\tVendor:\t", x$CL_DEVICE_VENDOR, "\n",
               "\tType:\t", x$CL_DEVICE_TYPE, "\n",
               "\tVersion:\t", x$CL_DEVICE_VERSION, "\n",
               "\tExtensions:\t", x$CL_DEVICE_EXTENSIONS, "\n")
  cat(str)
}

`$.opencl_platform` <- cl.platform.getattr

print.opencl_platform <- function(x, ...){
  str <- paste0("OpenCL Platform\n",
                "\tName:\t", x$CL_PLATFORM_NAME, "\n",
                "\tVendor:\t", x$CL_PLATFORM_VENDOR, "\n",
                "\tProfile:\t", x$CL_PLATFORM_PROFILE, "\n",
                "\tVersion:\t", x$CL_PLATFORM_VERSION, "\n",
                "\tExtensions:\t", x$CL_PLATFORM_EXTENSIONS, "\n")
  cat(str)
}

`$.opencl_kernel` <- cl.kernel.getattr

print.opencl_kernel <- function(x, ...){
  str <- paste0("OpenCL Kernel:\n",
                "\tName:\t", x$CL_KERNEL_FUNCTION_NAME, "\n",
                "\tNumber of Arguments:\t", x$CL_KERNEL_NUM_ARGS, "\n")
  cat(str)
}