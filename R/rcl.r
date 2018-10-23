
.deallocate_platform <- function(plat){
  .Call("simple_free_platform", plat)
}

.deallocate_device <- function(dev){
  .Call("simple_free_device", dev)
}


.checkerror <- function(result){
  if(result[[2]] == 0){
    return(result[[1]])
  }else{
    stop(paste0("OpenCL Error: ", result[[2]]))
  }
}

#' @title  Return the number of platforms
#' @return The number of platforms available
cl.platforms.count <- function() {
  result <- .Call("get_num_cl_platforms")
  return(.checkerror(result))
}

#' @title Return all platforms
#' 
#' @return A list of opencl_platform objects
cl.get.platforms <- function(){
  result <- .Call("get_platform_pointers")
  retvals <- .checkerror(result)

  retvals <- result[[1]]
  retvals <- lapply(X = retvals, FUN = function(p){
    class(p) <- "opencl_platform"
    reg.finalizer(p, .deallocate_platform, T)
    return(p)
  })
  return(retvals)
}

#' @title Device Count
#' @description Counts the number of available devices belonging to a platform
#' @param platform An opencl_platform object
#' @return The number of devices available
cl.devices.count <- function(platform){
  stopifnot("opencl_platform" %in% class(platform))
  result <- .Call("get_num_devices", platform)
  return(.checkerror(result))
}

#' @title Get OpenCL devices
#' @description Gets all OpenCL devices available under a given platform
#' @param platform An opencl_platform object
#' @return A list of opencl_device objects
cl.get.devices <- function(platform){
  stopifnot("opencl_platform" %in% class(platform))
  result <- .Call("get_device_pointers", platform)
  retvals <- .checkerror(result)
  
  retvals <- result[[1]]
  retvals <- lapply(X = retvals, FUN = function(p){
    class(p) <- "opencl_device"
    reg.finalizer(p, .deallocate_device, T)
    return(p)
  })
  return(retvals)
}

#' @title Get OpenCL device info
#' @description Gets information from clGetDeviceInfo call
#' @param device An opencl_device object
#' @param attr An OpenCL device attribute as a string
#' @return Requested information about the device
cl.device.getattr <- function(device, attr){
  stopifnot("opencl_device" %in% class(device), is.character(attr))
  result <- .Call("get_device_property", device, attr[[1]])
  return(result)
}

#' @title Get OpenCL platform info
#' @description Gets information from clGetPlatformInfo call
#' @param device An opencl_platform object
#' @param attr An OpenCL platform attribute as a string
#' @return Requested information about the platform
cl.platform.getattr <- function(device, attr){
  stopifnot("opencl_platform" %in% class(device), is.character(attr))
  result <- .Call("get_platform_property", device, attr[[1]])
  return(result)
}

#' @title Release OpenCL context
#' @description Releases resources held by the OpenCL context.
#' @details Context must be released last, after all other objects are released (or garbage collected)
#' @param context An opencl_context object
cl.release.context <- function(context){
  stopifnot("opencl_context" %in% class(context))
  .Call("release_context", context)
}

#' @title Create OpenCL context
#' @description Allocates an OpenCL context with one or more devices.
#' @details A finalizer is not registered for contexts, thus they must be freed manually. 
#' Contexts must be freed only after all OpenCL objects created with the context are
#' freed.
#' @param devices One or a list of opencl_device objects.
#' @return An opencl_context object.
cl.create.context <- function(devices){
  if(class(devices) == "opencl_device"){
    devices <- list(devices)
  }
  stopifnot(class(devices) == "list")
  stopifnot(all("opencl_device" %in% sapply(devices, class)))
  res <- .Call("get_cl_context", devices)
  ctx <- .checkerror(res)
  class(ctx) <- "opencl_context"
  #reg.finalizer(ctx, cl.release.context) Force users to do it manually
  return(ctx)
}

#' @title Release OpenCL buffer
#' @description Frees the resources held by the OpenCL buffer object.
#' @param buffer An opencl_buffer object
cl.release.buffer <- function(buffer){
  stopifnot("opencl_buffer" %in% class(buffer))
  .Call("release_buffer", buffer)
}

#' @title Allocate OpenCL buffer
#' @description Allocates an OpenCL buffer with the given context.
#' @details Calls clCreateBuffer with the given context, flags and buffer.
#' The data is optional, if vector is left null an empty buffer wil be allocated. It can be 
#' filled with data at a later point with a call to cl.enqueue.write.buffer.
#' The size must be the size in bytes of the OpenCL buffer to be allocated. Note that 
#' object.size includes R overhead. Integer and float32 vectors take up 4 bytes
#' per element, double vectors (the R default) take up 8 bytes per element. 
#' Note that 64-bit float performance is crippled on most GPUs, so it is a good idea
#' to cast a double vector to a float32 vector before use with OpenCL. 
#' @param context An opencl_context object.
#' @param flags Character vector of OpenCL buffer flags. Refer to \href{https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/clCreateBuffer.html}{The OpenCL documentation}.
#' @param size The size in bytes of the vector. Mandatory if vector is NULL.
#' @param vector The data to go into an OpenCL buffer. Mandatory if size is NULL.
#' @return An opencl_buffer object
cl.create.buffer <- function(context, flags, size = NULL, vector = NULL){
  stopifnot(!(is.null(size) & is.null(vector)))
  if(!is.null(vector)){
    stopifnot(class(vector) %in% c("numeric", "integer", "float32"))
  }
  elesize <- switch(class(vector),
         "numeric" = 8,
         "integer" = 4,
         "float32" = 4)
  if(!is.null(vector)){
    size <- length(vector) * elesize
  }
  if(class(vector) == "float32"){
    vector = vector@Data
  }
  stopifnot("opencl_context" %in% class(context))
  stopifnot(class(flags) == "character")
  
  memflags <- list(
    CL_MEM_READ_WRITE = 0,
    CL_MEM_WRITE_ONLY = 1,
    CL_MEM_READ_ONLY = 2,
    CL_MEM_USE_HOST_PTR = 3,
    CL_MEM_ALLOC_HOST_PTR = 4,
    CL_MEM_COPY_HOST_PTR = 5,
    CL_MEM_HOST_WRITE_ONLY = 6,
    CL_MEM_HOST_READ_ONLY = 7,
    CL_MEM_HOST_NO_ACCESS = 8
  )
  
  stopifnot(all(flags %in% names(memflags)))
  
  numflags <- as.integer(unname(sapply(X = flags, FUN = function(x) memflags[[x]])))
  
  res <- .Call("get_cl_buf", context, numflags, size, vector);
  
  buffer <- .checkerror(res)
  
  class(buffer) <- "opencl_buffer"
  
  reg.finalizer(buffer, cl.release.buffer, T)
  
  return(buffer)
}

#' @title Release OpenCL queue
#' @description Frees the resources held by the OpenCL queue object.
#' @param queue An opencl_queue object
cl.release.queue <- function(queue){
  stopifnot("opencl_command_queue" %in% class(queue))
  .Call("release_cl_queue", queue)
}

#' @title Create an OpenCL command queue
#' @description Creates an OpenCL command queue for the given context and device.
#' @param context An opencl_context object
#' @param device An opencl_device object
#' @param in_order Specifies whether the queue is an in-order or out-of-order queue. 
#' Not currently used, included for future use.
#' @param profiling Enable profiling of commands. Not currently used, included for future use.
#' @param on_device Create a device-side queue. Not currently used, included for future use.
#' @return An opencl_command_queue object.
cl.create.queue <- function(context, device, in_order = T, profiling = F, on_device = F){
  stopifnot("opencl_context" %in% class(context), "opencl_device" %in% class(device))
  queue <- .checkerror(.Call("get_cl_queue", context, device, in_order, profiling, on_device))
  class(queue) <- "opencl_command_queue"
  reg.finalizer(queue, cl.release.queue, T)
  return(queue)
}

#' @title Release OpenCL program
#' @description Frees the resources held by the OpenCL program object.
#' @param program An opencl_program object
cl.release.program <- function(program){
  stopifnot("opencl_program" %in% class(program))
  .Call("release_cl_program", program)
}

#' @title Create OpenCL program
#' @description Creates an OpenCL program from source code.
#' @param context An opencl_context object
#' @param source_code The source code of the OpenCL kernel
#' @param build Automatically build the program
#' @return An opencl_program object
cl.create.program <- function(context, source_code, build = F){
  stopifnot("opencl_context" %in% class(context), class(source_code) == "character", length(source_code) == 1)
  program <- .checkerror(.Call("create_cl_program", context, source_code))
  
  class(program) <- "opencl_program"
  reg.finalizer(program, cl.release.program)
  if(build) cl.build.program(program)
  return(program)
}

#' @title Load OpenCL binary
#' @description Creates a program from a pre-compiled OpenCL binary
#' @details Loads an OpenCL binary. Can only load a binary into the same device as
#' the binary was built with. So a compiled CPU kernel cannot be loaded into a GPU context.
#' @param path File path of the binary
#' @param context An opencl_context object
#' @param devs A list of devices to load the binary into
#' @return An opencl_program object
cl.load.binary <- function(path, context, devs){
  stopifnot("opencl_context" %in% class(context), class(path) == "character", length(path) == 1)
  
  program <- .checkerror(.Call("create_cl_from_binary", path, context, devs))
  class(program) <- "opencl_program"
  reg.finalizer(program, cl.release.program)
  return(program)
}

#' @title Build OpenCL program
#' @description Compiles the code in an OpenCL program object.
#' @param program An opencl_program object
#' @param options OpenCL build options as a string
cl.build.program <- function(program, options = NULL){
  stopifnot("opencl_program" %in% class(program))
  if(!is.null(options)) stopifnot(class(options) == "character", length(options) == 1)
  status <- .Call("build_cl_program", program, options)
  if(status != 0){
    stop(paste0("OpenCL Error: ", status))
  }
}

#' @title Write data to a buffer
#' @description Enqueues an operation to write data to an OpenCL buffer.
#' @param queue An opencl_command_queue object. The queue the operation will be put on.
#' @param buffer An opencl_buffer object. The buffer to write data to.
#' @param vector The data to write. The length in bytes of the vector must not be longer than the buffer. 
#' @param blocking Whether to wait for the write to finish or return immediately. 
#' Not currently used, included for future use.
#' @param offset Offset in bytes from the beginning of the supplied vector.
#' @param waitlist OpenCL events that must complete before this operation can start.
#' Not currently used, included for future use.
cl.enqueue.write.buffer <- function(queue, buffer, vector, blocking = T, offset = 0L, waitlist = NULL){
  stopifnot("opencl_command_queue" %in% class(queue), "opencl_buffer" %in% class(buffer),
            class(offset) == "integer", length(offset) == 1)
  stopifnot(class(vector) %in% c("numeric", "integer", "float32", "raw"))
  if(class(vector) == "float32"){
    vector = vector@Data
  }
  
  status <- .Call("write_buffer", queue, buffer, vector, blocking, offset, waitlist)
  
  if(status != 0){
    stop(paste0("OpenCL Error: ", status))
  }
  
  return(status)
}

#' @title Read data from a buffer
#' @description Enqueues an operation to read data from an OpenCL buffer to an R vector.
#' @param queue An opencl_command_queue object. The queue the operation will be put on.
#' @param buffer An opencl_buffer object. The buffer to read from.
#' @param destination The destination vector to write data into. Must be as long in bytes as the data being read.
#' @param blocking Whether to wait for the write to finish or return immediately. 
#' Not currently used, included for future use.
#' @param offset Offset in bytes from the beginning of the buffer to begin reading from.
#' @param waitlist OpenCL events that must complete before this operation can start.
#' Not currently used, included for future use.
cl.enqueue.read.buffer <- function(queue, buffer, destination, offset = 0L, blocking = T, waitlist = NULL){
  stopifnot("opencl_command_queue" %in% class(queue), "opencl_buffer" %in% class(buffer),
            class(offset) == "integer", length(offset) == 1)
  stopifnot(class(destination) %in% c("numeric", "integer", "float32", "raw"))
  if(class(destination) == "float32"){
    destination = destination@Data
  }
  
  status <- .Call("read_buffer", queue, buffer, destination, offset, blocking, waitlist)
  
  if(status != 0){
    stop(paste0("OpenCL Error: ", status))
  }
  
  return(status)
}

#' @title Release OpenCL kernel
#' @description Frees the resources held by the OpenCL kernel object.
#' @param kernel An opencl_kernel object
cl.release.kernel <- function(kernel){
  stopifnot("opencl_kernel" %in% class(kernel))
  .Call("release_kernel", kernel)
}

#' @title Get OpenCL kernel
#' @description Extracts an OpenCL kernel from a program
#' @details Finds the specified kernel in the given program. The program must have been built.
#' @param program An opencl_program object
#' @param name The name of the kernel to get.
cl.create.kernel <- function(program, name){
  stopifnot("opencl_program" %in% class(program), class(name) == "character", 
            length(name) == 1)
  kernel <- .checkerror(.Call("extract_kernel", program, name))
  class(kernel) <- "opencl_kernel"
  reg.finalizer(kernel, cl.release.kernel, T)
  return(kernel)
}

#' @title Get OpenCL kernel info
#' @description Gets information from clGetKernelInfo call
#' @param device An opencl_kernel object
#' @param what An OpenCL kernel attribute as a string
#' @return Requested information about the kernel
cl.kernel.getattr <- function(kernel, what){
  stopifnot("opencl_kernel" %in% class(kernel), 
            class(what) == "character",
            length(what) == 1)
  return(.checkerror(.Call("get_kernel_property", kernel, what)))
}

#' @title Execute OpenCL kernel
#' @description Enqueues an OpenCL kernel to be executed via clEnqueueNDRangeKernel.
#' @param queue An opencl_command_queue object. The queue the operation will be put on.
#' @param kernel The kernel to execute. An opencl_kernel object
#' @param dimensions The dimensions of the kernel
#' @param global_work_size The number of global threads in each dimension
#' @param arguments A list of kernel arguments. Must be all opencl_buffer, scalar int, scalar double, scalar float or scalar raw objects. Any names are ignored, the arguments
#' are passed by position. 
#' @param local_work_size The number of local threads in each dimension inside a work-group. If left NULL, the OpenCL runtime will pick a value.
#' @param global_work_offset The starting offset of the global thread ids in each dimension. If left as NULL, defaults to 0 in each dimension.
#' @param waitlist OpenCL events that must complete before this operation can start.
#' Not currently used, included for future use.
#' @param return_event Schedule the kernel asynchronously and return an opencl event object.
#' Not currently used, included for future use.
#' 
cl.enqueue.kernel <- function(queue, kernel, dimensions, global_work_size, arguments, local_work_size = NULL, 
                              global_work_offset = NULL,
                              waitlist = NULL,
                              return_event = NULL){
  stopifnot(class(queue) == "opencl_command_queue",
            class(kernel) == "opencl_kernel",
            class(dimensions) == "integer",
            class(global_work_size) == "integer",
            class(arguments) == "list",
            all(sapply(X = arguments, FUN = function(x) class(x) %in% c("opencl_buffer", "integer", "raw", "double", "float32"))),
            length(arguments) == kernel$CL_KERNEL_NUM_ARGS,
            length(global_work_size) == dimensions)
  arguments <- lapply(X = arguments, FUN = function(x) if(class(x) == "float32"){x@Data}else{x})
  
  if(!is.null(local_work_size)) stopifnot(class(local_work_size) == "integer")
  if(!is.null(global_work_offset)) stopifnot(class(global_work_offset) == "integer")
  if(!is.null(waitlist)) warning("Waitlist not yet implemented")
  if(!is.null(return_event)) warning("Returning an event not yet implemented")
  
  dimensions <- dimensions[[1]]
  
  if(is.null(global_work_offset)) global_work_offset <- rep(0L, dimensions)
  status = .Call("enqueue_kern", queue, kernel, dimensions, global_work_size, 
                 arguments, local_work_size, global_work_offset)
  if(status != 0){
    stop(paste0("OpenCL Error: ", status))
  }
  return(status)
}

#' @title Execute an OpenCL kernel with less overhead
#' @description Just like cl.enqueue.kernel, but without expensive calls to stopifnot. 
#' @details cl.enqueue.kernel makes calls to stopifnot to ensure the types of arguments. This introduces significant overhead,
#' so this function is without any type-checking, at the risk of crashing R if the wrong types are given. Note that
#' global_work_offset is mandatory here.
#' @param queue An opencl_command_queue object. The queue the operation will be put on.
#' @param kernel The kernel to execute. An opencl_kernel object
#' @param dimensions The dimensions of the kernel
#' @param global_work_size The number of global threads in each dimension
#' @param arguments A list of kernel arguments. Must be all opencl_buffer, scalar int, scalar double or scalar raw objects. Any names are ignored, the arguments
#' are passed by position. Scalar floats shoud have the underlying bits extracted into an int.
#' @param local_work_size The number of local threads in each dimension inside a work-group. If left NULL, the OpenCL runtime will pick a value.
#' @param global_work_offset The starting offset of the global thread ids in each dimension.
#' @param waitlist OpenCL events that must complete before this operation can start.
#' Not currently used, included for future use.
#' @param return_event Schedule the kernel asynchronously and return an opencl event object.
#' Not currently used, included for future use.
cl.enqueue.kernel.unsafe <- function(queue, kernel, dimensions, global_work_size, arguments, local_work_size = NULL, 
                                     global_work_offset,
                                     waitlist = NULL,
                                     return_event = NULL){
  status = .Call("enqueue_kern", queue, kernel, dimensions, global_work_size, 
                 arguments, local_work_size, global_work_offset)
  if(status != 0){
    stop(paste0("OpenCL Error: ", status))
  }
  return(status)
}
