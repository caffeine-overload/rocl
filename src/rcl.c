#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <CL/cl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>



#define __RE(stacknum) ({if(status != CL_SUCCESS){SEXP retval = set_error_info(R_NilValue, status); UNPROTECT( stacknum + 2); return retval;}})

#define __CE if(status != CL_SUCCESS)
#define __AV(type, len) (PROTECT(allocVector(type, len)))
#define _l(len) (__AV(VECSXP, len))
#define _i(len) (__AV(INTSXP, len))
#define _s(len) (__AV(STRSXP, len))
#define __i(n, len) SEXP n = _i(len)
#define __l(n, len) SEXP n = _l(len)
#define __s(n, len) SEXP n = _s(len)

#define sfl(c, n) for(int c = 0; c < n; c++)

#define nilptr(P) R_ExternalPtrAddr(P) == NULL
#define _retnp(P) if(nilptr(P)){return R_NilValue;}

// OpenCL stuff

const char* devtype2str(cl_device_type devtype){
  static char* types[] = {"CPU", "GPU", "ACCELERATOR", "DEFAULT"};
  switch(devtype){
  case CL_DEVICE_TYPE_CPU:
    return types[0];
  case CL_DEVICE_TYPE_GPU:
    return types[1];
  case CL_DEVICE_TYPE_ACCELERATOR:
    return types[2];
  default:
    return types[3];
  }
}

// Interface to R

//Adds 2 to the protect stack
SEXP set_error_info(SEXP value, cl_uint status){
  SEXP statusvec = _i(1); //PROTECT(allocVector(INTSXP, 1));
  INTEGER(statusvec)[0] = (int) status;
  SEXP vec = _l(2); //PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(vec, 0, value);
  SET_VECTOR_ELT(vec, 1, statusvec);
  return vec;
}

SEXP get_num_cl_platforms() {
  cl_uint status;
  cl_uint num = 0;
  status = clGetPlatformIDs(0, NULL, &num);
  
  __RE(0);
  
  __i(snum, 1); // SEXP snum = _i(1); //PROTECT(allocVector(INTSXP, 1));
  INTEGER(snum)[0] = (int) num;
  
  SEXP res = set_error_info(snum, status);
  
  UNPROTECT(3);
  return(res);
}

SEXP release_context(SEXP Rptr){
  //printf("Garbage collected context");
  _retnp(Rptr);
  cl_context* ctx = (cl_context*)R_ExternalPtrAddr(Rptr);
  clReleaseContext(*ctx);
  free(ctx);
  R_ClearExternalPtr(Rptr);
  return R_NilValue;
}

SEXP simple_free_platform(SEXP Rptr){
  //printf("Garbage collected platform\n");
  cl_platform_id* platform = (cl_platform_id*) R_ExternalPtrAddr(Rptr);
  
  free(platform);
  
  R_ClearExternalPtr(Rptr);
  return R_NilValue;
}

SEXP simple_free_device(SEXP Rptr){
  //printf("Garbage collected device\n");
  cl_device_id* platform = (cl_device_id*) R_ExternalPtrAddr(Rptr);
  
  free(platform);
  
  R_ClearExternalPtr(Rptr);
  return R_NilValue;
}

SEXP get_platform_pointers(){
  cl_uint status;
  cl_uint num = 0;
  cl_platform_id* platforms;
  
  //Get number of platforms
  status = clGetPlatformIDs(0, NULL, &num);
  __RE(0);
  
  
  //Get platforms
  platforms = (cl_platform_id*) malloc(sizeof(cl_platform_id) * num);
  status = clGetPlatformIDs(num, platforms, NULL);
  __CE{free(platforms); __RE(0);}
  
  //Assign them to R pointer objects
  __l(platform_list, num);
  for(int i = 0; i < num; i++){
    cl_platform_id* local = malloc(sizeof(cl_platform_id));
    *local = platforms[i];
    SEXP Rptr = PROTECT(R_MakeExternalPtr(local, R_NilValue, R_NilValue));
    SET_VECTOR_ELT(platform_list, i, Rptr);
  }
  free(platforms);
  SEXP retval = set_error_info(platform_list, 0);
  UNPROTECT(3 + num);
  return retval;
}

SEXP get_num_devices(SEXP platform_ptr){
  cl_uint status;
  cl_uint numdevices = 0;
  
  cl_platform_id* this_platform = (cl_platform_id*) R_ExternalPtrAddr(platform_ptr);
  
  status = clGetDeviceIDs(*this_platform, CL_DEVICE_TYPE_ALL, 0, NULL, &numdevices);
  
  __RE(0);
  
  
  __i(snum, 1); 
  INTEGER(snum)[0] = (int) numdevices;
  
  SEXP res = set_error_info(snum, status);
  
  UNPROTECT(3);
  return(res);
  
}

SEXP get_device_pointers(SEXP platform_ptr){
  cl_uint status;
  cl_uint numdevices = 0;
  cl_device_id* devices;
  
  cl_platform_id* this_platform = (cl_platform_id*) R_ExternalPtrAddr(platform_ptr);
  
  status = clGetDeviceIDs(*this_platform, CL_DEVICE_TYPE_ALL, 0, NULL, &numdevices);

  __RE(0);
  devices = (cl_device_id*) malloc(sizeof(cl_device_id) * numdevices);
  status = clGetDeviceIDs(*this_platform, CL_DEVICE_TYPE_ALL, numdevices, devices, NULL);
  __CE{free(devices); __RE(0);}
  
  
  __l(l_devices, numdevices);
  for(int i = 0; i < numdevices; i++){
    cl_device_id* dev = malloc(sizeof(cl_device_id));
    *dev = devices[i];
    SEXP Rptr = PROTECT(R_MakeExternalPtr(dev, R_NilValue, R_NilValue));
    SET_VECTOR_ELT(l_devices, i, Rptr);
  }
  
  free(devices);
  SEXP retval = set_error_info(l_devices, 0);
  UNPROTECT(3 + numdevices);
  return retval;
}



#define cldevinfo(dev, what, sizev, destptr) clGetDeviceInfo(dev, what, 0, NULL, &sizev); \
  destptr = malloc(sizev);                                     \
  clGetDeviceInfo(dev, what, sizev, destptr, NULL)             

#define cldi(what, destptr) cldevinfo(*this_dev, what, ressize, destptr)

#define strcase(str) else if (strcmp(sprop, #str) == 0)

#define clplatinfo(plat, what, sizev, destptr) clGetPlatformInfo(plat, what, 0, NULL, &sizev); \
  destptr = malloc(sizev);                                     \
  clGetPlatformInfo(plat, what, sizev, destptr, NULL)

SEXP get_platform_property(SEXP prop_ptr, SEXP prop){
  cl_uint status;
  void* info;
  int ifree = 1;
  size_t ressize;
  
  const char* sprop = CHAR(STRING_ELT(prop, 0));
  
  cl_platform_id* this_plat = (cl_platform_id*) R_ExternalPtrAddr(prop_ptr);
  
  SEXP retval;
  
  if(0){}
  strcase(CL_PLATFORM_NAME){
    clplatinfo(*this_plat, CL_PLATFORM_NAME, ressize, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_PLATFORM_VENDOR){
    clplatinfo(*this_plat, CL_PLATFORM_VENDOR, ressize, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_PLATFORM_VERSION){
    clplatinfo(*this_plat, CL_PLATFORM_VERSION, ressize, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_PLATFORM_PROFILE){
    clplatinfo(*this_plat, CL_PLATFORM_PROFILE, ressize, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_PLATFORM_EXTENSIONS){
    clplatinfo(*this_plat, CL_PLATFORM_EXTENSIONS, ressize, info);
    retval = PROTECT(mkString((char*)info));
  }else{
    return R_NilValue;
  }
  if(ifree) free(info);
  UNPROTECT(1);
  return retval;
}

SEXP get_device_property(SEXP dev_ptr, SEXP prop){
  cl_uint status;
  void* info;
  int ifree = 1;
  size_t ressize;
  
  const char* sprop = CHAR(STRING_ELT(prop, 0));
  
  cl_device_id* this_dev = (cl_device_id*) R_ExternalPtrAddr(dev_ptr);
  
  SEXP retval;
  
  if(0){}
  strcase(CL_DEVICE_NAME){
    cldi(CL_DEVICE_NAME, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_DEVICE_EXTENSIONS){
    cldi(CL_DEVICE_EXTENSIONS, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_DEVICE_PROFILE){
    cldi(CL_DEVICE_PROFILE, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_DEVICE_VENDOR){
    cldi(CL_DEVICE_VENDOR, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_DEVICE_VERSION){
    cldi(CL_DEVICE_VERSION, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_DRIVER_VERSION){
    cldi(CL_DRIVER_VERSION, info);
    retval = PROTECT(mkString((char*)info));
  }strcase(CL_DEVICE_TYPE){
    cl_device_type devtype;
    status = clGetDeviceInfo(*this_dev, CL_DEVICE_TYPE, sizeof(cl_device_type), &devtype, NULL);
    retval = PROTECT(mkString(devtype2str(devtype)));
    ifree = 0;
  }else{
    return R_NilValue;
  }
  if(ifree) free(info);
  UNPROTECT(1);
  return retval;
  }
  
  
SEXP get_cl_context(SEXP devs){
  int numdevs = Rf_length(devs);
  cl_device_id devices[numdevs];
  for(int i = 0; i < numdevs; i++){
    cl_device_id* rptr = R_ExternalPtrAddr(VECTOR_ELT(devs, i));
    devices[i] = *rptr;
  }
  cl_uint status = 0;
  cl_context* hctx = malloc(sizeof(cl_context));
  *hctx = clCreateContext(NULL, numdevs, devices, NULL, NULL, &status);
  __CE{free(hctx); __RE(0);}
  
  SEXP Rptr = PROTECT(R_MakeExternalPtr(hctx, R_NilValue, R_NilValue));
  SEXP retval = set_error_info(Rptr, 0);
  UNPROTECT(3);
  return retval;
}

cl_mem_flags int2flag(int iflag){
    switch(iflag){
        case 0:
          return CL_MEM_READ_WRITE;
        case 1:
          return CL_MEM_WRITE_ONLY;
        case 2:
          return CL_MEM_READ_ONLY;
        case 3:
          return CL_MEM_USE_HOST_PTR;
        case 4:
          return CL_MEM_ALLOC_HOST_PTR;
        case 5:
          return CL_MEM_COPY_HOST_PTR;
        case 6:
          return CL_MEM_HOST_WRITE_ONLY;
        case 7:
          return CL_MEM_HOST_READ_ONLY;
        case 8:
          return CL_MEM_HOST_NO_ACCESS;
    }
}

SEXP release_buffer(SEXP Rptr){
    //printf("Garbage collected buffer");
    _retnp(Rptr);
    cl_mem* ctx = (cl_mem*)R_ExternalPtrAddr(Rptr);
    clReleaseMemObject(*ctx);
    free(ctx);
    R_ClearExternalPtr(Rptr);
    return R_NilValue;
}

SEXP get_cl_buf(SEXP rctx, SEXP rflags, SEXP rsize, SEXP vector) {
    //Size of buffer
    size_t vecsize = (size_t) asInteger(rsize);

    //Extract flags
    int numflags = Rf_length(rflags);
    cl_mem_flags buf_flags = int2flag(INTEGER(rflags)[0]);
    for(int i = 1; i < numflags; i++){
        buf_flags = buf_flags | int2flag(INTEGER(rflags)[i]);
    }

    //Extract vector pointer
    void* hostptr;
    if(Rf_isInteger(vector)){
        hostptr = INTEGER(vector);
    }else if(Rf_isNumeric(vector)){
        hostptr = REAL(vector);
    }else{
        hostptr = NULL;
    }

    //Get context
    cl_context* ctx = (cl_context*) R_ExternalPtrAddr(rctx);

    //Create buffer
    cl_int status;
    cl_mem* buffer = malloc(sizeof(cl_mem));
    *buffer = clCreateBuffer(*ctx, buf_flags, vecsize, hostptr, &status);
    __CE{free(buffer); __RE(0);}
    SEXP rbufptr = PROTECT(R_MakeExternalPtr(buffer, R_NilValue, vector));
    SEXP retval = set_error_info(rbufptr, 0);
    UNPROTECT(3);
    return retval;
}

SEXP release_cl_queue(SEXP Rptr){
    //printf("Garbage collected queue");
    _retnp(Rptr);
    cl_command_queue* q = (cl_command_queue*)R_ExternalPtrAddr(Rptr);
    clReleaseCommandQueue(*q);
    free(q);
    R_ClearExternalPtr(Rptr);
    return R_NilValue;
}

SEXP get_cl_queue(SEXP rctx, SEXP rdev, SEXP rin_order, SEXP rprofiling, SEXP ron_device){
    //Extract booleans and pointers
    cl_context* ctx = (cl_context*) R_ExternalPtrAddr(rctx);
    cl_device_id* dev = (cl_device_id*) R_ExternalPtrAddr(rdev);
    //These aren't used yet
    int in_order = asLogical(rin_order);
    if(!in_order) Rf_warning("Out of order not yet implemented");
    int profiling = asLogical(rprofiling);
    if(profiling) Rf_warning("Profiling not yet implemented");
    int on_device = asLogical(ron_device);
    if(on_device) Rf_warning("Device queue not yet implemented");

    cl_int status;
    cl_command_queue* q = malloc(sizeof(cl_command_queue));
    *q = clCreateCommandQueueWithProperties(*ctx, *dev, NULL, &status);
    __CE{free(q); __RE(0);}
    SEXP rq = PROTECT(R_MakeExternalPtr(q, R_NilValue, rctx));
    SEXP retval = set_error_info(rq, 0);
    UNPROTECT(3);
    return retval;
}

SEXP release_cl_program(SEXP Rptr){
    //printf("Garbage collected program");
    _retnp(Rptr);
    cl_program* q = (cl_program*)R_ExternalPtrAddr(Rptr);
    clReleaseProgram(*q);
    free(q);
    R_ClearExternalPtr(Rptr);
    return R_NilValue;
}

SEXP create_cl_program(SEXP rctx, SEXP source){
    const char* cl_src = CHAR(asChar(source));
    cl_context* ctx = (cl_context*) R_ExternalPtrAddr(rctx);
    cl_int status;

    cl_program* prog = malloc(sizeof(cl_program));
    *prog = clCreateProgramWithSource(*ctx, 1, (const char**) &cl_src, NULL, &status);
    __CE{free(prog); __RE(0);}
    SEXP res = PROTECT(R_MakeExternalPtr(prog, R_NilValue, rctx));

    SEXP retval = set_error_info(res, 0);
    UNPROTECT(3);
    return retval;
}

SEXP create_cl_from_binary(SEXP path, SEXP rctx, SEXP devs){
    cl_context* ctx = (cl_context*) R_ExternalPtrAddr(rctx);
    const char* fpath = CHAR(asChar(path));

    FILE *ptr;

    off_t file_size;

    ptr = fopen(fpath,"rb");

    fseeko(ptr, 0 , SEEK_END);
    file_size = ftello(ptr);
    rewind(ptr);
    char *buffer;
    buffer = (char*)malloc(file_size);

    size_t read = fread(buffer, 1, file_size, ptr);
    fclose(ptr);

    int numdevs = Rf_length(devs);
    cl_device_id devices[numdevs];
    for(int i = 0; i < numdevs; i++){
      cl_device_id* rptr = R_ExternalPtrAddr(VECTOR_ELT(devs, i));
      devices[i] = *rptr;
    }

    cl_int status;

    cl_program* prog = malloc(sizeof(cl_program));
    *prog = clCreateProgramWithBinary(*ctx, numdevs, (const cl_device_id *) devices, &read, (const unsigned char**)&buffer, NULL, &status);
    __CE{free(prog); free(buffer); __RE(0);}

  SEXP res = PROTECT(R_MakeExternalPtr(prog, R_NilValue, rctx));

  SEXP retval = set_error_info(res, 0);
  UNPROTECT(3);
  return retval;

}


SEXP build_cl_program(SEXP rprog, SEXP options){
    cl_int status = 0;
    cl_program* prog = (cl_program*) R_ExternalPtrAddr(rprog);
    const char* boptions = Rf_isNull(options) ? NULL : CHAR(asChar(options));

    cl_uint numdevs = 0;
    const cl_device_id* devl = NULL;

    status =  clBuildProgram(*prog, 0, NULL, boptions, NULL, NULL);
    return ScalarInteger((int)status);
}

SEXP write_buffer(SEXP rq, SEXP rbuf, SEXP rdata, SEXP rblocking, SEXP roffset, SEXP waitlist){
    cl_int status;
    cl_command_queue* q = (cl_command_queue*) R_ExternalPtrAddr(rq);
    cl_mem* buf = (cl_mem*) R_ExternalPtrAddr(rbuf);
    int iblock = asLogical(rblocking);
    cl_bool blocking = asLogical(rblocking) ? CL_TRUE : CL_FALSE;
    size_t offset = (size_t) asInteger(roffset);
    int datalen = Rf_length(rdata);
    size_t datasize;

    //If this is an async call, duplicate the vector
    //Not implemented yet
    if(!blocking) Rf_warning("Async not yet implemented");
    if(!Rf_isNull(waitlist)) Rf_warning("Waitlist not yet implemented");

    //Extract vector pointer
    const void* hostptr;
    if(Rf_isInteger(rdata)){
      hostptr = INTEGER(rdata);
      datasize = 4 * datalen;
    }else if(Rf_isNumeric(rdata)){
      hostptr = REAL(rdata);
      datasize = 8 * datalen;
    }else{
      hostptr = NULL;
    }

    status = clEnqueueWriteBuffer(*q, *buf, CL_TRUE, offset, datasize, hostptr, 0, NULL, NULL);

    return ScalarInteger((int)status);
}

SEXP read_buffer(SEXP rq, SEXP rbuf, SEXP rdestvec, SEXP roffset, SEXP rblocking, SEXP waitlist){
    cl_int status;
    cl_command_queue* q = (cl_command_queue*) R_ExternalPtrAddr(rq);
    cl_mem* buf = (cl_mem*) R_ExternalPtrAddr(rbuf);
    cl_bool blocking = asLogical(rblocking) ? CL_TRUE : CL_FALSE;
    size_t offset = (size_t) asInteger(roffset);
    int datalen = Rf_length(rdestvec);
    size_t datasize;

    //Not implemented yet
    if(!blocking) Rf_warning("Async not yet implemented");
    if(!Rf_isNull(waitlist)) Rf_warning("Waitlist not yet implemented");

    void* dest;
    if(Rf_isInteger(rdestvec)){
      dest = INTEGER(rdestvec);
      datasize = 4 * datalen;
    }else if(Rf_isNumeric(rdestvec)){
      dest = REAL(rdestvec);
      datasize = 8 * datalen;
    }

    status = clEnqueueReadBuffer(*q, *buf, CL_TRUE, offset, datasize, dest, 0, NULL, NULL);

    return ScalarInteger((int)status);
}

SEXP release_kernel(SEXP Rptr){
    //printf("Garbage collected kernel");
    _retnp(Rptr);
    cl_kernel* q = (cl_kernel*)R_ExternalPtrAddr(Rptr);
    clReleaseKernel(*q);
    free(q);
    R_ClearExternalPtr(Rptr);
    return R_NilValue;
}

SEXP extract_kernel(SEXP rprog, SEXP name){
    cl_int status;
    const char* kernel_name = CHAR(asChar(name));
    cl_program* prog = (cl_program*) R_ExternalPtrAddr(rprog);

    cl_kernel* kern = malloc(sizeof(cl_kernel));
    *kern = clCreateKernel(*prog, kernel_name, &status);
    __CE{free(kern); __RE(0);}
    SEXP rkern = PROTECT(R_MakeExternalPtr(kern, R_NilValue, rprog));
    SEXP ret = set_error_info(rkern, 0);
    UNPROTECT(3);
    return ret;
}

SEXP get_kernel_property(SEXP rkern, SEXP what){
    cl_int status;
    cl_kernel* kern = (cl_kernel*) R_ExternalPtrAddr(rkern);
    const char* sprop = CHAR(asChar(what));

    SEXP retval;
    size_t dsize;

    if(0){}
    strcase(CL_KERNEL_FUNCTION_NAME){
        status = clGetKernelInfo(*kern, CL_KERNEL_FUNCTION_NAME, 0, NULL, &dsize);
        __RE(0);
        char* info = malloc(dsize);
        status = clGetKernelInfo(*kern, CL_KERNEL_FUNCTION_NAME, dsize, info, NULL);
        __RE(0);
        SEXP x = PROTECT(mkString(info));
        free(info);
        retval = set_error_info(x, 0);
    }strcase(CL_KERNEL_NUM_ARGS){
        cl_uint number;
        status = clGetKernelInfo(*kern, CL_KERNEL_NUM_ARGS, sizeof(cl_uint), &number, NULL);
        __RE(0);
        SEXP x = PROTECT(ScalarInteger((int)number));
        retval = set_error_info(x, 0);
    }
    UNPROTECT(3);
    return retval;
}

SEXP enqueue_kern(SEXP rq, SEXP rkern, SEXP rdim, SEXP rglobal_work_size, SEXP r_bufs, SEXP rlocal_work_size, SEXP rglobal_work_offset){
    cl_int status;
    cl_command_queue* q = (cl_command_queue*) R_ExternalPtrAddr(rq);
    cl_kernel* kern = (cl_kernel*) R_ExternalPtrAddr(rkern);
    cl_uint work_dim = (cl_uint) asInteger(rdim);
    //int create_event = asLogical(give_event);

    size_t global_work_size[(int)work_dim];
    for(int i = 0; i < work_dim; i++){
        global_work_size[i] = INTEGER(rglobal_work_size)[i];
    }

    size_t global_work_offset[(int)work_dim];
    for(int i = 0; i < work_dim; i++){
        global_work_offset[i] = INTEGER(rglobal_work_offset)[i];
    }

    const size_t* ptr_local_work_size = NULL;
    if(!Rf_isNull(rlocal_work_size)) {
        size_t local_work_size[(int) work_dim];
        for (int i = 0; i < work_dim; i++) {
            local_work_size[i] = INTEGER(rlocal_work_size)[i];
        }
        ptr_local_work_size = local_work_size;
    }

    for(int i = 0; i < Rf_length(r_bufs); i++){
        cl_mem* buf = R_ExternalPtrAddr(VECTOR_ELT(r_bufs, i));
        status = clSetKernelArg(*kern, i, sizeof(cl_mem), buf);
        __RE(0);
    }

    //struct timeval stop, start;
    //gettimeofday(&start, NULL);

    status = clEnqueueNDRangeKernel(*q, *kern, work_dim, global_work_offset, global_work_size, ptr_local_work_size,
            0, NULL, NULL);

    clFinish(*q);

    //gettimeofday(&stop, NULL);
    //printf("took %lu\n", stop.tv_usec - start.tv_usec);

    return ScalarInteger((int) status);
}