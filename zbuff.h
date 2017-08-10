/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/* ***************************************************************
*  NOTES/WARNINGS
******************************************************************/
/* The streaming API defined here is deprecated.
 * Consider migrating towards ZSTD1_compressStream() API in `zstd.h`
 * See 'lib/README.md'.
 *****************************************************************/


#if defined (__cplusplus)
extern "C" {
#endif

#ifndef ZSTD1_BUFFERED_H_23987
#define ZSTD1_BUFFERED_H_23987

/* *************************************
*  Dependencies
***************************************/
#include <stddef.h>      /* size_t */
#include "zstd.h"        /* ZSTD1_CStream, ZSTD1_DStream, ZSTD1LIB_API */


/* ***************************************************************
*  Compiler specifics
*****************************************************************/
/* Deprecation warnings */
/* Should these warnings be a problem,
   it is generally possible to disable them,
   typically with -Wno-deprecated-declarations for gcc
   or _CRT_SECURE_NO_WARNINGS in Visual.
   Otherwise, it's also possible to define ZBUFF1_DISABLE_DEPRECATE_WARNINGS */
#ifdef ZBUFF1_DISABLE_DEPRECATE_WARNINGS
#  define ZBUFF1_DEPRECATED(message) ZSTD1LIB_API  /* disable deprecation warnings */
#else
#  if defined (__cplusplus) && (__cplusplus >= 201402) /* C++14 or greater */
#    define ZBUFF1_DEPRECATED(message) [[deprecated(message)]] ZSTD1LIB_API
#  elif (defined(__GNUC__) && (__GNUC__ >= 5)) || defined(__clang__)
#    define ZBUFF1_DEPRECATED(message) ZSTD1LIB_API __attribute__((deprecated(message)))
#  elif defined(__GNUC__) && (__GNUC__ >= 3)
#    define ZBUFF1_DEPRECATED(message) ZSTD1LIB_API __attribute__((deprecated))
#  elif defined(_MSC_VER)
#    define ZBUFF1_DEPRECATED(message) ZSTD1LIB_API __declspec(deprecated(message))
#  else
#    pragma message("WARNING: You need to implement ZBUFF1_DEPRECATED for this compiler")
#    define ZBUFF1_DEPRECATED(message) ZSTD1LIB_API
#  endif
#endif /* ZBUFF1_DISABLE_DEPRECATE_WARNINGS */


/* *************************************
*  Streaming functions
***************************************/
/* This is the easier "buffered" streaming API,
*  using an internal buffer to lift all restrictions on user-provided buffers
*  which can be any size, any place, for both input and output.
*  ZBUFF1 and ZSTD1 are 100% interoperable,
*  frames created by one can be decoded by the other one */

typedef ZSTD1_CStream ZBUFF1_CCtx;
ZBUFF1_DEPRECATED("use ZSTD1_createCStream") ZBUFF1_CCtx* ZBUFF1_createCCtx(void);
ZBUFF1_DEPRECATED("use ZSTD1_freeCStream")   size_t      ZBUFF1_freeCCtx(ZBUFF1_CCtx* cctx);

ZBUFF1_DEPRECATED("use ZSTD1_initCStream")           size_t ZBUFF1_compressInit(ZBUFF1_CCtx* cctx, int compressionLevel);
ZBUFF1_DEPRECATED("use ZSTD1_initCStream_usingDict") size_t ZBUFF1_compressInitDictionary(ZBUFF1_CCtx* cctx, const void* dict, size_t dictSize, int compressionLevel);

ZBUFF1_DEPRECATED("use ZSTD1_compressStream") size_t ZBUFF1_compressContinue(ZBUFF1_CCtx* cctx, void* dst, size_t* dstCapacityPtr, const void* src, size_t* srcSizePtr);
ZBUFF1_DEPRECATED("use ZSTD1_flushStream")    size_t ZBUFF1_compressFlush(ZBUFF1_CCtx* cctx, void* dst, size_t* dstCapacityPtr);
ZBUFF1_DEPRECATED("use ZSTD1_endStream")      size_t ZBUFF1_compressEnd(ZBUFF1_CCtx* cctx, void* dst, size_t* dstCapacityPtr);

/*-*************************************************
*  Streaming compression - howto
*
*  A ZBUFF1_CCtx object is required to track streaming operation.
*  Use ZBUFF1_createCCtx() and ZBUFF1_freeCCtx() to create/release resources.
*  ZBUFF1_CCtx objects can be reused multiple times.
*
*  Start by initializing ZBUF_CCtx.
*  Use ZBUFF1_compressInit() to start a new compression operation.
*  Use ZBUFF1_compressInitDictionary() for a compression which requires a dictionary.
*
*  Use ZBUFF1_compressContinue() repetitively to consume input stream.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written within *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to present again remaining data.
*  The content of `dst` will be overwritten (up to *dstCapacityPtr) at each call, so save its content if it matters or change @dst .
*  @return : a hint to preferred nb of bytes to use as input for next function call (it's just a hint, to improve latency)
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  At any moment, it's possible to flush whatever data remains within buffer, using ZBUFF1_compressFlush().
*  The nb of bytes written into `dst` will be reported into *dstCapacityPtr.
*  Note that the function cannot output more than *dstCapacityPtr,
*  therefore, some content might still be left into internal buffer if *dstCapacityPtr is too small.
*  @return : nb of bytes still present into internal buffer (0 if it's empty)
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  ZBUFF1_compressEnd() instructs to finish a frame.
*  It will perform a flush and write frame epilogue.
*  The epilogue is required for decoders to consider a frame completed.
*  Similar to ZBUFF1_compressFlush(), it may not be able to output the entire internal buffer content if *dstCapacityPtr is too small.
*  In which case, call again ZBUFF1_compressFlush() to complete the flush.
*  @return : nb of bytes still present into internal buffer (0 if it's empty)
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  Hint : _recommended buffer_ sizes (not compulsory) : ZBUFF1_recommendedCInSize() / ZBUFF1_recommendedCOutSize()
*  input : ZBUFF1_recommendedCInSize==128 KB block size is the internal unit, use this value to reduce intermediate stages (better latency)
*  output : ZBUFF1_recommendedCOutSize==ZSTD1_compressBound(128 KB) + 3 + 3 : ensures it's always possible to write/flush/end a full block. Skip some buffering.
*  By using both, it ensures that input will be entirely consumed, and output will always contain the result, reducing intermediate buffering.
* **************************************************/


typedef ZSTD1_DStream ZBUFF1_DCtx;
ZBUFF1_DEPRECATED("use ZSTD1_createDStream") ZBUFF1_DCtx* ZBUFF1_createDCtx(void);
ZBUFF1_DEPRECATED("use ZSTD1_freeDStream")   size_t      ZBUFF1_freeDCtx(ZBUFF1_DCtx* dctx);

ZBUFF1_DEPRECATED("use ZSTD1_initDStream")           size_t ZBUFF1_decompressInit(ZBUFF1_DCtx* dctx);
ZBUFF1_DEPRECATED("use ZSTD1_initDStream_usingDict") size_t ZBUFF1_decompressInitDictionary(ZBUFF1_DCtx* dctx, const void* dict, size_t dictSize);

ZBUFF1_DEPRECATED("use ZSTD1_decompressStream") size_t ZBUFF1_decompressContinue(ZBUFF1_DCtx* dctx,
                                            void* dst, size_t* dstCapacityPtr,
                                      const void* src, size_t* srcSizePtr);

/*-***************************************************************************
*  Streaming decompression howto
*
*  A ZBUFF1_DCtx object is required to track streaming operations.
*  Use ZBUFF1_createDCtx() and ZBUFF1_freeDCtx() to create/release resources.
*  Use ZBUFF1_decompressInit() to start a new decompression operation,
*   or ZBUFF1_decompressInitDictionary() if decompression requires a dictionary.
*  Note that ZBUFF1_DCtx objects can be re-init multiple times.
*
*  Use ZBUFF1_decompressContinue() repetitively to consume your input.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written by modifying *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to present remaining input again.
*  The content of `dst` will be overwritten (up to *dstCapacityPtr) at each function call, so save its content if it matters, or change `dst`.
*  @return : 0 when a frame is completely decoded and fully flushed,
*            1 when there is still some data left within internal buffer to flush,
*            >1 when more data is expected, with value being a suggested next input size (it's just a hint, which helps latency),
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  Hint : recommended buffer sizes (not compulsory) : ZBUFF1_recommendedDInSize() and ZBUFF1_recommendedDOutSize()
*  output : ZBUFF1_recommendedDOutSize== 128 KB block size is the internal unit, it ensures it's always possible to write a full block when decoded.
*  input  : ZBUFF1_recommendedDInSize == 128KB + 3;
*           just follow indications from ZBUFF1_decompressContinue() to minimize latency. It should always be <= 128 KB + 3 .
* *******************************************************************************/


/* *************************************
*  Tool functions
***************************************/
ZBUFF1_DEPRECATED("use ZSTD1_isError")      unsigned ZBUFF1_isError(size_t errorCode);
ZBUFF1_DEPRECATED("use ZSTD1_getErrorName") const char* ZBUFF1_getErrorName(size_t errorCode);

/** Functions below provide recommended buffer sizes for Compression or Decompression operations.
*   These sizes are just hints, they tend to offer better latency */
ZBUFF1_DEPRECATED("use ZSTD1_CStreamInSize")  size_t ZBUFF1_recommendedCInSize(void);
ZBUFF1_DEPRECATED("use ZSTD1_CStreamOutSize") size_t ZBUFF1_recommendedCOutSize(void);
ZBUFF1_DEPRECATED("use ZSTD1_DStreamInSize")  size_t ZBUFF1_recommendedDInSize(void);
ZBUFF1_DEPRECATED("use ZSTD1_DStreamOutSize") size_t ZBUFF1_recommendedDOutSize(void);

#endif  /* ZSTD1_BUFFERED_H_23987 */


#ifdef ZBUFF1_STATIC_LINKING_ONLY
#ifndef ZBUFF1_STATIC_H_30298098432
#define ZBUFF1_STATIC_H_30298098432

/* ====================================================================================
 * The definitions in this section are considered experimental.
 * They should never be used in association with a dynamic library, as they may change in the future.
 * They are provided for advanced usages.
 * Use them only in association with static linking.
 * ==================================================================================== */

/*--- Dependency ---*/
#define ZSTD1_STATIC_LINKING_ONLY   /* ZSTD1_parameters, ZSTD1_customMem */
#include "zstd.h"


/*--- Custom memory allocator ---*/
/*! ZBUFF1_createCCtx_advanced() :
 *  Create a ZBUFF1 compression context using external alloc and free functions */
ZBUFF1_DEPRECATED("use ZSTD1_createCStream_advanced") ZBUFF1_CCtx* ZBUFF1_createCCtx_advanced(ZSTD1_customMem customMem);

/*! ZBUFF1_createDCtx_advanced() :
 *  Create a ZBUFF1 decompression context using external alloc and free functions */
ZBUFF1_DEPRECATED("use ZSTD1_createDStream_advanced") ZBUFF1_DCtx* ZBUFF1_createDCtx_advanced(ZSTD1_customMem customMem);


/*--- Advanced Streaming Initialization ---*/
ZBUFF1_DEPRECATED("use ZSTD1_initDStream_usingDict") size_t ZBUFF1_compressInit_advanced(ZBUFF1_CCtx* zbc,
                                               const void* dict, size_t dictSize,
                                               ZSTD1_parameters params, unsigned long long pledgedSrcSize);


#endif    /* ZBUFF1_STATIC_H_30298098432 */
#endif    /* ZBUFF1_STATIC_LINKING_ONLY */


#if defined (__cplusplus)
}
#endif
