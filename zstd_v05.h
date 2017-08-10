/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1v05_H
#define ZSTD1v05_H

#if defined (__cplusplus)
extern "C" {
#endif

/*-*************************************
*  Dependencies
***************************************/
#include <stddef.h>   /* size_t */
#include "mem.h"      /* U64, U32 */


/* *************************************
*  Simple functions
***************************************/
/*! ZSTD1v05_decompress() :
    `compressedSize` : is the _exact_ size of the compressed blob, otherwise decompression will fail.
    `dstCapacity` must be large enough, equal or larger than originalSize.
    @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
              or an errorCode if it fails (which can be tested using ZSTD1v05_isError()) */
size_t ZSTD1v05_decompress( void* dst, size_t dstCapacity,
                     const void* src, size_t compressedSize);

/**
ZSTD1v05_getFrameSrcSize() : get the source length of a ZSTD1 frame
    compressedSize : The size of the 'src' buffer, at least as large as the frame pointed to by 'src'
    return : the number of bytes that would be read to decompress this frame
             or an errorCode if it fails (which can be tested using ZSTD1v05_isError())
*/
size_t ZSTD1v05_findFrameCompressedSize(const void* src, size_t compressedSize);

/* *************************************
*  Helper functions
***************************************/
/* Error Management */
unsigned    ZSTD1v05_isError(size_t code);          /*!< tells if a `size_t` function result is an error code */
const char* ZSTD1v05_getErrorName(size_t code);     /*!< provides readable string for an error code */


/* *************************************
*  Explicit memory management
***************************************/
/** Decompression context */
typedef struct ZSTD1v05_DCtx_s ZSTD1v05_DCtx;
ZSTD1v05_DCtx* ZSTD1v05_createDCtx(void);
size_t ZSTD1v05_freeDCtx(ZSTD1v05_DCtx* dctx);      /*!< @return : errorCode */

/** ZSTD1v05_decompressDCtx() :
*   Same as ZSTD1v05_decompress(), but requires an already allocated ZSTD1v05_DCtx (see ZSTD1v05_createDCtx()) */
size_t ZSTD1v05_decompressDCtx(ZSTD1v05_DCtx* ctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize);


/*-***********************
*  Simple Dictionary API
*************************/
/*! ZSTD1v05_decompress_usingDict() :
*   Decompression using a pre-defined Dictionary content (see dictBuilder).
*   Dictionary must be identical to the one used during compression, otherwise regenerated data will be corrupted.
*   Note : dict can be NULL, in which case, it's equivalent to ZSTD1v05_decompressDCtx() */
size_t ZSTD1v05_decompress_usingDict(ZSTD1v05_DCtx* dctx,
                                            void* dst, size_t dstCapacity,
                                      const void* src, size_t srcSize,
                                      const void* dict,size_t dictSize);

/*-************************
*  Advanced Streaming API
***************************/
typedef enum { ZSTD1v05_fast, ZSTD1v05_greedy, ZSTD1v05_lazy, ZSTD1v05_lazy2, ZSTD1v05_btlazy2, ZSTD1v05_opt, ZSTD1v05_btopt } ZSTD1v05_strategy;
typedef struct {
    U64 srcSize;
    U32 windowLog;     /* the only useful information to retrieve */
    U32 contentLog; U32 hashLog; U32 searchLog; U32 searchLength; U32 targetLength; ZSTD1v05_strategy strategy;
} ZSTD1v05_parameters;
size_t ZSTD1v05_getFrameParams(ZSTD1v05_parameters* params, const void* src, size_t srcSize);

size_t ZSTD1v05_decompressBegin_usingDict(ZSTD1v05_DCtx* dctx, const void* dict, size_t dictSize);
void   ZSTD1v05_copyDCtx(ZSTD1v05_DCtx* dstDCtx, const ZSTD1v05_DCtx* srcDCtx);
size_t ZSTD1v05_nextSrcSizeToDecompress(ZSTD1v05_DCtx* dctx);
size_t ZSTD1v05_decompressContinue(ZSTD1v05_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize);


/*-***********************
*  ZBUFF1 API
*************************/
typedef struct ZBUFF1v05_DCtx_s ZBUFF1v05_DCtx;
ZBUFF1v05_DCtx* ZBUFF1v05_createDCtx(void);
size_t         ZBUFF1v05_freeDCtx(ZBUFF1v05_DCtx* dctx);

size_t ZBUFF1v05_decompressInit(ZBUFF1v05_DCtx* dctx);
size_t ZBUFF1v05_decompressInitDictionary(ZBUFF1v05_DCtx* dctx, const void* dict, size_t dictSize);

size_t ZBUFF1v05_decompressContinue(ZBUFF1v05_DCtx* dctx,
                                            void* dst, size_t* dstCapacityPtr,
                                      const void* src, size_t* srcSizePtr);

/*-***************************************************************************
*  Streaming decompression
*
*  A ZBUFF1v05_DCtx object is required to track streaming operations.
*  Use ZBUFF1v05_createDCtx() and ZBUFF1v05_freeDCtx() to create/release resources.
*  Use ZBUFF1v05_decompressInit() to start a new decompression operation,
*   or ZBUFF1v05_decompressInitDictionary() if decompression requires a dictionary.
*  Note that ZBUFF1v05_DCtx objects can be reused multiple times.
*
*  Use ZBUFF1v05_decompressContinue() repetitively to consume your input.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written by modifying *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to present remaining input again.
*  The content of @dst will be overwritten (up to *dstCapacityPtr) at each function call, so save its content if it matters or change @dst.
*  @return : a hint to preferred nb of bytes to use as input for next function call (it's only a hint, to help latency)
*            or 0 when a frame is completely decoded
*            or an error code, which can be tested using ZBUFF1v05_isError().
*
*  Hint : recommended buffer sizes (not compulsory) : ZBUFF1v05_recommendedDInSize() / ZBUFF1v05_recommendedDOutSize()
*  output : ZBUFF1v05_recommendedDOutSize==128 KB block size is the internal unit, it ensures it's always possible to write a full block when decoded.
*  input  : ZBUFF1v05_recommendedDInSize==128Kb+3; just follow indications from ZBUFF1v05_decompressContinue() to minimize latency. It should always be <= 128 KB + 3 .
* *******************************************************************************/


/* *************************************
*  Tool functions
***************************************/
unsigned ZBUFF1v05_isError(size_t errorCode);
const char* ZBUFF1v05_getErrorName(size_t errorCode);

/** Functions below provide recommended buffer sizes for Compression or Decompression operations.
*   These sizes are just hints, and tend to offer better latency */
size_t ZBUFF1v05_recommendedDInSize(void);
size_t ZBUFF1v05_recommendedDOutSize(void);



/*-*************************************
*  Constants
***************************************/
#define ZSTD1v05_MAGICNUMBER 0xFD2FB525   /* v0.5 */




#if defined (__cplusplus)
}
#endif

#endif  /* ZSTD1v0505_H */
