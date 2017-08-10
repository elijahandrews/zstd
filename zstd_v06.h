/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1v06_H
#define ZSTD1v06_H

#if defined (__cplusplus)
extern "C" {
#endif

/*======  Dependency  ======*/
#include <stddef.h>   /* size_t */


/*======  Export for Windows  ======*/
/*!
*  ZSTD1v06_DLL_EXPORT :
*  Enable exporting of functions when building a Windows DLL
*/
#if defined(_WIN32) && defined(ZSTD1v06_DLL_EXPORT) && (ZSTD1v06_DLL_EXPORT==1)
#  define ZSTD1LIBv06_API __declspec(dllexport)
#else
#  define ZSTD1LIBv06_API
#endif


/* *************************************
*  Simple functions
***************************************/
/*! ZSTD1v06_decompress() :
    `compressedSize` : is the _exact_ size of the compressed blob, otherwise decompression will fail.
    `dstCapacity` must be large enough, equal or larger than originalSize.
    @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
              or an errorCode if it fails (which can be tested using ZSTD1v06_isError()) */
ZSTD1LIBv06_API size_t ZSTD1v06_decompress( void* dst, size_t dstCapacity,
                                    const void* src, size_t compressedSize);

/**
ZSTD1v06_getFrameSrcSize() : get the source length of a ZSTD1 frame
    compressedSize : The size of the 'src' buffer, at least as large as the frame pointed to by 'src'
    return : the number of bytes that would be read to decompress this frame
             or an errorCode if it fails (which can be tested using ZSTD1v06_isError())
*/
size_t ZSTD1v06_findFrameCompressedSize(const void* src, size_t compressedSize);

/* *************************************
*  Helper functions
***************************************/
ZSTD1LIBv06_API size_t      ZSTD1v06_compressBound(size_t srcSize); /*!< maximum compressed size (worst case scenario) */

/* Error Management */
ZSTD1LIBv06_API unsigned    ZSTD1v06_isError(size_t code);          /*!< tells if a `size_t` function result is an error code */
ZSTD1LIBv06_API const char* ZSTD1v06_getErrorName(size_t code);     /*!< provides readable string for an error code */


/* *************************************
*  Explicit memory management
***************************************/
/** Decompression context */
typedef struct ZSTD1v06_DCtx_s ZSTD1v06_DCtx;
ZSTD1LIBv06_API ZSTD1v06_DCtx* ZSTD1v06_createDCtx(void);
ZSTD1LIBv06_API size_t     ZSTD1v06_freeDCtx(ZSTD1v06_DCtx* dctx);      /*!< @return : errorCode */

/** ZSTD1v06_decompressDCtx() :
*   Same as ZSTD1v06_decompress(), but requires an already allocated ZSTD1v06_DCtx (see ZSTD1v06_createDCtx()) */
ZSTD1LIBv06_API size_t ZSTD1v06_decompressDCtx(ZSTD1v06_DCtx* ctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize);


/*-***********************
*  Dictionary API
*************************/
/*! ZSTD1v06_decompress_usingDict() :
*   Decompression using a pre-defined Dictionary content (see dictBuilder).
*   Dictionary must be identical to the one used during compression, otherwise regenerated data will be corrupted.
*   Note : dict can be NULL, in which case, it's equivalent to ZSTD1v06_decompressDCtx() */
ZSTD1LIBv06_API size_t ZSTD1v06_decompress_usingDict(ZSTD1v06_DCtx* dctx,
                                                   void* dst, size_t dstCapacity,
                                             const void* src, size_t srcSize,
                                             const void* dict,size_t dictSize);


/*-************************
*  Advanced Streaming API
***************************/
struct ZSTD1v06_frameParams_s { unsigned long long frameContentSize; unsigned windowLog; };
typedef struct ZSTD1v06_frameParams_s ZSTD1v06_frameParams;

ZSTD1LIBv06_API size_t ZSTD1v06_getFrameParams(ZSTD1v06_frameParams* fparamsPtr, const void* src, size_t srcSize);   /**< doesn't consume input */
ZSTD1LIBv06_API size_t ZSTD1v06_decompressBegin_usingDict(ZSTD1v06_DCtx* dctx, const void* dict, size_t dictSize);
ZSTD1LIBv06_API void   ZSTD1v06_copyDCtx(ZSTD1v06_DCtx* dctx, const ZSTD1v06_DCtx* preparedDCtx);

ZSTD1LIBv06_API size_t ZSTD1v06_nextSrcSizeToDecompress(ZSTD1v06_DCtx* dctx);
ZSTD1LIBv06_API size_t ZSTD1v06_decompressContinue(ZSTD1v06_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize);



/* *************************************
*  ZBUFF1 API
***************************************/

typedef struct ZBUFF1v06_DCtx_s ZBUFF1v06_DCtx;
ZSTD1LIBv06_API ZBUFF1v06_DCtx* ZBUFF1v06_createDCtx(void);
ZSTD1LIBv06_API size_t         ZBUFF1v06_freeDCtx(ZBUFF1v06_DCtx* dctx);

ZSTD1LIBv06_API size_t ZBUFF1v06_decompressInit(ZBUFF1v06_DCtx* dctx);
ZSTD1LIBv06_API size_t ZBUFF1v06_decompressInitDictionary(ZBUFF1v06_DCtx* dctx, const void* dict, size_t dictSize);

ZSTD1LIBv06_API size_t ZBUFF1v06_decompressContinue(ZBUFF1v06_DCtx* dctx,
                                                  void* dst, size_t* dstCapacityPtr,
                                            const void* src, size_t* srcSizePtr);

/*-***************************************************************************
*  Streaming decompression howto
*
*  A ZBUFF1v06_DCtx object is required to track streaming operations.
*  Use ZBUFF1v06_createDCtx() and ZBUFF1v06_freeDCtx() to create/release resources.
*  Use ZBUFF1v06_decompressInit() to start a new decompression operation,
*   or ZBUFF1v06_decompressInitDictionary() if decompression requires a dictionary.
*  Note that ZBUFF1v06_DCtx objects can be re-init multiple times.
*
*  Use ZBUFF1v06_decompressContinue() repetitively to consume your input.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written by modifying *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to present remaining input again.
*  The content of `dst` will be overwritten (up to *dstCapacityPtr) at each function call, so save its content if it matters, or change `dst`.
*  @return : a hint to preferred nb of bytes to use as input for next function call (it's only a hint, to help latency),
*            or 0 when a frame is completely decoded,
*            or an error code, which can be tested using ZBUFF1v06_isError().
*
*  Hint : recommended buffer sizes (not compulsory) : ZBUFF1v06_recommendedDInSize() and ZBUFF1v06_recommendedDOutSize()
*  output : ZBUFF1v06_recommendedDOutSize== 128 KB block size is the internal unit, it ensures it's always possible to write a full block when decoded.
*  input  : ZBUFF1v06_recommendedDInSize == 128KB + 3;
*           just follow indications from ZBUFF1v06_decompressContinue() to minimize latency. It should always be <= 128 KB + 3 .
* *******************************************************************************/


/* *************************************
*  Tool functions
***************************************/
ZSTD1LIBv06_API unsigned ZBUFF1v06_isError(size_t errorCode);
ZSTD1LIBv06_API const char* ZBUFF1v06_getErrorName(size_t errorCode);

/** Functions below provide recommended buffer sizes for Compression or Decompression operations.
*   These sizes are just hints, they tend to offer better latency */
ZSTD1LIBv06_API size_t ZBUFF1v06_recommendedDInSize(void);
ZSTD1LIBv06_API size_t ZBUFF1v06_recommendedDOutSize(void);


/*-*************************************
*  Constants
***************************************/
#define ZSTD1v06_MAGICNUMBER 0xFD2FB526   /* v0.6 */



#if defined (__cplusplus)
}
#endif

#endif  /* ZSTD1v06_BUFFERED_H */
