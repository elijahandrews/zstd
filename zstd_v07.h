/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1v07_H_235446
#define ZSTD1v07_H_235446

#if defined (__cplusplus)
extern "C" {
#endif

/*======  Dependency  ======*/
#include <stddef.h>   /* size_t */


/*======  Export for Windows  ======*/
/*!
*  ZSTD1v07_DLL_EXPORT :
*  Enable exporting of functions when building a Windows DLL
*/
#if defined(_WIN32) && defined(ZSTD1v07_DLL_EXPORT) && (ZSTD1v07_DLL_EXPORT==1)
#  define ZSTD1LIBv07_API __declspec(dllexport)
#else
#  define ZSTD1LIBv07_API
#endif


/* *************************************
*  Simple API
***************************************/
/*! ZSTD1v07_getDecompressedSize() :
*   @return : decompressed size if known, 0 otherwise.
       note 1 : if `0`, follow up with ZSTD1v07_getFrameParams() to know precise failure cause.
       note 2 : decompressed size could be wrong or intentionally modified !
                always ensure results fit within application's authorized limits */
unsigned long long ZSTD1v07_getDecompressedSize(const void* src, size_t srcSize);

/*! ZSTD1v07_decompress() :
    `compressedSize` : must be _exact_ size of compressed input, otherwise decompression will fail.
    `dstCapacity` must be equal or larger than originalSize.
    @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
              or an errorCode if it fails (which can be tested using ZSTD1v07_isError()) */
ZSTD1LIBv07_API size_t ZSTD1v07_decompress( void* dst, size_t dstCapacity,
                                    const void* src, size_t compressedSize);

/**
ZSTD1v07_getFrameSrcSize() : get the source length of a ZSTD1 frame
    compressedSize : The size of the 'src' buffer, at least as large as the frame pointed to by 'src'
    return : the number of bytes that would be read to decompress this frame
             or an errorCode if it fails (which can be tested using ZSTD1v07_isError())
*/
size_t ZSTD1v07_findFrameCompressedSize(const void* src, size_t compressedSize);

/*======  Helper functions  ======*/
ZSTD1LIBv07_API unsigned    ZSTD1v07_isError(size_t code);          /*!< tells if a `size_t` function result is an error code */
ZSTD1LIBv07_API const char* ZSTD1v07_getErrorName(size_t code);     /*!< provides readable string from an error code */


/*-*************************************
*  Explicit memory management
***************************************/
/** Decompression context */
typedef struct ZSTD1v07_DCtx_s ZSTD1v07_DCtx;
ZSTD1LIBv07_API ZSTD1v07_DCtx* ZSTD1v07_createDCtx(void);
ZSTD1LIBv07_API size_t     ZSTD1v07_freeDCtx(ZSTD1v07_DCtx* dctx);      /*!< @return : errorCode */

/** ZSTD1v07_decompressDCtx() :
*   Same as ZSTD1v07_decompress(), requires an allocated ZSTD1v07_DCtx (see ZSTD1v07_createDCtx()) */
ZSTD1LIBv07_API size_t ZSTD1v07_decompressDCtx(ZSTD1v07_DCtx* ctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize);


/*-************************
*  Simple dictionary API
***************************/
/*! ZSTD1v07_decompress_usingDict() :
*   Decompression using a pre-defined Dictionary content (see dictBuilder).
*   Dictionary must be identical to the one used during compression.
*   Note : This function load the dictionary, resulting in a significant startup time */
ZSTD1LIBv07_API size_t ZSTD1v07_decompress_usingDict(ZSTD1v07_DCtx* dctx,
                                                   void* dst, size_t dstCapacity,
                                             const void* src, size_t srcSize,
                                             const void* dict,size_t dictSize);


/*-**************************
*  Advanced Dictionary API
****************************/
/*! ZSTD1v07_createDDict() :
*   Create a digested dictionary, ready to start decompression operation without startup delay.
*   `dict` can be released after creation */
typedef struct ZSTD1v07_DDict_s ZSTD1v07_DDict;
ZSTD1LIBv07_API ZSTD1v07_DDict* ZSTD1v07_createDDict(const void* dict, size_t dictSize);
ZSTD1LIBv07_API size_t      ZSTD1v07_freeDDict(ZSTD1v07_DDict* ddict);

/*! ZSTD1v07_decompress_usingDDict() :
*   Decompression using a pre-digested Dictionary
*   Faster startup than ZSTD1v07_decompress_usingDict(), recommended when same dictionary is used multiple times. */
ZSTD1LIBv07_API size_t ZSTD1v07_decompress_usingDDict(ZSTD1v07_DCtx* dctx,
                                                    void* dst, size_t dstCapacity,
                                              const void* src, size_t srcSize,
                                              const ZSTD1v07_DDict* ddict);

typedef struct {
    unsigned long long frameContentSize;
    unsigned windowSize;
    unsigned dictID;
    unsigned checksumFlag;
} ZSTD1v07_frameParams;

ZSTD1LIBv07_API size_t ZSTD1v07_getFrameParams(ZSTD1v07_frameParams* fparamsPtr, const void* src, size_t srcSize);   /**< doesn't consume input */




/* *************************************
*  Streaming functions
***************************************/
typedef struct ZBUFF1v07_DCtx_s ZBUFF1v07_DCtx;
ZSTD1LIBv07_API ZBUFF1v07_DCtx* ZBUFF1v07_createDCtx(void);
ZSTD1LIBv07_API size_t      ZBUFF1v07_freeDCtx(ZBUFF1v07_DCtx* dctx);

ZSTD1LIBv07_API size_t ZBUFF1v07_decompressInit(ZBUFF1v07_DCtx* dctx);
ZSTD1LIBv07_API size_t ZBUFF1v07_decompressInitDictionary(ZBUFF1v07_DCtx* dctx, const void* dict, size_t dictSize);

ZSTD1LIBv07_API size_t ZBUFF1v07_decompressContinue(ZBUFF1v07_DCtx* dctx,
                                            void* dst, size_t* dstCapacityPtr,
                                      const void* src, size_t* srcSizePtr);

/*-***************************************************************************
*  Streaming decompression howto
*
*  A ZBUFF1v07_DCtx object is required to track streaming operations.
*  Use ZBUFF1v07_createDCtx() and ZBUFF1v07_freeDCtx() to create/release resources.
*  Use ZBUFF1v07_decompressInit() to start a new decompression operation,
*   or ZBUFF1v07_decompressInitDictionary() if decompression requires a dictionary.
*  Note that ZBUFF1v07_DCtx objects can be re-init multiple times.
*
*  Use ZBUFF1v07_decompressContinue() repetitively to consume your input.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written by modifying *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to present remaining input again.
*  The content of `dst` will be overwritten (up to *dstCapacityPtr) at each function call, so save its content if it matters, or change `dst`.
*  @return : a hint to preferred nb of bytes to use as input for next function call (it's only a hint, to help latency),
*            or 0 when a frame is completely decoded,
*            or an error code, which can be tested using ZBUFF1v07_isError().
*
*  Hint : recommended buffer sizes (not compulsory) : ZBUFF1v07_recommendedDInSize() and ZBUFF1v07_recommendedDOutSize()
*  output : ZBUFF1v07_recommendedDOutSize== 128 KB block size is the internal unit, it ensures it's always possible to write a full block when decoded.
*  input  : ZBUFF1v07_recommendedDInSize == 128KB + 3;
*           just follow indications from ZBUFF1v07_decompressContinue() to minimize latency. It should always be <= 128 KB + 3 .
* *******************************************************************************/


/* *************************************
*  Tool functions
***************************************/
ZSTD1LIBv07_API unsigned ZBUFF1v07_isError(size_t errorCode);
ZSTD1LIBv07_API const char* ZBUFF1v07_getErrorName(size_t errorCode);

/** Functions below provide recommended buffer sizes for Compression or Decompression operations.
*   These sizes are just hints, they tend to offer better latency */
ZSTD1LIBv07_API size_t ZBUFF1v07_recommendedDInSize(void);
ZSTD1LIBv07_API size_t ZBUFF1v07_recommendedDOutSize(void);


/*-*************************************
*  Constants
***************************************/
#define ZSTD1v07_MAGICNUMBER            0xFD2FB527   /* v0.7 */


#if defined (__cplusplus)
}
#endif

#endif  /* ZSTD1v07_H_235446 */
