/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */



/* *************************************
*  Dependencies
***************************************/
#define ZBUFF1_STATIC_LINKING_ONLY
#include "zbuff.h"


/*-***********************************************************
*  Streaming compression
*
*  A ZBUFF1_CCtx object is required to track streaming operation.
*  Use ZBUFF1_createCCtx() and ZBUFF1_freeCCtx() to create/release resources.
*  Use ZBUFF1_compressInit() to start a new compression operation.
*  ZBUFF1_CCtx objects can be reused multiple times.
*
*  Use ZBUFF1_compressContinue() repetitively to consume your input.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written by modifying *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to call again the function with remaining input.
*  The content of dst will be overwritten (up to *dstCapacityPtr) at each function call, so save its content if it matters or change dst .
*  @return : a hint to preferred nb of bytes to use as input for next function call (it's only a hint, to improve latency)
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  ZBUFF1_compressFlush() can be used to instruct ZBUFF1 to compress and output whatever remains within its buffer.
*  Note that it will not output more than *dstCapacityPtr.
*  Therefore, some content might still be left into its internal buffer if dst buffer is too small.
*  @return : nb of bytes still present into internal buffer (0 if it's empty)
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  ZBUFF1_compressEnd() instructs to finish a frame.
*  It will perform a flush and write frame epilogue.
*  Similar to ZBUFF1_compressFlush(), it may not be able to output the entire internal buffer content if *dstCapacityPtr is too small.
*  @return : nb of bytes still present into internal buffer (0 if it's empty)
*            or an error code, which can be tested using ZBUFF1_isError().
*
*  Hint : recommended buffer sizes (not compulsory)
*  input : ZSTD1_BLOCKSIZE_MAX (128 KB), internal unit size, it improves latency to use this value.
*  output : ZSTD1_compressBound(ZSTD1_BLOCKSIZE_MAX) + ZSTD1_blockHeaderSize + ZBUFF1_endFrameSize : ensures it's always possible to write/flush/end a full block at best speed.
* ***********************************************************/

ZBUFF1_CCtx* ZBUFF1_createCCtx(void)
{
    return ZSTD1_createCStream();
}

ZBUFF1_CCtx* ZBUFF1_createCCtx_advanced(ZSTD1_customMem customMem)
{
    return ZSTD1_createCStream_advanced(customMem);
}

size_t ZBUFF1_freeCCtx(ZBUFF1_CCtx* zbc)
{
    return ZSTD1_freeCStream(zbc);
}


/* ======   Initialization   ====== */

size_t ZBUFF1_compressInit_advanced(ZBUFF1_CCtx* zbc,
                                   const void* dict, size_t dictSize,
                                   ZSTD1_parameters params, unsigned long long pledgedSrcSize)
{
    return ZSTD1_initCStream_advanced(zbc, dict, dictSize, params, pledgedSrcSize);
}


size_t ZBUFF1_compressInitDictionary(ZBUFF1_CCtx* zbc, const void* dict, size_t dictSize, int compressionLevel)
{
    return ZSTD1_initCStream_usingDict(zbc, dict, dictSize, compressionLevel);
}

size_t ZBUFF1_compressInit(ZBUFF1_CCtx* zbc, int compressionLevel)
{
    return ZSTD1_initCStream(zbc, compressionLevel);
}

/* ======   Compression   ====== */


size_t ZBUFF1_compressContinue(ZBUFF1_CCtx* zbc,
                              void* dst, size_t* dstCapacityPtr,
                        const void* src, size_t* srcSizePtr)
{
    size_t result;
    ZSTD1_outBuffer outBuff;
    ZSTD1_inBuffer inBuff;
    outBuff.dst = dst;
    outBuff.pos = 0;
    outBuff.size = *dstCapacityPtr;
    inBuff.src = src;
    inBuff.pos = 0;
    inBuff.size = *srcSizePtr;
    result = ZSTD1_compressStream(zbc, &outBuff, &inBuff);
    *dstCapacityPtr = outBuff.pos;
    *srcSizePtr = inBuff.pos;
    return result;
}



/* ======   Finalize   ====== */

size_t ZBUFF1_compressFlush(ZBUFF1_CCtx* zbc, void* dst, size_t* dstCapacityPtr)
{
    size_t result;
    ZSTD1_outBuffer outBuff;
    outBuff.dst = dst;
    outBuff.pos = 0;
    outBuff.size = *dstCapacityPtr;
    result = ZSTD1_flushStream(zbc, &outBuff);
    *dstCapacityPtr = outBuff.pos;
    return result;
}


size_t ZBUFF1_compressEnd(ZBUFF1_CCtx* zbc, void* dst, size_t* dstCapacityPtr)
{
    size_t result;
    ZSTD1_outBuffer outBuff;
    outBuff.dst = dst;
    outBuff.pos = 0;
    outBuff.size = *dstCapacityPtr;
    result = ZSTD1_endStream(zbc, &outBuff);
    *dstCapacityPtr = outBuff.pos;
    return result;
}



/* *************************************
*  Tool functions
***************************************/
size_t ZBUFF1_recommendedCInSize(void)  { return ZSTD1_CStreamInSize(); }
size_t ZBUFF1_recommendedCOutSize(void) { return ZSTD1_CStreamOutSize(); }
