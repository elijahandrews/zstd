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


ZBUFF1_DCtx* ZBUFF1_createDCtx(void)
{
    return ZSTD1_createDStream();
}

ZBUFF1_DCtx* ZBUFF1_createDCtx_advanced(ZSTD1_customMem customMem)
{
    return ZSTD1_createDStream_advanced(customMem);
}

size_t ZBUFF1_freeDCtx(ZBUFF1_DCtx* zbd)
{
    return ZSTD1_freeDStream(zbd);
}


/* *** Initialization *** */

size_t ZBUFF1_decompressInitDictionary(ZBUFF1_DCtx* zbd, const void* dict, size_t dictSize)
{
    return ZSTD1_initDStream_usingDict(zbd, dict, dictSize);
}

size_t ZBUFF1_decompressInit(ZBUFF1_DCtx* zbd)
{
    return ZSTD1_initDStream(zbd);
}


/* *** Decompression *** */

size_t ZBUFF1_decompressContinue(ZBUFF1_DCtx* zbd,
                                void* dst, size_t* dstCapacityPtr,
                          const void* src, size_t* srcSizePtr)
{
    ZSTD1_outBuffer outBuff;
    ZSTD1_inBuffer inBuff;
    size_t result;
    outBuff.dst  = dst;
    outBuff.pos  = 0;
    outBuff.size = *dstCapacityPtr;
    inBuff.src  = src;
    inBuff.pos  = 0;
    inBuff.size = *srcSizePtr;
    result = ZSTD1_decompressStream(zbd, &outBuff, &inBuff);
    *dstCapacityPtr = outBuff.pos;
    *srcSizePtr = inBuff.pos;
    return result;
}


/* *************************************
*  Tool functions
***************************************/
size_t ZBUFF1_recommendedDInSize(void)  { return ZSTD1_DStreamInSize(); }
size_t ZBUFF1_recommendedDOutSize(void) { return ZSTD1_DStreamOutSize(); }
