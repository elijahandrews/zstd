/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1_LEGACY_H
#define ZSTD1_LEGACY_H

#if defined (__cplusplus)
extern "C" {
#endif

/* *************************************
*  Includes
***************************************/
#include "mem.h"            /* MEM_STATIC */
#include "error_private.h"  /* ERROR */
#include "zstd.h"           /* ZSTD1_inBuffer, ZSTD1_outBuffer */

#if !defined (ZSTD1_LEGACY_SUPPORT) || (ZSTD1_LEGACY_SUPPORT == 0)
#  undef ZSTD1_LEGACY_SUPPORT
#  define ZSTD1_LEGACY_SUPPORT 8
#endif

#if (ZSTD1_LEGACY_SUPPORT <= 1)
#  include "zstd_v01.h"
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 2)
#  include "zstd_v02.h"
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 3)
#  include "zstd_v03.h"
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 4)
#  include "zstd_v04.h"
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
#  include "zstd_v05.h"
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
#  include "zstd_v06.h"
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
#  include "zstd_v07.h"
#endif

/** ZSTD1_isLegacy() :
    @return : > 0 if supported by legacy decoder. 0 otherwise.
              return value is the version.
*/
MEM_STATIC unsigned ZSTD1_isLegacy(const void* src, size_t srcSize)
{
    U32 magicNumberLE;
    if (srcSize<4) return 0;
    magicNumberLE = MEM_readLE32(src);
    switch(magicNumberLE)
    {
#if (ZSTD1_LEGACY_SUPPORT <= 1)
        case ZSTD1v01_magicNumberLE:return 1;
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 2)
        case ZSTD1v02_magicNumber : return 2;
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 3)
        case ZSTD1v03_magicNumber : return 3;
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 4)
        case ZSTD1v04_magicNumber : return 4;
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
        case ZSTD1v05_MAGICNUMBER : return 5;
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
        case ZSTD1v06_MAGICNUMBER : return 6;
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
        case ZSTD1v07_MAGICNUMBER : return 7;
#endif
        default : return 0;
    }
}


MEM_STATIC unsigned long long ZSTD1_getDecompressedSize_legacy(const void* src, size_t srcSize)
{
    U32 const version = ZSTD1_isLegacy(src, srcSize);
    if (version < 5) return 0;  /* no decompressed size in frame header, or not a legacy format */
#if (ZSTD1_LEGACY_SUPPORT <= 5)
    if (version==5) {
        ZSTD1v05_parameters fParams;
        size_t const frResult = ZSTD1v05_getFrameParams(&fParams, src, srcSize);
        if (frResult != 0) return 0;
        return fParams.srcSize;
    }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
    if (version==6) {
        ZSTD1v06_frameParams fParams;
        size_t const frResult = ZSTD1v06_getFrameParams(&fParams, src, srcSize);
        if (frResult != 0) return 0;
        return fParams.frameContentSize;
    }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
    if (version==7) {
        ZSTD1v07_frameParams fParams;
        size_t const frResult = ZSTD1v07_getFrameParams(&fParams, src, srcSize);
        if (frResult != 0) return 0;
        return fParams.frameContentSize;
    }
#endif
    return 0;   /* should not be possible */
}


MEM_STATIC size_t ZSTD1_decompressLegacy(
                     void* dst, size_t dstCapacity,
               const void* src, size_t compressedSize,
               const void* dict,size_t dictSize)
{
    U32 const version = ZSTD1_isLegacy(src, compressedSize);
    switch(version)
    {
#if (ZSTD1_LEGACY_SUPPORT <= 1)
        case 1 :
            return ZSTD1v01_decompress(dst, dstCapacity, src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 2)
        case 2 :
            return ZSTD1v02_decompress(dst, dstCapacity, src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 3)
        case 3 :
            return ZSTD1v03_decompress(dst, dstCapacity, src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 4)
        case 4 :
            return ZSTD1v04_decompress(dst, dstCapacity, src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
        case 5 :
            {   size_t result;
                ZSTD1v05_DCtx* const zd = ZSTD1v05_createDCtx();
                if (zd==NULL) return ERROR(memory_allocation);
                result = ZSTD1v05_decompress_usingDict(zd, dst, dstCapacity, src, compressedSize, dict, dictSize);
                ZSTD1v05_freeDCtx(zd);
                return result;
            }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
        case 6 :
            {   size_t result;
                ZSTD1v06_DCtx* const zd = ZSTD1v06_createDCtx();
                if (zd==NULL) return ERROR(memory_allocation);
                result = ZSTD1v06_decompress_usingDict(zd, dst, dstCapacity, src, compressedSize, dict, dictSize);
                ZSTD1v06_freeDCtx(zd);
                return result;
            }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
        case 7 :
            {   size_t result;
                ZSTD1v07_DCtx* const zd = ZSTD1v07_createDCtx();
                if (zd==NULL) return ERROR(memory_allocation);
                result = ZSTD1v07_decompress_usingDict(zd, dst, dstCapacity, src, compressedSize, dict, dictSize);
                ZSTD1v07_freeDCtx(zd);
                return result;
            }
#endif
        default :
            return ERROR(prefix_unknown);
    }
}

MEM_STATIC size_t ZSTD1_findFrameCompressedSizeLegacy(const void *src,
                                             size_t compressedSize)
{
    U32 const version = ZSTD1_isLegacy(src, compressedSize);
    switch(version)
    {
#if (ZSTD1_LEGACY_SUPPORT <= 1)
        case 1 :
            return ZSTD1v01_findFrameCompressedSize(src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 2)
        case 2 :
            return ZSTD1v02_findFrameCompressedSize(src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 3)
        case 3 :
            return ZSTD1v03_findFrameCompressedSize(src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 4)
        case 4 :
            return ZSTD1v04_findFrameCompressedSize(src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
        case 5 :
            return ZSTD1v05_findFrameCompressedSize(src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
        case 6 :
            return ZSTD1v06_findFrameCompressedSize(src, compressedSize);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
        case 7 :
            return ZSTD1v07_findFrameCompressedSize(src, compressedSize);
#endif
        default :
            return ERROR(prefix_unknown);
    }
}

MEM_STATIC size_t ZSTD1_freeLegacyStreamContext(void* legacyContext, U32 version)
{
    switch(version)
    {
        default :
        case 1 :
        case 2 :
        case 3 :
            return ERROR(version_unsupported);
#if (ZSTD1_LEGACY_SUPPORT <= 4)
        case 4 : return ZBUFF1v04_freeDCtx((ZBUFF1v04_DCtx*)legacyContext);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
        case 5 : return ZBUFF1v05_freeDCtx((ZBUFF1v05_DCtx*)legacyContext);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
        case 6 : return ZBUFF1v06_freeDCtx((ZBUFF1v06_DCtx*)legacyContext);
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
        case 7 : return ZBUFF1v07_freeDCtx((ZBUFF1v07_DCtx*)legacyContext);
#endif
    }
}


MEM_STATIC size_t ZSTD1_initLegacyStream(void** legacyContext, U32 prevVersion, U32 newVersion,
                                        const void* dict, size_t dictSize)
{
    if (prevVersion != newVersion) ZSTD1_freeLegacyStreamContext(*legacyContext, prevVersion);
    switch(newVersion)
    {
        default :
        case 1 :
        case 2 :
        case 3 :
            return 0;
#if (ZSTD1_LEGACY_SUPPORT <= 4)
        case 4 :
        {
            ZBUFF1v04_DCtx* dctx = (prevVersion != newVersion) ? ZBUFF1v04_createDCtx() : (ZBUFF1v04_DCtx*)*legacyContext;
            if (dctx==NULL) return ERROR(memory_allocation);
            ZBUFF1v04_decompressInit(dctx);
            ZBUFF1v04_decompressWithDictionary(dctx, dict, dictSize);
            *legacyContext = dctx;
            return 0;
        }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
        case 5 :
        {
            ZBUFF1v05_DCtx* dctx = (prevVersion != newVersion) ? ZBUFF1v05_createDCtx() : (ZBUFF1v05_DCtx*)*legacyContext;
            if (dctx==NULL) return ERROR(memory_allocation);
            ZBUFF1v05_decompressInitDictionary(dctx, dict, dictSize);
            *legacyContext = dctx;
            return 0;
        }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
        case 6 :
        {
            ZBUFF1v06_DCtx* dctx = (prevVersion != newVersion) ? ZBUFF1v06_createDCtx() : (ZBUFF1v06_DCtx*)*legacyContext;
            if (dctx==NULL) return ERROR(memory_allocation);
            ZBUFF1v06_decompressInitDictionary(dctx, dict, dictSize);
            *legacyContext = dctx;
            return 0;
        }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
        case 7 :
        {
            ZBUFF1v07_DCtx* dctx = (prevVersion != newVersion) ? ZBUFF1v07_createDCtx() : (ZBUFF1v07_DCtx*)*legacyContext;
            if (dctx==NULL) return ERROR(memory_allocation);
            ZBUFF1v07_decompressInitDictionary(dctx, dict, dictSize);
            *legacyContext = dctx;
            return 0;
        }
#endif
    }
}



MEM_STATIC size_t ZSTD1_decompressLegacyStream(void* legacyContext, U32 version,
                                              ZSTD1_outBuffer* output, ZSTD1_inBuffer* input)
{
    switch(version)
    {
        default :
        case 1 :
        case 2 :
        case 3 :
            return ERROR(version_unsupported);
#if (ZSTD1_LEGACY_SUPPORT <= 4)
        case 4 :
            {
                ZBUFF1v04_DCtx* dctx = (ZBUFF1v04_DCtx*) legacyContext;
                const void* src = (const char*)input->src + input->pos;
                size_t readSize = input->size - input->pos;
                void* dst = (char*)output->dst + output->pos;
                size_t decodedSize = output->size - output->pos;
                size_t const hintSize = ZBUFF1v04_decompressContinue(dctx, dst, &decodedSize, src, &readSize);
                output->pos += decodedSize;
                input->pos += readSize;
                return hintSize;
            }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 5)
        case 5 :
            {
                ZBUFF1v05_DCtx* dctx = (ZBUFF1v05_DCtx*) legacyContext;
                const void* src = (const char*)input->src + input->pos;
                size_t readSize = input->size - input->pos;
                void* dst = (char*)output->dst + output->pos;
                size_t decodedSize = output->size - output->pos;
                size_t const hintSize = ZBUFF1v05_decompressContinue(dctx, dst, &decodedSize, src, &readSize);
                output->pos += decodedSize;
                input->pos += readSize;
                return hintSize;
            }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 6)
        case 6 :
            {
                ZBUFF1v06_DCtx* dctx = (ZBUFF1v06_DCtx*) legacyContext;
                const void* src = (const char*)input->src + input->pos;
                size_t readSize = input->size - input->pos;
                void* dst = (char*)output->dst + output->pos;
                size_t decodedSize = output->size - output->pos;
                size_t const hintSize = ZBUFF1v06_decompressContinue(dctx, dst, &decodedSize, src, &readSize);
                output->pos += decodedSize;
                input->pos += readSize;
                return hintSize;
            }
#endif
#if (ZSTD1_LEGACY_SUPPORT <= 7)
        case 7 :
            {
                ZBUFF1v07_DCtx* dctx = (ZBUFF1v07_DCtx*) legacyContext;
                const void* src = (const char*)input->src + input->pos;
                size_t readSize = input->size - input->pos;
                void* dst = (char*)output->dst + output->pos;
                size_t decodedSize = output->size - output->pos;
                size_t const hintSize = ZBUFF1v07_decompressContinue(dctx, dst, &decodedSize, src, &readSize);
                output->pos += decodedSize;
                input->pos += readSize;
                return hintSize;
            }
#endif
    }
}


#if defined (__cplusplus)
}
#endif

#endif   /* ZSTD1_LEGACY_H */
