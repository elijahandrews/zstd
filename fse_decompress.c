/* ******************************************************************
   FSE1 : Finite State Entropy decoder
   Copyright (C) 2013-2015, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - FSE1 source repository : https://github.com/Cyan4973/FiniteStateEntropy
    - Public forum : https://groups.google.com/forum/#!forum/lz4c
****************************************************************** */


/* **************************************************************
*  Compiler specifics
****************************************************************/
#ifdef _MSC_VER    /* Visual Studio */
#  define FORCE_INLINE static __forceinline
#  include <intrin.h>                    /* For Visual 2005 */
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#  pragma warning(disable : 4214)        /* disable: C4214: non-int bitfields */
#else
#  if defined (__cplusplus) || defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L   /* C99 */
#    ifdef __GNUC__
#      define FORCE_INLINE static inline __attribute__((always_inline))
#    else
#      define FORCE_INLINE static inline
#    endif
#  else
#    define FORCE_INLINE static
#  endif /* __STDC_VERSION__ */
#endif


/* **************************************************************
*  Includes
****************************************************************/
#include <stdlib.h>     /* malloc, free, qsort */
#include <string.h>     /* memcpy, memset */
#include "bitstream.h"
#define FSE1_STATIC_LINKING_ONLY
#include "fse.h"


/* **************************************************************
*  Error Management
****************************************************************/
#define FSE1_isError ERR_isError
#define FSE1_STATIC_ASSERT(c) { enum { FSE1_static_assert = 1/(int)(!!(c)) }; }   /* use only *after* variable declarations */

/* check and forward error code */
#define CHECK_F(f) { size_t const e = f; if (FSE1_isError(e)) return e; }


/* **************************************************************
*  Templates
****************************************************************/
/*
  designed to be included
  for type-specific functions (template emulation in C)
  Objective is to write these functions only once, for improved maintenance
*/

/* safety checks */
#ifndef FSE1_FUNCTION_EXTENSION
#  error "FSE1_FUNCTION_EXTENSION must be defined"
#endif
#ifndef FSE1_FUNCTION_TYPE
#  error "FSE1_FUNCTION_TYPE must be defined"
#endif

/* Function names */
#define FSE1_CAT(X,Y) X##Y
#define FSE1_FUNCTION_NAME(X,Y) FSE1_CAT(X,Y)
#define FSE1_TYPE_NAME(X,Y) FSE1_CAT(X,Y)


/* Function templates */
FSE1_DTable* FSE1_createDTable (unsigned tableLog)
{
    if (tableLog > FSE1_TABLELOG_ABSOLUTE_MAX) tableLog = FSE1_TABLELOG_ABSOLUTE_MAX;
    return (FSE1_DTable*)malloc( FSE1_DTABLE_SIZE_U32(tableLog) * sizeof (U32) );
}

void FSE1_freeDTable (FSE1_DTable* dt)
{
    free(dt);
}

size_t FSE1_buildDTable(FSE1_DTable* dt, const short* normalizedCounter, unsigned maxSymbolValue, unsigned tableLog)
{
    void* const tdPtr = dt+1;   /* because *dt is unsigned, 32-bits aligned on 32-bits */
    FSE1_DECODE_TYPE* const tableDecode = (FSE1_DECODE_TYPE*) (tdPtr);
    U16 symbolNext[FSE1_MAX_SYMBOL_VALUE+1];

    U32 const maxSV1 = maxSymbolValue + 1;
    U32 const tableSize = 1 << tableLog;
    U32 highThreshold = tableSize-1;

    /* Sanity Checks */
    if (maxSymbolValue > FSE1_MAX_SYMBOL_VALUE) return ERROR(maxSymbolValue_tooLarge);
    if (tableLog > FSE1_MAX_TABLELOG) return ERROR(tableLog_tooLarge);

    /* Init, lay down lowprob symbols */
    {   FSE1_DTableHeader DTableH;
        DTableH.tableLog = (U16)tableLog;
        DTableH.fastMode = 1;
        {   S16 const largeLimit= (S16)(1 << (tableLog-1));
            U32 s;
            for (s=0; s<maxSV1; s++) {
                if (normalizedCounter[s]==-1) {
                    tableDecode[highThreshold--].symbol = (FSE1_FUNCTION_TYPE)s;
                    symbolNext[s] = 1;
                } else {
                    if (normalizedCounter[s] >= largeLimit) DTableH.fastMode=0;
                    symbolNext[s] = normalizedCounter[s];
        }   }   }
        memcpy(dt, &DTableH, sizeof(DTableH));
    }

    /* Spread symbols */
    {   U32 const tableMask = tableSize-1;
        U32 const step = FSE1_TABLESTEP(tableSize);
        U32 s, position = 0;
        for (s=0; s<maxSV1; s++) {
            int i;
            for (i=0; i<normalizedCounter[s]; i++) {
                tableDecode[position].symbol = (FSE1_FUNCTION_TYPE)s;
                position = (position + step) & tableMask;
                while (position > highThreshold) position = (position + step) & tableMask;   /* lowprob area */
        }   }
        if (position!=0) return ERROR(GENERIC);   /* position must reach all cells once, otherwise normalizedCounter is incorrect */
    }

    /* Build Decoding table */
    {   U32 u;
        for (u=0; u<tableSize; u++) {
            FSE1_FUNCTION_TYPE const symbol = (FSE1_FUNCTION_TYPE)(tableDecode[u].symbol);
            U16 nextState = symbolNext[symbol]++;
            tableDecode[u].nbBits = (BYTE) (tableLog - BIT_highbit32 ((U32)nextState) );
            tableDecode[u].newState = (U16) ( (nextState << tableDecode[u].nbBits) - tableSize);
    }   }

    return 0;
}


#ifndef FSE1_COMMONDEFS_ONLY

/*-*******************************************************
*  Decompression (Byte symbols)
*********************************************************/
size_t FSE1_buildDTable_rle (FSE1_DTable* dt, BYTE symbolValue)
{
    void* ptr = dt;
    FSE1_DTableHeader* const DTableH = (FSE1_DTableHeader*)ptr;
    void* dPtr = dt + 1;
    FSE1_decode_t* const cell = (FSE1_decode_t*)dPtr;

    DTableH->tableLog = 0;
    DTableH->fastMode = 0;

    cell->newState = 0;
    cell->symbol = symbolValue;
    cell->nbBits = 0;

    return 0;
}


size_t FSE1_buildDTable_raw (FSE1_DTable* dt, unsigned nbBits)
{
    void* ptr = dt;
    FSE1_DTableHeader* const DTableH = (FSE1_DTableHeader*)ptr;
    void* dPtr = dt + 1;
    FSE1_decode_t* const dinfo = (FSE1_decode_t*)dPtr;
    const unsigned tableSize = 1 << nbBits;
    const unsigned tableMask = tableSize - 1;
    const unsigned maxSV1 = tableMask+1;
    unsigned s;

    /* Sanity checks */
    if (nbBits < 1) return ERROR(GENERIC);         /* min size */

    /* Build Decoding Table */
    DTableH->tableLog = (U16)nbBits;
    DTableH->fastMode = 1;
    for (s=0; s<maxSV1; s++) {
        dinfo[s].newState = 0;
        dinfo[s].symbol = (BYTE)s;
        dinfo[s].nbBits = (BYTE)nbBits;
    }

    return 0;
}

FORCE_INLINE size_t FSE1_decompress_usingDTable_generic(
          void* dst, size_t maxDstSize,
    const void* cSrc, size_t cSrcSize,
    const FSE1_DTable* dt, const unsigned fast)
{
    BYTE* const ostart = (BYTE*) dst;
    BYTE* op = ostart;
    BYTE* const omax = op + maxDstSize;
    BYTE* const olimit = omax-3;

    BIT_DStream_t bitD;
    FSE1_DState_t state1;
    FSE1_DState_t state2;

    /* Init */
    CHECK_F(BIT_initDStream(&bitD, cSrc, cSrcSize));

    FSE1_initDState(&state1, &bitD, dt);
    FSE1_initDState(&state2, &bitD, dt);

#define FSE1_GETSYMBOL(statePtr) fast ? FSE1_decodeSymbolFast(statePtr, &bitD) : FSE1_decodeSymbol(statePtr, &bitD)

    /* 4 symbols per loop */
    for ( ; (BIT_reloadDStream(&bitD)==BIT_DStream_unfinished) & (op<olimit) ; op+=4) {
        op[0] = FSE1_GETSYMBOL(&state1);

        if (FSE1_MAX_TABLELOG*2+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            BIT_reloadDStream(&bitD);

        op[1] = FSE1_GETSYMBOL(&state2);

        if (FSE1_MAX_TABLELOG*4+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            { if (BIT_reloadDStream(&bitD) > BIT_DStream_unfinished) { op+=2; break; } }

        op[2] = FSE1_GETSYMBOL(&state1);

        if (FSE1_MAX_TABLELOG*2+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            BIT_reloadDStream(&bitD);

        op[3] = FSE1_GETSYMBOL(&state2);
    }

    /* tail */
    /* note : BIT_reloadDStream(&bitD) >= FSE1_DStream_partiallyFilled; Ends at exactly BIT_DStream_completed */
    while (1) {
        if (op>(omax-2)) return ERROR(dstSize_tooSmall);
        *op++ = FSE1_GETSYMBOL(&state1);
        if (BIT_reloadDStream(&bitD)==BIT_DStream_overflow) {
            *op++ = FSE1_GETSYMBOL(&state2);
            break;
        }

        if (op>(omax-2)) return ERROR(dstSize_tooSmall);
        *op++ = FSE1_GETSYMBOL(&state2);
        if (BIT_reloadDStream(&bitD)==BIT_DStream_overflow) {
            *op++ = FSE1_GETSYMBOL(&state1);
            break;
    }   }

    return op-ostart;
}


size_t FSE1_decompress_usingDTable(void* dst, size_t originalSize,
                            const void* cSrc, size_t cSrcSize,
                            const FSE1_DTable* dt)
{
    const void* ptr = dt;
    const FSE1_DTableHeader* DTableH = (const FSE1_DTableHeader*)ptr;
    const U32 fastMode = DTableH->fastMode;

    /* select fast mode (static) */
    if (fastMode) return FSE1_decompress_usingDTable_generic(dst, originalSize, cSrc, cSrcSize, dt, 1);
    return FSE1_decompress_usingDTable_generic(dst, originalSize, cSrc, cSrcSize, dt, 0);
}


size_t FSE1_decompress_wksp(void* dst, size_t dstCapacity, const void* cSrc, size_t cSrcSize, FSE1_DTable* workSpace, unsigned maxLog)
{
    const BYTE* const istart = (const BYTE*)cSrc;
    const BYTE* ip = istart;
    short counting[FSE1_MAX_SYMBOL_VALUE+1];
    unsigned tableLog;
    unsigned maxSymbolValue = FSE1_MAX_SYMBOL_VALUE;

    /* normal FSE1 decoding mode */
    size_t const NCountLength = FSE1_readNCount (counting, &maxSymbolValue, &tableLog, istart, cSrcSize);
    if (FSE1_isError(NCountLength)) return NCountLength;
    //if (NCountLength >= cSrcSize) return ERROR(srcSize_wrong);   /* too small input size; supposed to be already checked in NCountLength, only remaining case : NCountLength==cSrcSize */
    if (tableLog > maxLog) return ERROR(tableLog_tooLarge);
    ip += NCountLength;
    cSrcSize -= NCountLength;

    CHECK_F( FSE1_buildDTable (workSpace, counting, maxSymbolValue, tableLog) );

    return FSE1_decompress_usingDTable (dst, dstCapacity, ip, cSrcSize, workSpace);   /* always return, even if it is an error code */
}


typedef FSE1_DTable DTable_max_t[FSE1_DTABLE_SIZE_U32(FSE1_MAX_TABLELOG)];

size_t FSE1_decompress(void* dst, size_t dstCapacity, const void* cSrc, size_t cSrcSize)
{
    DTable_max_t dt;   /* Static analyzer seems unable to understand this table will be properly initialized later */
    return FSE1_decompress_wksp(dst, dstCapacity, cSrc, cSrcSize, dt, FSE1_MAX_TABLELOG);
}



#endif   /* FSE1_COMMONDEFS_ONLY */
