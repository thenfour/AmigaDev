#pragma once

#include "support/gcc8_c_support.h"
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>
#include <exec/execbase.h>
#include <graphics/gfxmacros.h>
#include <hardware/custom.h>
#include <hardware/dmabits.h>
#include <hardware/intbits.h>
#include <exec/types.h>
#include <libraries/dos.h>
#include "base.hpp"
#include "fixedPoint.hpp"
#include "image.hpp"
#include "app.hpp"

inline USHORT *copSetPlanes(UBYTE bplPtrStart, USHORT *copListEnd, const UBYTE **planes, int numPlanes)
{
    for (USHORT i = 0; i < numPlanes; i++)
    {
        // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node006B.html
        // just sets an address into the register pair.
        ULONG addr = (ULONG)planes[i];
        *copListEnd++ = offsetof(struct Custom, bplpt[0]) + (i + bplPtrStart) * sizeof(APTR);
        *copListEnd++ = (UWORD)(addr >> 16);
        *copListEnd++ = offsetof(struct Custom, bplpt[0]) + (i + bplPtrStart) * sizeof(APTR) + 2;
        *copListEnd++ = (UWORD)addr;
    }
    return copListEnd;
}

inline USHORT *copWaitXY(USHORT *copListEnd, USHORT x, USHORT i)
{
    *copListEnd++ = (i << 8) | (x << 1) | 1; // bit 1 means wait. waits for vertical position x<<8, first raster stop position outside the left
    *copListEnd++ = 0xfffe;
    return copListEnd;
}

inline USHORT *copWaitY(USHORT *copListEnd, USHORT i)
{
    *copListEnd++ = (i << 8) | 4 | 1; // bit 1 means wait. waits for vertical position x<<8, first raster stop position outside the left
    *copListEnd++ = 0xfffe;
    return copListEnd;
}

inline USHORT *copSetColor(USHORT *copListCurrent, USHORT index, USHORT color)
{
    *copListCurrent++ = offsetof(struct Custom, color) + sizeof(UWORD) * index;
    *copListCurrent++ = color;
    return copListCurrent;
}

// set up a 320x256 lowres display
// sets DDFSTRT, DDFSTOP, DIWSTRT, DIWSTOP registers.
// i don't quite understand these but ok.
inline USHORT *screenScanDefault(USHORT *copListEnd)
{
    constexpr USHORT x = 129;
    constexpr USHORT width = 320;
    constexpr USHORT height = 256;
    constexpr USHORT y = 44;
    constexpr USHORT RES = 8; // 8=lowres,4=hires
    constexpr USHORT xstop = x + width;
    constexpr USHORT ystop = y + height;
    constexpr USHORT fw = (x >> 1) - RES; // typically $38 for low res, $3C for hires

    // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0071.html
    // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0072.html#line9
    *copListEnd++ = offsetof(struct Custom, ddfstrt);
    *copListEnd++ = fw;
    *copListEnd++ = offsetof(struct Custom, ddfstop);
    *copListEnd++ = fw + (((width >> 4) - 1) << 3); // $d0 for lowres, $d4 for hires.

    // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0070.html#line9
    *copListEnd++ = offsetof(struct Custom, diwstrt);
    *copListEnd++ = x + (y << 8); // $2C00
    *copListEnd++ = offsetof(struct Custom, diwstop);
    *copListEnd++ = (xstop - 256) + ((ystop - 256) << 8); // $12C
    return copListEnd;
}

namespace cc
{

    // auto list = MakeCopperlist(CopperInstruction<0, 0>, CopperInstruction<1, 1>);


    // template<size_t entries>
    // struct IntList {
    //     // auto
    // }

} // namespace cc