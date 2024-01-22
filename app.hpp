// interrupt info
// https://wiki.amigaos.net/wiki/Exec_Interrupts
// timer info http://amigadev.elowar.com/read/ADCD_2.1/Devices_Manual_guide/node00BD.html
//

// https://github.com/Bloodmosher/AmigaSparkler/blob/d93239a5cc792dea207336f90497feff22dc7ba8/src/sparkler.c
// in theory this should work but i can't get it to.
// overall keyboard is not practical because it relies on interrupts which we have disabled.

#pragma once

#include "support/gcc8_c_support.h"
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>
#include <exec/execbase.h>
#include <graphics/gfxmacros.h>
#include <hardware/cia.h>
#include <hardware/custom.h>
#include <hardware/dmabits.h>
#include <hardware/intbits.h>
#include <exec/types.h>
#include <intuition/intuition.h>
#include <libraries/dos.h>
#include <exec/interrupts.h>
#include <devices/keyboard.h>

#include "image.hpp"
#include "music.hpp"
#include "copper.hpp"

extern struct ExecBase *SysBase;
extern volatile struct Custom *custom;
extern struct DosLibrary *DOSBase;
extern struct GfxBase *GfxBase;

CIA *const ciaa = (CIA *)0xBFE001;
CIA *const ciab = (CIA *)0xbfd000;

namespace cc
{

    // https://github.com/cahirwpz/demoscene-aga/blob/7b24bbf765ce9694c6573726e8f78a06dcfa7424/system/hardware.c#L16
    /* All TOD registers latch on a read of MSB event and remain latched until
     * after a read of LSB event. */
    // but to be honest in my environment i believe it's best to just keep my own frame counter; would be faster and less code most likely.
    inline int ReadLineCounter()
    {
        // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node012E.html
        // .715909 Mhz NTSC; .709379 Mhz PAL
        int res = 0;
        res |= ciab->ciatodhi;
        res <<= 8;
        res |= ciab->ciatodmid;
        res <<= 8;
        res |= ciab->ciatodlow;
        return res;
    }

    inline int ReadFrameCounter()
    {
        // 50 or 60 hz
        int res = 0;
        res |= ciaa->ciatodhi;
        res <<= 8;
        res |= ciaa->ciatodmid;
        res <<= 8;
        res |= ciaa->ciatodlow;
        return res;
    }

    /* TOD is automatically stopped whenever a write to the register occurs. The
     * clock will not start again until after a write to the LSB event register. */

    inline void SetFrameCounter(int frame)
    {
        ciaa->ciatodhi = frame >> 16;
        ciaa->ciatodmid = frame >> 8;
        ciaa->ciatodlow = frame;
    }

    // wrapping behavior. clamping is possible
    template <int TminX, int TminY, int Twidth, int Theight, int Tspeed0_256>
    struct Mouse
    {
        int16_t mPointerX = 0;
        int16_t mPointerY = 0;

        bool mLeftButtonClicked = false;
        bool mRightButtonClicked = false;

        // frame state
        int32_t mPointerXPrecise = 0;
        int32_t mPointerYPrecise = 0;

        int8_t mCounterX = 0;
        int8_t mCounterY = 0;
        int8_t mDeltaX;
        int8_t mDeltaY;

        void OnFrame()
        {
            uint16_t joy0dat = custom->joy0dat;

            // https://github.com/balrogsoft/amiga500-game-engine/blob/edbe9859fa815bbdaf7b43f1555dd4ca9737845f/Engine.c#L383
            // https://github.com/cahirwpz/demoscene/blob/26a6cbfc6eaac73a51cf1207baa31dff2bbd321b/system/drivers/mouse.c#L24
            // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0180.html
            //    Bits 15-8     Mouse/trackball vertical count
            //    Bits  7-0     Mouse/trackball horizontal count
            BYTE xctr = joy0dat & 0xff;
            BYTE xrel = xctr - mCounterX;
            mCounterX = xctr;
            // a problem with slower mouse speeds is that we can only advance by whole pixels here. we should actually store subpixels.
            mPointerXPrecise = cc::clamp_into_range<TminX * 256, Twidth * 256>(mPointerXPrecise + (((int32_t)xrel) * Tspeed0_256));
            mPointerX = mPointerXPrecise / 256;
            mDeltaX = xrel;

            BYTE ycounter = joy0dat >> 8;
            BYTE ydelta = ycounter - mCounterY;
            mCounterY = ycounter;
            mPointerYPrecise = cc::clamp_into_range<TminY * 256, Theight * 256>(mPointerYPrecise + (((int32_t)ydelta) * Tspeed0_256));
            mPointerY = mPointerYPrecise / 256;
            mDeltaY = ydelta;

            mLeftButtonClicked = !((*(volatile UBYTE *)0xbfe001) & 64);
            mRightButtonClicked = !((*(volatile UWORD *)0xdff016) & (1 << 10));
        }
    }; // struct Mouse

} // namespace cc

volatile uint32_t gFrameCounter = 0;

struct AmigaApp
{
    volatile APTR VBR;
    UWORD SystemInts;
    UWORD SystemDMA;
    UWORD SystemADKCON;
    APTR SystemIrq;
    struct View *ActiView;

    AmigaApp()
    {
        SysBase = *((struct ExecBase **)4UL);
        custom = (struct Custom *)0xdff000;

        // We will use the graphics library only to locate and restore the system copper list once we are through.
        GfxBase = (struct GfxBase *)OpenLibrary((CONST_STRPTR) "graphics.library", 0);

        // used for printing
        DOSBase = (struct DosLibrary *)OpenLibrary((CONST_STRPTR) "dos.library", 0);

        // KPrintF("MAIN2()!\n");                          // debug print
        // Write(Output(), (APTR) "Hello cons1le!\n", 15); // console print

        TakeSystem();
        WaitVbl();
    }

    ~AmigaApp()
    {
        FreeSystem();

        CloseLibrary((struct Library *)DOSBase);
        CloseLibrary((struct Library *)GfxBase);
        DOSBase = nullptr;
        GfxBase = nullptr;
    }

    static __attribute__((interrupt)) void interruptHandler()
    {
        custom->intreq = (1 << INTB_VERTB);
        custom->intreq = (1 << INTB_VERTB); // reset vbl req. twice for a4000 bug.
        // static bool sign = false;

        // modify scrolling in copper list
        // if (scroll)
        // {
        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0092.html#line88
        //     // this sets BPLCON1, which is a "horizontal delay", for both bitplane groups (playfields).
        //     // int sin = sinus15[frameCounter & 63];
        //     short sin = (frameCounter & 15) - 7;
        //     sin = sin < 0 ? -sin : sin;
        //     *scroll = sin | (sin << 4);
        //     // sign = !sign;
        // }

        // p61Music();
        gFrameCounter++;
    }

    // vblank begins at vpos 312 hpos 1 and ends at vpos 25 hpos 1
    // vsync begins at line 2 hpos 132 and ends at vpos 5 hpos 18
    static void WaitVbl()
    {
        debug_start_idle(); // tell the debugger / profiler about frame info. helps make sense of frame-based profiling data.
        while (1)
        {
            volatile ULONG vpos = *(volatile ULONG *)0xDFF004;
            vpos &= 0x1ff00;
            if (vpos != (311 << 8))
                break;
        }
        while (1)
        {
            volatile ULONG vpos = *(volatile ULONG *)0xDFF004;
            vpos &= 0x1ff00;
            if (vpos == (311 << 8))
                break;
        }
        debug_stop_idle();
    }

    static void WaitLine(USHORT line)
    {
        while (1)
        {
            volatile ULONG vpos = *(volatile ULONG *)0xDFF004;
            if (((vpos >> 8) & 511) == line)
                break;
        }
    }

    static void WaitBlt()
    {
        UWORD tst = *(volatile UWORD *)&custom->dmaconr; // for compatiblity a1000
        (void)tst;
        while (*(volatile UWORD *)&custom->dmaconr & (1 << 14))
        {
        } // blitter busy wait
    }

    // mouse reading in hardware registers:
    // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0180.html

    APTR GetInterruptHandler()
    {
        return *(volatile APTR *)(((UBYTE *)VBR) + 0x6c);
    }

    void SetInterruptHandler(APTR interrupt)
    {
        *(volatile APTR *)(((UBYTE *)VBR) + 0x6c) = interrupt;
    }

    // gets the vector base register which is stored in D0 at the top of a VBL apparently?
    APTR GetVBR(void)
    {
        if (SysBase->AttnFlags & AFF_68010)
        {
            // for 68010 vbr may not be 0; must be fetched.
            const UWORD getvbr[] = {0x4e7a, 0x0801, 0x4e73}; // MOVEC.L VBR,D0 RTE
            return (APTR)Supervisor((ULONG(*)())getvbr);     // http://amigadev.elowar.com/read/ADCD_2.1/Includes_and_Autodocs_2._guide/node0386.html
        }

        return 0; // 68000 vbr is 0.
    }

    void TakeSystem()
    {
        Forbid();
        // Save current interrupts and DMA settings so we can restore them upon exit.
        SystemADKCON = custom->adkconr;
        SystemInts = custom->intenar;
        SystemDMA = custom->dmaconr;
        ActiView = GfxBase->ActiView; // store current view

        LoadView(0); // http://amigadev.elowar.com/read/ADCD_2.1/Includes_and_Autodocs_2._guide/node0459.html
        WaitTOF();
        WaitTOF();

        WaitVbl();
        WaitVbl();

        OwnBlitter();
        WaitBlit();
        Disable();

        custom->intena = 0x7fff; // disable all interrupts
        custom->intreq = 0x7fff; // Clear any interrupts that were pending
        custom->dmacon = 0x7fff; // Clear all DMA channels

        WaitVbl();
        WaitVbl();

        VBR = GetVBR();
        SystemIrq = GetInterruptHandler(); // store interrupt register
    }

    void FreeSystem()
    {
        WaitVbl();
        WaitBlit();
        // custom->intena = 0x7fff; // disable all interrupts
        // custom->intreq = 0x7fff; // Clear any interrupts that were pending
        // custom->dmacon = 0x7fff; // Clear all DMA channels

        SetInterruptHandler(SystemIrq);

        /*Restore system copper list(s). */
        custom->cop1lc = (ULONG)GfxBase->copinit;
        custom->cop2lc = (ULONG)GfxBase->LOFlist;
        custom->copjmp1 = 0x7fff; // start coppper

        /*Restore all interrupts and DMA settings. */
        custom->adkcon = SystemADKCON | 0x8000;
        custom->dmacon = SystemDMA | 0x8000;
        custom->intena = SystemInts | 0x8000;

        WaitBlit();
        DisownBlitter();
        Enable();

        LoadView(ActiView);
        WaitTOF();
        WaitTOF();

        Permit();
    }
};
