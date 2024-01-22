// # TODO: keyboard input -- this requires use of interrupts, which are more than i want atm.
// # TODO: find a way to query a timer - CIA has a timer, but it's really a frame counter which is fine for us. the line counter also exists but what's the point? i guess for profiling.
// # TODO: 32-bit fixed-point class
// # TODO: other blitting routines
// TODO: copper
// TODO: instead of COPYING the offscreen buffer, SWAP the display ptrs. requires interaction with copper, app, images, ...
// TODO: sprites
// TODO: understand parallax, scrolling

// ok fixed point. i do like the Fixed<> class but apparently 16-bit ops are fastest. but 16-bit is very small, so some TLC should probably be done
// supporting normalized values is probably necessary
// probably some other precision adjustment values is needed. for example divide precision bits goes crazy.

// https://github.com/fluffyfreak/demoscene/tree/af5c06ab2b8ebf42994a4903c8c3c05cc288b4b7/a500/effects
// this actually seems worth investigating.

// https://www.youtube.com/watch?v=e5aTb5hg3H8
// this video walks through writing a blitter routine
// it's not for arbitrary sizes, but suggests making sprite sheets with enough padding between sprites so bleed doesn't cause problems. makes sense.

// TODO: kingcon has a big limitation in that you can't seem to use the same palette across images. need to find a workflow, maybe with aesprite or pro motion.
// kingcon business/business.png bzbackground -Interleaved -Format=5
// kingcon business/circlesprites.png bzspritesMasked -Interleaved -Format=5 -Mask=1

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
#include "music.hpp"
#include "copper.hpp"
#include "app.hpp"

INCBIN_CHIP(gpBusinessPalette, "gfx/bzbackground.PAL")
INCBIN_CHIP(gpBusinessImage, "gfx/bzbackground.BPL")
INCBIN_CHIP(gpBusinessSpritesMasked, "gfx/bzspritesMasked.BPL")
INCBIN_CHIP(gpBusinessSpritesNomask, "gfx/bzsprites.BPL")

struct ExecBase *SysBase;
volatile struct Custom *custom;
struct DosLibrary *DOSBase;
struct GfxBase *GfxBase;

// TODO: understand copper instructions better.
// - wait long line
// - bitmasks for all
// - skip what is it
// - loops what are they
// TODO: values do not need to be template params; they are adjusted at runtime so...
// TODO: augment instructionRef for semantic operations
// TODO: i am thinking about making something like static constexpr uint8_t* BPLCON0 = offsetof(struct Custom, bplcon0);
//       so it can be used as simply BPLCON0
// TODO: POC: bouncy raster bars, wavy h scroll, ...

// allows callers to access & modify this instruction at runtime.
struct CopperInstructionRef
{
    uint16_t *p;
};

template <uint16_t TA, uint16_t TB>
struct CopperInstruction
{
    static constexpr uint16_t A = TA;
    static constexpr uint16_t B = TB;
    CopperInstructionRef *ref = nullptr;
};

template <uint16_t address, uint16_t value>
constexpr auto move()
{
    return CopperInstruction<(address & 0x1FE) | 0x8000, value>{};
}

template <uint16_t address, uint16_t value>
constexpr auto move(CopperInstructionRef &ref)
{
    return CopperInstruction<(address & 0x1FE) | 0x8000, value>{&ref};
}

template <uint16_t position, uint16_t mask>
constexpr auto wait()
{
    return CopperInstruction<position & 0xFFFE, (mask & 0xFFFE) | 1>{};
}

template <uint16_t position, uint16_t mask>
constexpr auto wait(CopperInstructionRef &ref)
{
    return CopperInstruction<position & 0xFFFE, (mask & 0xFFFE) | 1>{&ref};
}

template <typename... Instructions>
struct Copperlist
{
    uint16_t *mpData;
    static constexpr size_t mpDataBytes = sizeof...(Instructions) * 2;
    Copperlist(Instructions... args) : mpData((uint16_t *)AllocMem(mpDataBytes, MEMF_CHIP))
    {
        copyInstructions(mpData, args...);
    }
    ~Copperlist()
    {
        FreeMem(mpData, mpDataBytes);
    }

    template <typename Instruction, typename... Rest>
    void copyInstructions(uint16_t *p, Instruction inst, Rest... rest)
    {
        *p = Instruction::A;
        *(p + 1) = Instruction::B;
        if (inst.ref != nullptr)
        {
            inst.ref->p = p;
        }
        copyInstructions(p + 2, rest...);
    }

    void copyInstructions(uint16_t *p)
    {
        // Base case for recursion. nop.
    }
};

// ok time to understand how the copperlist works now, and put this to practice.

template <typename... Instructions>
Copperlist<Instructions...> MakeCopperlist(Instructions... args)
{
    return Copperlist<Instructions...>{args...};
}

// example usage:
void fn()
{
    CopperInstructionRef setColor0Ref;
    auto list = MakeCopperlist(
        move<offsetof(struct Custom, bplcon0), 0>()
        // move<0, 0>
    );
}

int main()
{
    // KPrintF("MAIN2()!\n");                          // debug print
    // Write(Output(), (APTR) "Hello cons1le!\n", 15); // console print
    warpmode(1);

    // how to store locations to instructions for self-modifying?
    // the instruction can take a ref maybe?
    // like
    // MoveRef color0;
    // cc::MakeCopperlist(cc::move(0, 0, color0));
    // color0.setParam(xyz); <-- self-modifying
    // auto x = MakeCopperlist(cc::move(0, 0));

    AmigaApp app;
    cc::Mouse<-32, -32, 360, 300, 48> mouse;
    cc::Palette<32> palette{gpBusinessPalette};
    cc::Image<320, 256, 5, cc::ImageFlags::Interleaved> backgroundImage{gpBusinessImage, false};
    auto backgroundSurface = backgroundImage.MakeCopy(); // the live surface
    auto offscreenSurface = backgroundImage.MakeCopy();  // the offscreen surface for double buffer
    cc::Image<64, 64, 5, CombineFlags(cc::ImageFlags::Interleaved, cc::ImageFlags::Masked)> spritesWithMask{gpBusinessSpritesMasked, false};

    cc::Blitter blitter;

    // copper 1
    USHORT *copper1 = (USHORT *)AllocMem(1024, MEMF_CHIP);
    USHORT *copPtr = copper1;

    // debug_register_copperlist(copper1, "copper1", 1024, 0);
    // debug_register_copperlist(copper2, "copper2", sizeof(copper2), 0);

    // this copperlist sets screen mode, bitplane handling...
    copPtr = screenScanDefault(copPtr);
    // enable bitplanes
    // Q: why put this in copper? why not set this stuff up and leave it?
    // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0053.html
    // A: registers are cleared each frame and need to be restored.
    *copPtr++ = offsetof(struct Custom, bplcon0);                                                                                 //  http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0092.html
    *copPtr++ = (0 << 10) /*dual pf*/ | (1 << 9) /*color*/ | ((backgroundSurface.mNonmaskBitplaneCount) << 12) /*num bitplanes*/; // playfield
    *copPtr++ = offsetof(struct Custom, bplcon1);                                                                                 // scrolling
    // scroll = copPtr;
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2); // playfied priority
    *copPtr++ = 1 << 6;                           // 0x24;			//Sprites have priority over playfields

    // set bitplane modulo
    *copPtr++ = offsetof(struct Custom, bpl1mod);            // odd planes   1,3,5
    *copPtr++ = backgroundSurface.GetWholeLineModuloBytes(); // 4 * lineSizeBytes;                // according to interleaved format, this means SKIPPING 4 lines after drawing each line.
    //*copPtr++ = 10; // according to interleaved format, this means SKIPPING 4 lines after drawing each line.
    *copPtr++ = offsetof(struct Custom, bpl2mod);            // even  planes 2,4
    *copPtr++ = backgroundSurface.GetWholeLineModuloBytes(); // 4 * lineSizeBytes;
    //*copPtr++ = 10;

    // set bitplane pointers for DISPLAY.
    // const UBYTE *planes[5];
    // for (int a = 0; a < 5; a++)
    // {
    //     // from the docs
    //     // "The memory for each bitplane must be continuous"
    //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node006B.html
    //     // so maybe we're doing some other weird stuff (bplxmod??) to force this style of interleaving.
    //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0072.html#line9
    //     // yes that's exactly what happens because modulo is set to 4xline height. that means skipping the other 4 bitplanes each line.

    //     planes[a] = (UBYTE *)image + lineSizeBytes * (a);
    // }
    // copPtr = copSetPlanes(0, copPtr, planes, 5);
    copPtr = copSetPlanes(0, copPtr, backgroundSurface.mNonmaskBitplanePointers, backgroundSurface.mNonmaskBitplaneCount);

    for (int a = 0; a < palette.mEntryCount; a++)
    {
        copPtr = copSetColor(copPtr, a, palette[a]);
    }

    // jump to copper2
    // honestly this could all be done with just 1 copperlist. even though there are 2 copperlists,
    // copper is a synchronous thing with 1 program counter. so at the end of program 1, program 2 is executed.
    // at vertical blank, COP1LC is run. So cop2lc is never run unless you explicitly start it
    //*copPtr++ = offsetof(struct Custom, copjmp2); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0051.html
    //*copPtr++ = 0x7fff;                           // apparently it doesn't even matter what value you write to this; it simply sets the copper IP to COP1LC

    // end copper
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    custom->cop1lc = (ULONG)copper1;
    custom->cop2lc = 0;            //(ULONG)copper2;
    custom->dmacon = DMAF_BLITTER; // disable blitter dma for copjmp bug
                                   // start coppper. this is the jump address strobe http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0051.html
                                   // apparently it doesn't even matter what value you write to this; it simply sets the copper IP to COP1LC

    custom->copjmp1 = 0x7fff;
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    // DEMO
    gFrameCounter = cc::ReadFrameCounter();
    app.SetInterruptHandler((APTR)AmigaApp::interruptHandler);
    custom->intena = INTF_SETCLR | INTF_INTEN | INTF_VERTB;
#ifdef MUSIC
    custom->intena = INTF_SETCLR | INTF_EXTER; // ThePlayer needs INTF_EXTER
#endif

    custom->intreq = (1 << INTB_VERTB); // reset vbl req

    warpmode(0);

    uint32_t frame = gFrameCounter;
    offscreenSurface.mDirtyRect.MarkAsClean();
    while (true)
    {
        uint32_t processingLines = cc::ReadLineCounter();

        // // whole screen clear
        // cc::Blitter::BlitDupe(backgroundImage, offscreenSurface);
        // offscreenSurface.mDirtyRect.MarkAsClean();

        cc::Blitter::CleanRectWithSource(backgroundImage, offscreenSurface);

        cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, offscreenSurface, offscreenSurface, mouse.mPointerX, mouse.mPointerY);
        // cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, offscreenSurface, offscreenSurface, 320 / 2 - 32, 256 / 2 - 32);

        {
            auto t = cc::Fixed<8, 8, uint16_t>::FromUnderlyingValue((gFrameCounter * 3) & 0xff);
            auto s = t.Sine16_2pi();
            auto sh = s.Multiply(cc::Fixed<8, 8, uint16_t>::FromNumber(320 / 6));

            auto ty = cc::Fixed<8, 8, uint16_t>::FromUnderlyingValue((gFrameCounter * 9 / 2) & 0xff);
            auto sy = ty.Sine16_2pi();
            auto shy = sy.Multiply(cc::Fixed<8, 8, uint16_t>::FromNumber(256 / 6));

            cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, offscreenSurface, offscreenSurface, 320 / 2 + sh.IntPart() - 32, 256 / 2 + shy.IntPart() - 32);
        }
        int16_t dirtyX = offscreenSurface.mDirtyRect.GetDirtyRectStartXPixel();
        int16_t dirtyY = offscreenSurface.mDirtyRect.mDirtyRectStartY;
        int16_t dirtyW = offscreenSurface.mDirtyRect.GetDirtyRectWidthPixels();
        int16_t dirtyH = offscreenSurface.mDirtyRect.GetDirtyRectHeightLines();

        AmigaApp::WaitVbl();
        mouse.OnFrame();
        uint32_t thisFrame = gFrameCounter;
        uint32_t frameSpan = (thisFrame - frame);
        frame = thisFrame;

        cc::Blitter::BlitDirtyRect(offscreenSurface, backgroundSurface);
        // cc::Blitter::BlitDupe(offscreenSurface, backgroundSurface);aoeu

        // auto a = cc::fixed<5>();
        // auto b = cc::fixed<3>();
        // auto c = cc::fixed<-3, int32_t>();
        // auto d = (a + b);
        // auto e = d / c;
        // DbgPrintF("a=%s b=%s c=%s, d=%s e=%s",
        //           a.ToString().str,
        //           b.ToString().str,
        //           c.ToString().str,
        //           d.ToString().str,
        //           e.ToString().str);

        auto a = cc::fixed<5>();
        auto b = a * 20;

        // auto a = cc::fixed<-5>();
        // auto x = cc::Fixed<3,28,int32_t>{a};
        // auto c = cc::fixed<3, int32_t>();
        // auto e = a / c;
        // auto f = e.Abs();
        // int16_t xxx = 8192;
        // cc::FixedFromFractionHelper<1,2,int16_t> helper;
        // cc::FixedFromFractionHelper<1,-2,int16_t> helperneg;
        // auto r = cc::fixed<-1,2, int16_t>();
        // auto str = r.ToString();
        // DbgPrintF("r=%s",
        //           str.str);
        // DbgPrintF("a=%s x=%s c=%s, e=%s f=%s",
        //           a.ToString().str,
        //           x.ToString().str,
        //           c.ToString().str,
        //           e.ToString().str,
        //           f.ToString().str);

// auto a = cc::fixed<1,-4, int16_t>();
// cc::FixedFromFractionHelper<1,-4,int16_t> helper;
// auto str = a.ToString();
// DbgPrintF("%s", str.str);

        uint32_t processingLines2 = cc::ReadLineCounter();
        debug_clear();
        debug_filled_rect(0, 0, 400, 60, 0x80004444); // 0x00RRGGBB
        debug_text_format(20, 30, 0x00cc6600, "frameSpan:%d, lineDelta:%d, mouse:(%d,%d)", frameSpan, processingLines2 - processingLines, mouse.mPointerX, mouse.mPointerY);
        debug_text_format(20, 50, 0x00cc6600, "offscrDirty:(%d,%d)[%d x %d]", dirtyX, dirtyY, dirtyW, dirtyH);
        // debug_text_format(20, 70, 0x00cc6600, "5/3 = %s", ft.str);
        debug_rect(dirtyX * 2 + 72, dirtyY * 2 + 36, (dirtyX + dirtyW) * 2 + 72, (dirtyY + dirtyH) * 2 + 36, 0xff0000);
    }

    p61End();
    return 0;
}
