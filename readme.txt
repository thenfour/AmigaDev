this project is my attempt to learn hardware programming on A500 OCS.
attempting to write a blitter function is not easy, and the proper thing is now to probably just not support non-word-aligned source rects.
then make a copper "programmer" class
then abstract music (& timing?)
and make something.

random notes to follow.


// when using interleaved + mask, kingcon will output DOUBLE the number of bitplanes, not just 1 extra for mask.
// OK I see why. it's because when you do a blit on interleaved data, the whole point is to do it in 1 big block copy,
// rather than one for each bitplane.
//
// that is to say the copy is the same size on all channels involved. so your mask needs to be the exact same size as the image data.
//
// when kingcon outputs the file, the mask is interleaved with bitplanes.
//
// so, a 4x4 3-bitplane non-interleaved is like... and blitting the first 2 lines would look like:
// bitplane 0 line 0 <--copy src
// bitplane 0 line 1 <--copy src
// bitplane 0 line 2
// bitplane 0 line 3
// bitplane 1 line 0 <--copy src
// bitplane 1 line 1 <--copy src
// bitplane 1 line 2
// bitplane 1 line 3
// bitplane 2 line 0 <--copy src
// bitplane 2 line 1 <--copy src
// bitplane 2 line 2
// bitplane 2 line 3
// there's no way to do this in 1 block transfer, even with the modulo configuration.

// Interleave that file and it looks like this
// bitplane 0 line 0 <--copy src
// bitplane 1 line 0 <--copy src
// bitplane 2 line 0 <--copy src
// bitplane 0 line 1 <--copy src
// bitplane 1 line 1 <--copy src
// bitplane 2 line 1 <--copy src
// bitplane 0 line 2
// bitplane 1 line 2
// bitplane 2 line 2
// bitplane 0 line 3
// bitplane 1 line 3
// bitplane 2 line 3
// this is easily possible in 1 block using modulo for different sized surfaces

// non-interleaved with mask is PROBABLY (unconfirmed)
// bitplane 0 line 0 <--copy src
// bitplane 0 line 1 <--copy src
// bitplane 0 line 2
// bitplane 0 line 3
// bitplane 1 line 0 <--copy src
// bitplane 1 line 1 <--copy src
// bitplane 1 line 2
// bitplane 1 line 3
// bitplane 2 line 0 <--copy src
// bitplane 2 line 1 <--copy src
// bitplane 2 line 2
// bitplane 2 line 3
// bitmask line 0 <--MASK src
// bitmask line 1 <--MASK src
// bitmask line 2
// bitmask line 3
// this is especially difficult. the data size is good, but the mask src is not the same size as the image source, plus the above issue of requiring multiple passes.

// Add a mask, and it looks like this
// bitplane 0 line 0 <--copy src
// mask bpl 0 line 0 <-- MASK src
// bitplane 1 line 0 <--copy src
// mask bpl 1 line 0 <-- MASK src
// bitplane 2 line 0 <--copy src
// mask bpl 2 line 0 <-- MASK src
// bitplane 0 line 1 <--copy src
// mask bpl 0 line 1 <-- MASK src
// bitplane 1 line 1 <--copy src
// mask bpl 1 line 1 <-- MASK src
// bitplane 2 line 1 <--copy src
// mask bpl 2 line 1 <-- MASK src
// bitplane 0 line 2
// mask bpl 0 line 2
// bitplane 1 line 2
// mask bpl 1 line 2
// bitplane 2 line 2
// mask bpl 2 line 2
// bitplane 0 line 3
// mask bpl 0 line 3
// bitplane 1 line 3
// mask bpl 1 line 3
// bitplane 2 line 3
// mask bpl 2 line 3
// this is again possible in 1 block, because mask is the same size as image data; modulo just needs to be increased to skip across the mask line.

// regarding bitfields, playfields.
// http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0061.html
// summary of registers (good) http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0092.html

// regarding blitter:
// http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0118.html
// explanation of shifting: http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011F.html#line151

// regarding copper:
// http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0047.html
// more advanced: http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0058.html

// http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0064.html

// LIBRARIES documentation: (gfx, exec, dos)
// http://amigadev.elowar.com/read/ADCD_2.1/Libraries_Manual_guide/node0000.html

// ILBM format (DpaintIV image file format)
// https://moddingwiki.shikadi.net/wiki/LBM_Format
// https://netghost.narod.ru/gff/vendspec/iff/iff.txt
// https://en.wikipedia.org/wiki/ILBM
// DpaintIV also saves palettes in an IFF ILBM format. starting at hex $30, R G B byte values in sequence.

// kingcon - utility for converting images
// https://github.com/grahambates/kingcon

// https://forum.amiga.org/index.php?topic=58326.0

// this copperlist just sets palette colors
// put copperlist into chip mem so we can use it without copying
// const UWORD copper2[] __attribute__((section(".MEMF_CHIP"))) = {
// offsetof(struct Custom, color[0]), 0x0000,
// 0x4101, 0xff00, offsetof(struct Custom, color[0]), 0x0121, // line 0x41
// offsetof(struct Custom, color[1]), 0x0114,                 // line 0x41
// 0x4201, 0xff00, offsetof(struct Custom, color[0]), 0x0222, // line 0x42
// 0x4301, 0xff00, offsetof(struct Custom, color[0]), 0x0333, // line 0x43
// 0x4401, 0xff00, offsetof(struct Custom, color[0]), 0x0444, // line 0x44
// 0x4501, 0xff00, offsetof(struct Custom, color[0]), 0x0555, // line 0x45
// 0x4601, 0xff00, offsetof(struct Custom, color[0]), 0x0666, // line 0x46
// 0x4701, 0xff00, offsetof(struct Custom, color[0]), 0x0777, // line 0x47
// 0x4801, 0xff00, offsetof(struct Custom, color[0]), 0x0288, // line 0x48
// offsetof(struct Custom, color[1]), 0x0828,                 // line 0x48
// 0x4901, 0xff00, offsetof(struct Custom, color[0]), 0x0999, // line 0x49
// 0x4a01, 0xff00, offsetof(struct Custom, color[0]), 0x0aaa, // line 0x4a
// 0x4b01, 0xff00, offsetof(struct Custom, color[0]), 0x0bbb, // line 0x4b
// 0x4c01, 0xff00, offsetof(struct Custom, color[0]), 0x0ccc, // line 0x4c
// 0x4d01, 0xff00, offsetof(struct Custom, color[0]), 0x0ddd, // line 0x4d
// 0x4e01, 0xff00, offsetof(struct Custom, color[1]), 0x0e2e, // line 0x4e
// offsetof(struct Custom, color[0]), 0x0000,                 // line 0x4e
// 0x4f01, 0xff00, offsetof(struct Custom, color[0]), 0x0fff, // line 0x4e
//     0xffff, 0xfffe // end copper list
// };


// // control register. no shift, only D (but ... what about A??) i think A is not specified here because we don't use an A ptr. just the dest, and an A value (adat)
// custom->bltcon0 = A_TO_D /*logic*/ | DEST;
// custom->bltcon1 = 0; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

// // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001E.html  "normally loaded by the blitter DMA channel"
// // for us, we use a constant val for A.
// // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html
// // it means do NOT use "SRCA". SRCA specifically means to use DMA access, and disabling it means specifically that you will use ADAT.
// custom->bltadat = frameCounter; // 0xb4;
// custom->bltbdat = 0;
// custom->bltcdat = 0;

// // // copies A to D. D is the memory after A.
// // line 100
// custom->bltdpt = (UBYTE *)image.mpData + (320 / 8) * 130 * 5; // "D" pointer. http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0020.html

// // // why D mod = 0?. it's the # of pixels to skip for each line. http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001F.html
// // // maybe this is not the "stride", but the EMPTY bits to skip after each line? CORRECT. because we draw the full 320 x pixels, nothing to skip.
// custom->bltdmod = 0; // 320/16*4;
// // "AND" masks; i guess convenient masks for manipulating source data. http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0019.html
// // this is pretty much for partial first & last words of the blit, in case you're blitting a non-word-aligned size.
// custom->bltafwm = custom->bltalwm = 0xffff;
// // // sets area size and invokes the blitter.
// // // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001D.html.
// // // bits 0-5 (6 bits) = width. 6-15 (10 bits) = width, both 1024 max in theory. width is in WORDs.
// // // unit is in pixels/lines.
// // // 56 lines high * 5 bit planes suggests indeed the interleaving, but not necessarily so. would be the same calculation either way.
// // // 320/8=40 bytes /2= 20 WORDS
// // custom->bltsize = ((56 * 5) << HSIZEBITS) | (320 / 16); // 56 lines
// // 7 lines
// custom->bltsize = ((7 * 5) << HSIZEBITS) | (320 / 16); // 56 lines

// blit
// for (short i = 0; i < 16; i++)
// {
//     short x = i * 16 + frameCounter * i; // + sinus32[(frameCounter + i) % sizeof(sinus32)] * 2;
//     x = x & 15;
//     short y = 0;                         // sinus40[((frameCounter + i) * 2) & 63] / 2;
//     UBYTE *src = (UBYTE *)bob + 32 / 8 * 10 * 16;// * (i % 6); // colors from the various variations of the same bob on teh image

//     WaitBlit();
//     // value
//     // 0x                800 400 200 100  80  40  20  10   8   4   2   1
//     // bit
//     //    15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
//     //    |------------| 4 bits of shifting A source (channel?)
//     //                    |A--B---C-Dest| use a, b, c, d
//     //                                    |x---x-----------x-------x----| 0xCA = logic functions
//     // 0xca = 1100 1010
//     custom->bltcon0 = A_TO_D | DEST;
//     //custom->bltcon0 = 0xca | SRCA | SRCB | SRCC | DEST | ((x & 15) << ASHIFTSHIFT); // A = source, B = mask, C = background, D = destination
//     // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
//     custom->bltcon1 = ((x & 15) << BSHIFTSHIFT);
//     custom->bltapt = src; // pointer to "A" data - in the "bob" image
//     custom->bltamod = 32 / 8;
//     //custom->bltbpt = src + 32 / 8 * 1; // pointer to "B" data ---- MASK seems to be in the same bitmap. though i don't see it.
//     //custom->bltbmod = 32 / 8;
//     //custom->bltcpt = custom->bltdpt = (UBYTE *)image + 320 / 8 * 5 * (200 + y) + x / 8;
//     //custom->bltcmod = custom->bltdmod = (320 - 32) / 8;
//     custom->bltafwm = custom->bltalwm = 0xffff;
//     custom->bltsize = ((16 * 5) << HSIZEBITS) | (32 / 16);
// }



        // blitter.ClearLines(backgroundSurface, 100, 80, frameCounter);
        // blitter.ClearLines(backgroundSurface, 0, 200, 0);

        // blitter.BlitLines(backgroundImage, backgroundSurface, 80, 120, 40);
        // blitter.BlitMaskedBob(sprites, spritesMask, backgroundSurface, 0, 0, frameCounter & 255, 100, 32,32);
        // USHORT w = 32;
        // KPrintF("w:%u", FWord(w));
        // KPrintF("w:%x", w);
        // problem of word size in arg passing.
        // blitter.BlitMaskedBob(sprites, spritesMask, backgroundSurface, 0, 0, 0, 0, 32, 32);
        // blitter.BlitBob(backgroundImage, backgroundSurface, 0, 0, 0, 0, 100, 100);
        // blitter.BlitBob(sprites, backgroundSurface, 0, 0, f & 0x7f, f & 0x4f, 32, 32);
        // blitter.BlitBob(sprites, backgroundSurface, 1, 0, 67, 70, 63,63);

        // WinUAE debug overlay test
        // debug_clear();
        // debug_filled_rect(f + 100, 200 * 2, f + 400, 220 * 2, 0x0000ff00); // 0x00RRGGBB
        // debug_rect(f + 90, 190 * 2, f + 400, 220 * 2, 0x000000ff);         // 0x00RRGGBB
        // debug_text(20, 10, "This is a WinUAE debug overlay", 0x00ff00ff);

        // case 1a: src bleed == dest bleed == 0
        // case 1b: src bleed == dest bleed == 2 (shift -2+2=0) <--introduces first & last word mask

        // positive shift
        // case 2a: src bleed = 0, dest bleed = 2 (shift 0 + 2 = 2)
        // case 2b: src bleed = 3, dest bleed = 7 (shift -3 + 7 = 4)

        // negative shift
        // case 3a: src bleed = 14, dest bleed = 0 (shift -14 + 0 = -14)
        // case 3a: src bleed = 7, dest bleed = 3 (shift -7 + 3 = -4)

        // pre-processing coordinates:
        // when dest coord is negative, inc src coord and set dest coord to 0.
        // when dest coord + size is off screen, dec size.
        // <0 area = NOP.
        // as long as we can handle arbitrary coordinates then this can just be preprocessed.

        // OK are there issues forseen with the selection of descending?
        // i mean, if a left shift is required, and we can't shift 1 word, then descending mode is necessary.
        // but the same issue must then be present in descending mode except in the other direction no? well i guess with clamping this can't happen.

        // {
        //     // case 1a: src bleed == dest bleed == 0
        //     WaitBlit();

        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011C.html
        //     const int srcX = 16;
        //     const int srcY = 16;
        //     const int width = 32;
        //     const int height = 32;

        //     // A = mask, B = src, C = background
        //     constexpr auto logic = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
        //     // constexpr auto logic = ABC | ANBC | NABNC | NABC; // B masked by A inverted. all not-a-[anyb]-c + A-B-[anyc]
        //     //  constexpr auto logic = ABC | ABNC | NABC | NABNC; // just use B. so all the [B]
        //     custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (0 << ASHIFTSHIFT);
        //     custom->bltcon1 = (0 << BSHIFTSHIFT); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     // A = mask
        //     custom->bltapt = spritesWithMask.AtMaskByteOffset(0); // bitplane 0 offset
        //     custom->bltamod = 12;                                 // img width 64, + mask 64; blit width 32 means 32 remaining / 8 = 4 + mask 8 = 12
        //     // custom->bltadat = 0xaaaa;
        //     custom->bltafwm = 0xffff; // calc.srcFirstWordMask;
        //     custom->bltalwm = 0xffff; // calc.srcLastWordMask;

        //     // B = src
        //     custom->bltbpt = spritesWithMask.AtByteOffset(0);
        //     custom->bltbmod = 12; // same as A mask.

        //     // C = background
        //     size_t bgOffset = 40 * 5 * 100 + 10; // 40 pitch * 5 bitplanes * 100 lines, +20 (*8=160pixels)
        //     custom->bltcpt = (APTR)(backgroundImage.mpData + bgOffset);
        //     custom->bltcmod = 36; // img width 320 - 32 = 2 (efficient to calc together with above)

        //     APTR dpt = (APTR)(backgroundSurface.mpData + bgOffset);
        //     custom->bltdpt = dpt;
        //     custom->bltdmod = 36; // same as C.

        //     custom->bltsize = ((height * 5) << HSIZEBITS) | (width / 16);
        // }

        // {
        //     // case 1b: src bleed == dest bleed == 2 (shift -2+2=0)
        //     WaitBlit();

        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011C.html

        //     // A = mask, B = src, C = background
        //     constexpr auto logic = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
        //     //constexpr auto logic = ABC | ABNC | NABC | NABNC; // just use B. so all the [B]
        //       //constexpr auto logic = ABC | ABNC | ANBC | ANBNC; // just use A. so all the [A]
        //     custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (0 << ASHIFTSHIFT);
        //     custom->bltcon1 = (0 << BSHIFTSHIFT); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     // A = mask
        //     custom->bltapt = spritesWithMask.AtMaskByteOffset(0); // bitplane 0 offset
        //     custom->bltbmod = custom->bltamod = 10;                                 // img width 64, + mask 64; blit width 48 means 16 remaining / 8 = 2 + mask 8 = 10
        //     custom->bltafwm = "0000 0011 1111 1111"_b;
        //     custom->bltalwm = "1111 1100 0000 0000"_b;

        //     // B = src
        //     custom->bltbpt = spritesWithMask.AtByteOffset(0);

        //     // C = background
        //     size_t bgOffset = 40 * 5 * 150 + 20; // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels)
        //     custom->bltcpt = (APTR)(backgroundImage.mpData + bgOffset);
        //     custom->bltdmod = custom->bltcmod = 34; // img width 320 - 32 = 2 (efficient to calc together with above)

        //     APTR dpt = (APTR)(backgroundSurface.mpData + bgOffset);
        //     custom->bltdpt = dpt;

        //     custom->bltsize = ((32 * 5) << HSIZEBITS) | (48 / 16); // 48 width because we capture 3 bits of the right one.
        // }

        // {
        //     // case 2a: src bleed = 0, dest bleed = 2 (shift 0 + 2 = 2)
        //     // here we take a normal word-aligned src value, and attempt to blit to a positive-shifted location on dest.
        //     WaitBlit();

        //     // A = mask, B = src, C = background
        //     constexpr auto logic = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
        //                                                       // constexpr auto logic = ABC | ABNC | NABC | NABNC; // just use B. so all the [B]
        //     // constexpr auto logic = ABC | ABNC | ANBC | ANBNC; // just use A. so all the [A]
        //     int16_t shift = f & 15;
        //     custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
        //     custom->bltcon1 = (shift << BSHIFTSHIFT); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     // when shifting, actually it's not so simple.
        //     // shift is not really a shift, it's a ROTATE. so in order to have 32 bits shifted 5 right
        //     //
        //     //  word0               word1               word2               word3
        //     // |-------------------|-------------------|-------------------|-------------------
        //     // |abcdexxxxxxxxxxxxxxxxxxxxxxxxxxxxx12345|        src area
        //     //      |abcdexxxxxxxxxxxxxxxxxxxxxxxxxxxxx12345|    intended dest area
        //     // |...........................................................|         we must blit an extra word to contain both edges.
        //     //
        //     // now what if src is not big enough? it's generally OK, because we'll mask out that invalid portion of the image.
        //     // so mask.
        //     // |MMMM|---------------------------------------|MMMMMMMMMMMMMM|
        //     // NOTE: the mask gets rotated too. it's part of the SRC.
        //     // so the mask shall exist to correct the additional src blit. therefore,
        //     // |1111111111111111111|...................|0000000000000000000|

        //     // A = mask
        //     custom->bltapt = spritesWithMask.AtMaskByteOffset(0); // bitplane 0 offset
        //     custom->bltbmod = custom->bltamod = 10;               // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
        //     custom->bltafwm = 0xffff;                             //"0000 0001 1111 1111"_b;
        //     custom->bltalwm = 0;                                  //"1111 1111 1000 0000"_b;

        //     // B = src
        //     custom->bltbpt = spritesWithMask.AtByteOffset(0);

        //     // C = background
        //     size_t bgOffset = 40 * 5 * 150 + 10; // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels)
        //     custom->bltcpt = (APTR)(backgroundImage.mpData + bgOffset);
        //     custom->bltdmod = custom->bltcmod = 34; // img width 320 - 32 = 2 (efficient to calc together with above)

        //     APTR dpt = (APTR)(backgroundSurface.mpData + bgOffset);
        //     custom->bltdpt = dpt;

        //     custom->bltsize = ((32 * 5) << HSIZEBITS) | (48 / 16); // 48 width because we capture 3 bits of the right one.
        // }

        // {
        //     // case 2b: src bleed = 3, dest bleed = 7 (shift -3 + 7 = 4)
        //     // here we take a normal word-aligned src value, and attempt to blit to a positive-shifted location on dest.
        //     // the only change to 2a is that our masking has to account for src offset.

        //     // there is a vexing (impossible?) case, where source end is not word-aligned, onto a non-word-aligned pos of dest.
        //     // when src requires a last word mask, and right shift widens the blit by a word, then the last word mask theoretically also must be widened but it's not supported in hardware.
        //     // this page describes some complexities in arbitrary blitting: http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0121.html
        //     // however it doesn't give explicit solutions for vexing scenarios.
        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    abcdefghijklmnop ppqrstuvwxyz0123 456789xABCDEFGHI                  src image
        //     // SRC:    abcdefghijklmnop ppqrstuvwxyz0...                                   srcX = 0, width 28 means masking out (.).
        //     // DEST:                   |>>>>>>>>>>>>abcd efghijklmnoppppq rstuvwxyz0       destX = 27 will begin blitting here, and shift right 12.

        //     WaitBlit();

        //     // A = mask, B = src, C = background
        //     //constexpr auto logic = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
        //                                                       // constexpr auto logic = ABC | ABNC | NABC | NABNC; // just use B. so all the [B]
        //     constexpr auto logic = ABC | ABNC | ANBC | ANBNC; // just use A. so all the [A]
        //     //int16_t shift = 4;
        //     int16_t shift = f & 15;
        //     custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
        //     custom->bltcon1 = (shift << BSHIFTSHIFT); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     // when shifting, actually it's not so simple.
        //     // shift is not really a shift, it's a ROTATE. so in order to have 32 bits shifted 5 right
        //     //
        //     //  word0               word1               word2               word3
        //     // |-------------------|-------------------|-------------------|-------------------
        //     // |abcdefghijklmnop...|rstuvwxyz0123456789|ABCDEFGHIJKLMNOPQRS| src image;
        //     //  ...defghijklmnoppppprstuvwxyz0123456789xABC                  src blit (shift -3) masking will remove the unwanted parts. if dest bleed is 0, then shift -3.
        //     // |0001111111111111111|...................|1110000000000000000| <-- src mask
        //     //  .......defghijklmnoppppprstuvwxyz0123456789xABC    intended dest (dest shift 7) -- actual shift 4 because 3 are virtual
        //     // |...........................................................|         we must blit an extra word to contain both edges.
        //     // and when shifted +4,

        //     // A = mask
        //     custom->bltapt = spritesWithMask.AtMaskByteOffset(0); // bitplane 0 offset
        //     custom->bltbmod = custom->bltamod = 10;               // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
        //     custom->bltafwm = "0001 1111 1111 1111"_b;
        //     custom->bltalwm = "1110 0000 0000 0000"_b;

        //     // B = src
        //     custom->bltbpt = spritesWithMask.AtByteOffset(0);

        //     // C = background
        //     size_t bgOffset = 40 * 5 * 150 + 10; // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels)
        //     custom->bltcpt = (APTR)(backgroundImage.mpData + bgOffset);
        //     custom->bltdmod = custom->bltcmod = 34; // img width 320 - 32 = 2 (efficient to calc together with above)

        //     APTR dpt = (APTR)(backgroundSurface.mpData + bgOffset);
        //     custom->bltdpt = dpt;

        //     custom->bltsize = ((32 * 5) << HSIZEBITS) | (48 / 16); // 48 width because we capture 3 bits of the right one.
        // }

        // {
        // // demonstration of descending mode.
        //     // preparation for
        //     // case 3a: src bleed = 14, dest bleed = 0 (shift -14 + 0 = -14)
        //     // so this is case 3*: standard blit but descending mode.
        //     WaitBlit();

        //     // A = mask, B = src, C = background
        //     constexpr auto logicBmaskA = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
        //     constexpr auto logicB = ABC | ABNC | NABC | NABNC;      // just use B. so all the [B]
        //     constexpr auto logicA = ABC | ABNC | ANBC | ANBNC;      // just use A. so all the [A]
        //     int16_t shift = f & 15;
        //     custom->bltcon0 = (logicBmaskA) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
        //     custom->bltcon1 = (shift << BSHIFTSHIFT) | BLITREVERSE; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     //  word0            word1            word2            word3
        //     // |----------------|----------------|----------------|----------------
        //     // |abcdefghijklmnoppppqrstuvwxyz0123456789xABCDEFGHIJ   src image
        //     // |        ijklmnoppppqrstuvwxyz0123456789xAB           intended slice (2 words)
        //     // |                    ijklmnoppppqrstuvwxyz0123456789xAB     intended destination
        //     // |                >                                         the word alignment used for dest
        //     // |                >abcdefghijklmnoppppqrstuvwxyz0123456789xABCDEFGHIJ                                         superimposing src on the dest word
        //     // notice how a negative shift is required.
        //     // because negative shifts are not possible, ONE solution is to step left 1 word so the shift goes positive.
        //     //  word0            word1            word2            word3
        //     // |----------------|----------------|----------------|----------------
        //     // >                                         new word alignment used for dest (shift -4 + 16 = +12)
        //     // >abcdefghijklmnoppppqrstuvwxyz0123456789xABCDEFGHIJ                                         superimposing src on the dest word
        //     // >            abcdefghijklmnoppppqrstuvwxyz0123456789xABCDEFGHIJ                                         now shift +12
        //     // >            --------ijklmnoppppqrstuvwxyz0123456789xABCDEFGHIJ                                         with mask
        //     // but this is not really ideal, because there's not always going to be room to do that.
        //     // the blitter provides a weird solution: descending mode, where everything goes in reverse.
        //     // DESC    Descending (decreasing address) control bit
        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0120.html
        //     // summary: http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node012D.html
        //     //
        //     // mod values stay the same, size stays the same.
        //     // but:
        //     // - PT should point to the "last word".
        //     // - first & last word masks are swapped
        //     // - shifts are left instead of right.

        //     // A = mask

        //     // my calculation shows it should be 5110.
        //     // 80 bytes per line, line #63 = 5040, but we need the last bitplane (4 bitplanes * 16 bytes = +64 = 5104). and +6 for the last word of the blit.
        //     // constexpr int soffsetBytes = spritesWithMask.GetLineOffsetBytes(64) + 6; // i can't yet understand why this is 64th line, not 63rd. 64th line should be the first one NOT drawn. possibly because of my bad bitplane calc
        //     int soffsetBytes = 5110;

        //     custom->bltapt = (APTR)spritesWithMask.mpMask0 + soffsetBytes; // bitplane 0 offset
        //     custom->bltbmod = custom->bltamod = 8;                         // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
        //     custom->bltafwm = "1111 1111 1111 1111"_b;
        //     custom->bltalwm = "1111 1111 1111 1111"_b;

        //     // B = src
        //     // custom->bltbpt = (APTR)( spritesWithMask.mNonmaskBitplanePointers[4][soffsetBytes]);
        //     custom->bltbpt = (APTR)(spritesWithMask.mpData + soffsetBytes);

        //     // C = background. let's put it at
        //     size_t bgOffset = (40 * 5 * (150 + 64)) + (48 / 8 + 10); // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels) -- end offset +64x64 but x-=16 (thus 64-16=48) because we need LAST not END
        //     // this is the "end". to make it "last", subtract a word.
        //     // bgOffset -= 2;
        //     custom->bltcpt = (APTR)(backgroundImage.mpData + bgOffset);
        //     custom->bltdmod = custom->bltcmod = (320 - 64) / 8; // img width 320 - 32 = 2 (efficient to calc together with above)

        //     APTR dpt = (APTR)(backgroundSurface.mpData + bgOffset);
        //     custom->bltdpt = dpt;

        //     custom->bltsize = ((64 * 5) << HSIZEBITS) | (64 / 16); // 48 width because we capture 3 bits of the right one.
        // }

        // // easy case. Y clipping
        //         cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, backgroundImage, backgroundSurface, (f & 15) + 121, -19);
        //         cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, backgroundImage, backgroundSurface, (f & 15) + 181, 198);
        //         cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, backgroundImage, backgroundSurface, (f & 15) + 231, 250);

        // // right clipping
        //         cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, backgroundImage, backgroundSurface, (f & 31) + 293, 96 + (f & 7));
        //         cc::Blitter::BlitMasked<48, 0, 16, 16>(spritesWithMask, backgroundImage, backgroundSurface, (f & 31) + 293, 164 + (f & 7));

        // left clipping
        // cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, backgroundImage, backgroundSurface, -32, 120);
        // cc::Blitter::BlitMasked<0, 0, 64, 64>(spritesWithMask, backgroundImage, backgroundSurface, -64 + (f & 63), 120);




        // struct BlitCalculation
        // {
        //     // offset from a bitplane start to the first word, but in pixels. used for blitter source pointer calc.
        //     ULONG srcBeginOffset;
        //     ULONG destBeginOffset;

        //     ULONG widthWords; // the blit width shall be the same for src & dest.

        //     UBYTE srcLeftBleed;  // # of pixels less than a word which are masked into the leftmost word.
        //     UBYTE srcRightBleed; // # of pixels less than a word which are masked into the rightmost word.
        //     USHORT srcFirstWordMask;
        //     USHORT srcLastWordMask;

        //     // dest doesn't have any masking; it should be done by the logic ops.

        //     USHORT srcModuloBytes;  // bytes to skip to get to the next line. note that for interleaved data, we copy in 1 big chunk; don't skip bitplanes.
        //     USHORT destModuloBytes; // bytes to skip to get to the next line.

        //     // signed, accounts for dest shifting right by up to 1 word., enabling pixel-fine positioning of the source onto the dest.
        //     UBYTE xShift;

        //     void Dump() const
        //     {
        //         DbgPrintF("widthW:%d srcBegin:%d srcMod:%d destBegin:%d destMod:%d [shift:%d] [masks:%b %b] [srcLeftBleed:%d]",
        //                   widthWords,
        //                   srcBeginOffset,
        //                   srcModuloBytes,
        //                   destBeginOffset,
        //                   destModuloBytes,
        //                   xShift,
        //                   srcFirstWordMask,
        //                   srcLastWordMask,
        //                   srcLeftBleed);
        //     }
        // };

        // // given offset & size...
        // template <USHORT TwidthA, USHORT TheightA, USHORT TbitplanesA, ImageFlags TflagsA,
        //           USHORT TwidthD, USHORT TheightD, ImageFlags TflagsD>
        // static constexpr BlitCalculation sCalculateBlit(const Image<TwidthA, TheightA, TbitplanesA, TflagsA> &src,
        //                                                 const Image<TwidthD, TheightD, TbitplanesA, TflagsD> &dest,
        //                                                 USHORT srcX, USHORT srcY, USHORT destX, USHORT destY, USHORT width, USHORT height)
        // {
        //     // if you pass in 32, it's on a word boundary so return 0 for offset.
        //     // if you pass in 42, 10 bits need to be discarded; return %0000000000111111
        //     BlitCalculation ret;

        //     USHORT srcLineOffsetWords = srcX / 16; // floor, to catch all partial left words.
        //     ret.srcLeftBleed = srcX & 15;
        //     ret.srcFirstWordMask = (ret.srcLeftBleed == 0 ? 0xffff : setRightmostBits(ret.srcLeftBleed));
        //     USHORT endOffsetWords = (srcX + width + 15) / 16; // first UNUSED word, ceil.

        //     ret.widthWords = endOffsetWords - srcLineOffsetWords;
        //     ret.srcRightBleed = (srcX + width) & 15;
        //     ret.srcLastWordMask = (ret.srcRightBleed == 0 ? 0xffff : setLeftmostBits(ret.srcRightBleed));

        //     // calc dest values.
        //     USHORT destLineOffsetWords = destX / 16; // floor
        //     USHORT destLeftBleed = destX & 15;
        //     SHORT shift = (SHORT)destLeftBleed - ret.srcLeftBleed;
        //     if (shift < 0)
        //     {
        //         // don't shift left. actually we should in theory switch to descending mode and recalc everything.
        //         // but for now just force shifting right. the only issue is when blitting to the left edge and there's nowhere to shift.
        //         // then, we could increment the src pointer but bleh this is getting messy.
        //         // i think in the end i really have no choice but to explore the descending mode, because when dealing with smaller sprites you just don't have the margin for this.

        //         // srcx 1, width 15, dest 32
        //         // src offset word = 0 (shift -1)
        //         // width words = 1
        //         // dest offset word = 2
        //         // and because shift is -1, dec dest offset, shift += 16 = shift 15.
        //         if (destLineOffsetWords > 0)
        //         {
        //             shift += 16;
        //             destLineOffsetWords--;
        //         }
        //     }

        //     ret.xShift = shift;

        //     ret.srcModuloBytes = src.GetLineStrideBytes() - ret.widthWords * 2;   // modulo bytes if 0 pixels are processed.
        //     ret.destModuloBytes = dest.GetLineStrideBytes() - ret.widthWords * 2; // modulo bytes if 0 pixels are processed.
        //     ret.srcBeginOffset = src.GetLineOffsetBytes(srcY) + srcLineOffsetWords * 2;
        //     ret.destBeginOffset = dest.GetLineOffsetBytes(destY) + destLineOffsetWords * 2;

        //     return ret;
        // }

        // // clear lines of an image using a constant value.
        // // optimal for interleaved images (TODO: support non-interleaved)
        // // optimized because copying full lines is possible in 1 continuous simple block
        // template <USHORT Twidth, USHORT Theight, USHORT Tbitplanes, ImageFlags Tflags>
        // static void ClearLines(const Image<Twidth, Theight, Tbitplanes, Tflags> &img, USHORT startLine, USHORT sizeLines, USHORT value = 0)
        // {
        //     WaitBlit();

        //     // for interleaved bitmaps, you can clear lines quickly because all bitplane lines are grouped together in 1 continuous hunk.

        //     // control register. no shift, only D (but ... what about A??) i think A is not specified here because we don't use an A ptr. just the dest, and an A value (adat)
        //     custom->bltcon0 = A_TO_D /*logic*/ | DEST;

        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001E.html  "normally loaded by the blitter DMA channel"
        //     // for us, we use a constant val for A.
        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html
        //     // it means do NOT use "SRCA". SRCA specifically means to use DMA access, and disabling it means specifically that you will use ADAT.
        //     custom->bltadat = value; // 0xb4;
        //     custom->bltcon1 = 0;     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill
        //     custom->bltbdat = 0;
        //     custom->bltcdat = 0;
        //     // D mod will always be 0 because we're clearing a continuous block.
        //     custom->bltdmod = 0; //
        //     // "AND" masks; i guess convenient masks for manipulating source data. http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0019.html
        //     // this is pretty much for partial first & last words of the blit, in case you're blitting a non-word-aligned size. for full lines this would naturally always be 0xffff.
        //     custom->bltafwm = custom->bltalwm = 0xffff;

        //     // // copies A to D. D is the memory after A.
        //     // line 100
        //     custom->bltdpt = (APTR)(img.mBitplanePointers[0] + img.GetLineOffsetBytes(startLine)); //  (UBYTE *)image.mpData + (320 / 8) * 130 * 5; // "D" pointer. http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0020.html

        //     // // sets area size and invokes the blitter.
        //     // // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001D.html.
        //     // // bits 0-5 (6 bits) = width. 6-15 (10 bits) = width, both 1024 max in theory. width is in WORDs.
        //     // // unit is in pixels/lines.
        //     // // 56 lines high * 5 bit planes suggests indeed the interleaving, but not necessarily so. would be the same calculation either way.
        //     // // 320/8=40 bytes /2= 20 WORDS
        //     // custom->bltsize = ((56 * 5) << HSIZEBITS) | (320 / 16); // 56 lines
        //     // 7 lines
        //     custom->bltsize = ((sizeLines * img.GetStrideLines()) << HSIZEBITS) | (img.mWidthWords);

        //     // now for non-interleaved, you would need to repeat this for all bitplanes.
        // }

        // // TODO: support images of different size.
        // template <USHORT TwidthA, USHORT TheightA, USHORT TbitplanesA, ImageFlags TflagsA,
        //           USHORT TwidthD, USHORT TheightD, USHORT TbitplanesD, ImageFlags TflagsD>
        // static void BlitLines(const Image<TwidthA, TheightA, TbitplanesA, TflagsA> &src, const Image<TwidthD, TheightD, TbitplanesD, TflagsD> &dest, USHORT srcStartLine, USHORT destStartLine, USHORT sizeLines)
        // {
        //     WaitBlit();
        //     custom->bltcon0 = A_TO_D /*logic*/ | SRCA | DEST;
        //     custom->bltcon1 = 0; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     custom->bltbdat = 0;
        //     custom->bltcdat = 0;

        //     // set mods for all used DMA channels.
        //     // if the images are not the same size, then one of these will be non-zero.
        //     custom->bltdmod = 0; //
        //     custom->bltamod = 0;
        //     // first and last word masks. full line = no mask (all 1)
        //     custom->bltafwm = custom->bltalwm = 0xffff;

        //     custom->bltapt = (APTR)(src.mBitplanePointers[0] + src.GetLineOffsetBytes(srcStartLine));
        //     custom->bltdpt = (APTR)(dest.mBitplanePointers[0] + dest.GetLineOffsetBytes(destStartLine)); //  (UBYTE *)image.mpData + (320 / 8) * 130 * 5; // "D" pointer. http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0020.html

        //     // sets area size and invokes the blitter.
        //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001D.html.
        //     // // bits 0-5 (6 bits) = width. 6-15 (10 bits) = width, both 1024 max in theory. width is in WORDs.
        //     custom->bltsize = ((sizeLines * src.GetStrideLines()) << HSIZEBITS) | (src.mWidthWords);

        //     // now for non-interleaved, you would need to repeat this for all bitplanes.
        // }

        // // // assumes A = mask, B = source image, C = background, D = destination
        // // // assumes mask and source are same dimensions
        // // // assumes dest & background are same dimensions
        // // inline static void BlitMaskedRaw(uint16_t logicMask, uint8_t shift, uint16_t descendingModeFlag,
        // //                           const void *maskPtr, const void *srcPtr,
        // //                           uint16_t srcMod, uint16_t firstWordMask, uint16_t lastWordMask,
        // //                           const void *backgroundSourcePtr, const void *destPtr, uint16_t destMod,
        // //                           uint16_t blitWidthWords, uint16_t blitHeightLines
        // // )
        // // {
        // //     WaitBlit();

        // //     custom->bltcon0 = (logicMask) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
        // //     custom->bltcon1 = (shift << BSHIFTSHIFT) | descendingModeFlag; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill
        // //     custom->bltapt = ; // bitplane 0 offset
        // //     custom->bltbmod = custom->bltamod = srcMod;                         // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
        // //     custom->bltafwm = firstWordMask;
        // //     custom->bltalwm = lastWordMask;

        // //     // B = src
        // //     // custom->bltbpt = (APTR)( spritesWithMask.mNonmaskBitplanePointers[4][soffsetBytes]);
        // //     custom->bltbpt = (APTR)(spritesWithMask.mpData + soffsetBytes);

        // //     // C = background. let's put it at
        // //     size_t bgOffset = (40 * 5 * (150 + 64)) + (48 / 8 + 10); // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels) -- end offset +64x64 but x-=16 (thus 64-16=48) because we need LAST not END
        // //     // this is the "end". to make it "last", subtract a word.
        // //     // bgOffset -= 2;
        // //     custom->bltcpt = (APTR)(backgroundImage.mpData + bgOffset);
        // //     custom->bltdmod = custom->bltcmod = (320 - 64) / 8; // img width 320 - 32 = 2 (efficient to calc together with above)

        // //     APTR dpt = (APTR)(backgroundSurface.mpData + bgOffset);
        // //     custom->bltdpt = dpt;

        // //     custom->bltsize = (blitHeightLines << HSIZEBITS) | blitWidthWords; // 48 width because we capture 3 bits of the right one.
        // // }

        // // // effectively copies a sprite-like rect,
        // // // mask must be same dimensions as A, and have 1 bitplane only.
        // // // src and dest must have same bitplane count (otherwise math is less optimal, plus it's not clear what the point of that would be -- colors would be incompatible.)
        // // template <USHORT TwidthA, USHORT TheightA, USHORT TbitplanesA, ImageFlags TflagsA,
        // //           USHORT TwidthD, USHORT TheightD, ImageFlags TflagsD>
        // // static void BlitMaskedBob(const Image<TwidthA, TheightA, TbitplanesA, TflagsA> &src,
        // //                           const Image<TwidthA, TheightA, 1, TflagsA> &srcMask,
        // //                           const Image<TwidthD, TheightD, TbitplanesA, TflagsD> &dest,
        // //                           USHORT srcX, USHORT srcY, USHORT destX, USHORT destY, USHORT width, USHORT height)
        // // // int srcX, int srcY, int destX, int destY, int width, int height)
        // // {
        // //     WaitBlit();

        // //     // TODO: clamp values like width & height based on position & image sizes.

        // //     // 0xCA = 202 = 11001010 = 2 | 8| 64|128 = 2 | 8 | 0x40 | 0x80 = NANBC | NABC | ABNC | ABC.
        // //     // these logic flags are hard to interpret.
        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011C.html
        // //     // to me, we use B as a "pivot". when B(mask) is 0, then use the C(background) value directly.
        // //     // when B(mask) is 1, use A value directly.
        // //     // that's 11100010 = 2|32|64|128 = NANBC | ANBC | ABNC | ABC
        // //     // ER no A should be mask, in order to support first/last masks.
        // //     // 1100 1010
        // //     // A is mask so it can support first/last word masks.
        // //     custom->bltcon0 = (NANBC | ANBC | ABNC | ABC) | (SRCA | SRCB | SRCC | DEST); // A = mask, B = source, C = background, D = destination
        // //     custom->bltcon1 = 0;                                                         // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        // //     // set mods for all used DMA channels.
        // //     // if the images are not the same size, then one of these will be non-zero.
        // //     auto srcLineInfo = src.CalculatePixelOffsetInfo(srcX, srcY, width);
        // //     auto maskLineInfo = srcMask.CalculatePixelOffsetInfo(srcX, srcY, width);
        // //     auto destLineInfo = dest.CalculatePixelOffsetInfo(destX, destY, width);

        // //     DbgPrintF("DEST: beginByte:%d lenWords:%d moduloBytes:%d (srcLenWords:%d, maskWords:%d) - srcBeginByte:%d maskBeginByte:%d",
        // //               destLineInfo.beginOffsetFromBitplaneBytes, destLineInfo.lengthWords, destLineInfo.moduloBytes, srcLineInfo.lengthWords, maskLineInfo.lengthWords, srcLineInfo.beginOffsetFromBitplaneBytes, maskLineInfo.beginOffsetFromBitplaneBytes);
        // //     // calculating ptr & mod values:
        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html#line102

        // //     // this pointer probably needs to be aligned to a WORD.
        // //     custom->bltafwm = maskLineInfo.firstWordMask;
        // //     custom->bltalwm = maskLineInfo.lastWordMask;
        // //     custom->bltamod = maskLineInfo.moduloBytes; // in BYTES, though must be word-aligned.
        // //     custom->bltbmod = srcLineInfo.moduloBytes;  // in BYTES, though must be word-aligned.
        // //     custom->bltapt = (APTR)(srcMask.mBitplanePointers[0] + maskLineInfo.beginOffsetFromBitplaneBytes);
        // //     custom->bltbpt = (APTR)(src.mBitplanePointers[0] + srcLineInfo.beginOffsetFromBitplaneBytes);

        // //     custom->bltcpt = custom->bltdpt = (APTR)(dest.mBitplanePointers[0] + destLineInfo.beginOffsetFromBitplaneBytes);
        // //     custom->bltcmod = custom->bltdmod = destLineInfo.moduloBytes;

        // //     // sets area size and invokes the blitter.
        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001D.html.
        // //     // // bits 0-5 (6 bits) = width. 6-15 (10 bits) = width, both 1024 max in theory. width is in WORDs.
        // //     custom->bltsize = ((height * TbitplanesA) << HSIZEBITS) | srcLineInfo.lengthWords;

        // //     // USHORT lengthWords;
        // //     // USHORT firstWordMask;
        // //     // USHORT lastWordMask;
        // //     // USHORT moduloBytes; // bytes to skip to get to the next line.
        // //     // // offset from a bitplane start to the first word, but in pixels. used for blitter source pointer calc.
        // //     // USHORT beginOffsetFromBitplaneBytes; // srcMask.GetLineOffsetBytes(srcY) + srcLineInfo.lineOffsetWords * 2

        // //     // KPrintF("lenWords: %d",
        // //     // (int)srcLineInfo.lengthWords
        // //     // // (int)srcLineInfo.moduloBytes,
        // //     // //  (int)destLineInfo.moduloBytes
        // //     //  );

        // //     // debug_clear();
        // //     //  debug_filled_rect(40, 40, 140, 60, 0x00004444); // 0x00RRGGBB
        // //     //  debug_rect(destX * 2, destY * 2, (destX + width) * 2, (destY + height) * 2, 0xff8800);

        // //     //  debug_text(40, 40, "This is a WinUAE debug overlay", 0x00ff8800);
        // // }

        // // // effectively copies a sprite-like rect,
        // // // mask must be same dimensions as A, and have 1 bitplane only.
        // // // src and dest must have same bitplane count (otherwise math is less optimal, plus it's not clear what the point of that would be -- colors would be incompatible.)
        // // template <USHORT TwidthA, USHORT TheightA, USHORT TbitplanesA, ImageFlags TflagsA,
        // //           USHORT TwidthD, USHORT TheightD, ImageFlags TflagsD>
        // // static void BlitBob(const Image<TwidthA, TheightA, TbitplanesA, TflagsA> &src,
        // //                     const Image<TwidthD, TheightD, TbitplanesA, TflagsD> &dest,
        // //                     USHORT srcX, USHORT srcY, USHORT destX, USHORT destY, USHORT width, USHORT height)
        // // {
        // //     WaitBlit();

        // //     // set mods for all used DMA channels.
        // //     // if the images are not the same size, then one of these will be non-zero.
        // //     // auto srcLineInfo = src.CalculatePixelOffsetInfo(srcX, srcY, width);
        // //     // auto destLineInfo = dest.CalculatePixelOffsetInfo(destX, destY, width);
        // //     auto calc = sCalculateBlit(src, dest, srcX, srcY, destX, destY, width, height);
        // //     calc.Dump();

        // //     // srcLineInfo.Dump("srcLineInfo");
        // //     // destLineInfo.Dump("destLineInfo");

        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011C.html
        // //     //custom->bltcon0 = (A_TO_D) | (SRCA | DEST) | (calc.xShift << ASHIFTSHIFT); // A = src
        // //     custom->bltcon0 = (A_TO_D) | (SRCA | DEST) ;
        // //     // FOR USE WITH B CHANNEL: custom->bltcon1 = (calc.shift << BSHIFTSHIFT);                        // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill
        // //     custom->bltcon1 = 0; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill
        // //     custom->bltbdat = 0;
        // //     custom->bltcdat = 0;

        // //     // there's a problem when the src & dest are not using the same width words. for example when
        // //     // blitting at a weird src offset X, the resulting info will request you blit a bunch of unnecessary (masked out)
        // //     // bits to the left or right. Those must be taken into account for the destination.
        // //     // you cannot actually blit arbitrary data into arbitrary destination. everything is word-aligned
        // //     // this only affects X position, so take some examples of X registers.
        // //     // src from 0 width 18, to dest 0 = src offset word 0, 2 word width (17 width touches 2 words), dest offset word 0
        // //     // src from 1 width 18, to dest 0 = src offset word 0 (shift -1), 2 word width, dest offset word 0
        // //     // src from 15 width 18, to dest 0 = src offset word 0 (shift -15), 3 word width, dest offset word 0
        // //     // src from 15 width 18, to dest 14 = src offset word 0 (shift -15), 3 word width (15+18=33 end which touches 3rd word), dest offset word 0 (shift +14) -- total shift +1
        // //     // src from 0 width 16, to dest 1 = src offset word 0, 1 word width. dest offset word 0 (shift +1)
        // //     // src from 0 width 16, to dest 17 = src offset word 0, 1 word width. dest offset word 1 (shift +1)

        // //     // but then, the issue is shifts are only positive.
        // //     // src from 15 width 18, to dest 33 = src offset word 0 (shift -15), 3 word width, dest offset word 2 (shift +1) -- total shift -14
        // //     // since we don't support negative shifts, we must do something else.
        // //     //
        // //     // SRC
        // //     //  word0               word1               word2               word3
        // //     // |-------------------|-------------------|-------------------|-------------------
        // //     //                    XXXXXXXXXXXXXXXXXXXXXXXX                   <-- the part we want to copy.
        // //     // ...................XXXXXXXXXXXXXXXXXXXXXXXX.................. <-- the stuff we must copy
        // //     //
        // //     // DEST
        // //     //  word0               word1               word2               word3
        // //     // |-------------------|-------------------|-------------------|-------------------
        // //     //                                           ^ desired position
        // //     //                        ...................XXXXXXXXXXXXXXXXXXXXXXXX..................
        // //     //                                         ...................XXXXXXXXXXXXXXXXXXXXXXXX..................  <-- where we have calculated to blit
        // //     // hence shift of -14.
        // //     // but that won't work because of negative shift. so adjust.
        // //     // dest offset word -- = 1.
        // //     // shift += 16 = 2
        // //     // DEST
        // //     //  word0               word1               word2               word3
        // //     // |-------------------|-------------------|-------------------|-------------------
        // //     //                                           ^ desired position
        // //     //                     ...................XXXXXXXXXXXXXXXXXXXXXXXX..................  <-- newly calculated blit
        // //     //                        ...................XXXXXXXXXXXXXXXXXXXXXXXX..................  <-- and apply shift +2
        // //     //
        // //     // now there's a new problem if you try to blit to the left edge of the screen where we can't subtract a word.
        // //     //
        // //     // SRC
        // //     //  word0               word1               word2               word3
        // //     // |-------------------|-------------------|-------------------|-------------------
        // //     //                    XXXXXXXXXXXXXXXXXXXXXXXX                   <-- the part we want to copy.
        // //     // ...................XXXXXXXXXXXXXXXXXXXXXXXX.................. <-- the stuff we must copy
        // //     //
        // //     // let's make dest = 2 (word0), shift -14
        // //     // DEST
        // //     //                   word0               word1               word2               word3
        // //     //                  |-------------------|-------------------|-------------------|-------------------
        // //     //                    ^ desired position
        // //     // ...................XXXXXXXXXXXXXXXXXXXXXXXX..................
        // //     // in this case there's not much we can do. we are pretty much forced to do srcword+1, which would discard a few pixels.
        // //     // the solution is described:
        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0121.html
        // //     // the answer is descending mode, which reverses the direction of everything in the blit.
        // //     //

        // //     // it's clear that we should ALWAYS use source calculated width.

        // //     // shifts are required for moving the blit into the correct sub-word position. A is in bltcon0, B is in bltcon1

        // //     // calculating ptr & mod values:
        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html#line102
        // //     // this pointer probably needs to be aligned to a WORD.
        // //     custom->bltafwm = 0xffff;// calc.srcFirstWordMask;
        // //     custom->bltalwm =0xffff;// calc.srcLastWordMask;
        // //     custom->bltamod = calc.srcModuloBytes;  // in BYTES, though must be word-aligned.
        // //     custom->bltdmod = calc.destModuloBytes; // in BYTES, though must be word-aligned.
        // //     custom->bltapt = (APTR)(src.mBitplanePointers[0] + calc.srcBeginOffsetFromBitplaneBytes);
        // //     custom->bltdpt = (APTR)(dest.mBitplanePointers[0] + calc.destBeginOffsetFromBitplaneBytes);

        // //     // sets area size and invokes the blitter.
        // //     // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001D.html.
        // //     // // bits 0-5 (6 bits) = width. 6-15 (10 bits) = width, both 1024 max in theory. width is in WORDs.
        // //     // DbgPrintF("h:%d len:%d", height * TbitplanesA, srcLineInfo.lengthWords);
        // //     custom->bltsize = ((height * TbitplanesA) << HSIZEBITS) | calc.widthWords;
        // // }

        // // effectively copies a sprite-like rect. src is a masked sprite, interleaved.
        // // src and dest must have same non-masked bitplane count, which makes sense anyway due to color compat
        // // all calculations are greatly simplified if source X coordinate is word-aligned.
        // template <USHORT TwidthA, USHORT TheightA, USHORT TbitplanesA, ImageFlags TflagsA,
        //           USHORT TwidthD, USHORT TheightD, ImageFlags TflagsD>
        // static void BlitMaskedBobWithAlignedSource(const Image<TwidthA, TheightA, TbitplanesA, TflagsA> &srcMasked,
        //                                            const Image<TwidthD, TheightD, TbitplanesA, TflagsD> &background,
        //                                            const Image<TwidthD, TheightD, TbitplanesA, TflagsD> &dest,
        //                                            int16_t srcX, int16_t srcY, int16_t destX, int16_t destY, int16_t width, int16_t height)
        // {

        //     // TODO: pre-process bounds and discard NOPs.
        //     // if something is OOB, adjust other params to compensate.

        //     WaitBlit();

        //     // because blits are on word-aligned bounds, any shifting not a multiple of 16 must be masked out and/or shifted, and in the worst case, use descending mode.
        //     // all of this depends on the shift vals, so start by calculating that.

        //     int srcShift = srcX & 15;   // requesting a chunk suggests it would get left-shifted on the dest after masking.
        //     int destShift = destX & 15; // this would right-shift (positive shift)
        //     int finalShift = destShift - srcShift;
        //     if (finalShift < 0)
        //     {
        //         // TODO: implement descending mode.
        //         return;
        //     }
        //     // final shift is now always range [0,15]

        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    abcdefghijklmnop ppqrstuvwxyz0123 456789xABCDEFGHI                  src image
        //     // SRC:    abcdefghijklmnop ppqrstuvwxyz0...                                   srcX = 0, width 28 means masking out (.).
        //     // DEST:                   |>>>>>>>>>>>>abcd efghijklmnoppppq rstuvwxyz0       destX = 27 will begin blitting here, and shift right 12.
        //     // the true blit must be 3 words, and the

        //     //                  |>>>>>>>>>...defg|hijklmnoppppqrstu|vwxyz0.........|
        //     // now generate the masks. the way shifting & masking works is that the source is masked, then the "shift" rotates the result.
        //     // i am concerned there's a problem when i have to widen the blit. we can't mask more than a word, so ...
        //     // so let's walk through that from start to finish.
        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    abcdefghijklmnop ppqrstuvwxyz0123 456789xABCDEFGHI                  src image
        //     // SRC:    ...defghijklmnop ppqrstuvwxyz0... ................                  theoretical mask when blitting 3 words. but that's not possible so ...
        //     // SRC:    ...defghijklmnop ppqrstuvwxyz0XXX ................                  best mask we can manage. XXX is what we would consider garbage, impossible to mask out.
        //     // DEST:                   |...defghijklmnop ppqrstuvwxyz0XXX ................ rotate 9
        //     // DEST:                   |............defg hijklmnopppqrstu vwxyz0XXX....... rotate 9. yea we can't do this.

        //     // all this actually gets SO much easier if we only allow WORD-aligned source values.

        //     // how can i just use http://amigadev.elowar.com/read/ADCD_2.1/Includes_and_Autodocs_3._guide/node02B5.html

        //     // all that description and it's just a subtraction really to figure out the 3 shift values
        //     // calc total blit width. that's both shifts as illustrated above, plus requested blit width, ceil.
        //     int srcBeginWord = srcX / 16;
        //     int destBeginWord = destX / 16;
        //     int destEndWord = (destX + srcShift + destShift + 15) / 16;
        //     int actualBlitWidthWords = destEndWord - srcBeginWord;

        //     // WHAT HAPPENS if we put a restriction on the source. Basically:
        //     // - only supporting word-aligned sprites, avoiding weird masking scenarios (see case 2)

        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    ---------------- abcdefghijklmnop ppqrstuvwxyz0123 ----------------                  src image, sprite between word1 and word2, with 1 word of space on either side.

        //     // CASE 1 (simple, right shift)
        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    ---------------- abcdefghijklmnop ppqrstuvwxyz0123 ----------------                  src image, sprite between word1 and word2, with 1 word of space on either side.
        //     // DEST:                   |>>>>>abcdefghijk lmnopppqrstuvwxy z0123           intended destination image. shift right 5 into place.
        //     // SRC:                    |abcdefghijklmnop ppqrstuvwxyz0123 ----------------| src blit area with mask. ('-' = mask)
        //     // SRC:                    |-----abcdefghijk lmnopppqrstuvwxy z0123-----------| src blit area with mask, shifted.
        //     // can see here how if src was like 30 bits wide (non-word-aligned), the mask would be impossible. see:
        //     // DEST:                   |>>>>>abcdefghijk lmnopppqrstuvwxy z01             intended destination image. shift right 5 into place, so it touches 3 words.
        //     // SRC:                    |abcdefghijklmnop ppqrstuvwxyz01-- ----------------| src blit area with mask. mask requires more than 1 word so impossible.

        //     // CASE 2: clipping on left (requires descending mode)
        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    ---------------- abcdefghijklmnop ppqrstuvwxyz0123 ----------------                  src image, sprite between word1 and word2, with 1 word of space on either side.
        //     // DEST:  |ijklmnopppqrs tuvwxyz0123--------|    if you want to clip off the left edge of the screen, it's a similar issue to before.
        //     // in fact, preprocessing can potentially make the src rect non-word-aligned. but maybe we can assume it will still not put us into vexing scenarios.
        //     // we have no choice but to shift LEFT to make this one happen.
        //     // SRC:                             ijklmnop ppqrstuvwxyz0123       desired src rect.
        //     // so actually if we're forced to shift left, in descending mode, then maybe allowing non-word-aligned src rects is OK.
        //     // HOWEVER, practically speaking if we force empty space, what is even the point of allowing that? adds no value. oh well masking is still an issue in that case. see:
        //     // so we're forced to blit the following src:
        //     // SRC:                    |abcdefghijklmnop ppqrstuvwxyz0123|
        //     // DEST:  |xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx|                    <-- blit destination.
        //     // note that because the src rect is word-aligned, we don't need to expand the area, ever. this allows masking out the rotated bits.
        //     // SRC:                    |--------ijklmnop ppqrstuvwxyz0123|   <-- masked
        //     // SRC:                    |ijklmnopppqrstuv wxyz0123--------|   <-- shifted/rotated (mask is needed)
        //     // DEST:  |ijklmnopppqrstuv wxyz0123--------|   <-- and blitted on dest.
        //     //
        //     // are we certain that we never need to expand the src then? i can't see any scenario where it's required.
        //     // case 1 does require expansion, because the resulting VISIBLE dest area touches 3 words. here the word boundary for expansion is ALWAYS the invisible cutoff.

        //     // better double check with right clipping then too, right? it should follow teh same principle as it's right-shifting and clipping.
        //     //         word0            word1            word2            word3
        //     //        |----------------|----------------|----------------|----------------
        //     // SRC:    abcdefghijklmnop qrstuvwxyz012345 6789ABCDEFGHIJKL MNOPQRSTUVWXYZZZ     src image
        //     //                         |qrstuvwxyz012345 6789ABCDEFGHIJKL|                     src rgn
        //     // DEST:                                    |qrstuvwxyz012345 6789ABCDEFGHIJKL|    desired result -- CASE 1, everything word aligned. shift = 0, no expansion, no mask.
        //     // DEST:                   |   qrstuvwxyz012 3456789ABCDEFGHI JKL             |    desired result -- CASE 2, everything word aligned. right shift = 3. expansion = 1 word.
        //     // SRC                     |qrstuvwxyz012345 6789ABCDEFGHIJKL ----------------|     (case 2 src expanded rgn and mask)
        //     // DEST:  |            qrst uvwxyz0123456789 ABCDEFGHIJKL    |                     desired result -- CASE 2, everything word aligned. right shift = 12. expansion = 1 word, exactly the same mask.
        //     // DEST:  |vwxyz0123456789A BCDEFGHIJKL     |                                      desired result -- CASE 3: clip left edge. descending mode required. no expansion because the shifted region is not visible.
        //     // SRC                     |-----vwxyz012345 6789ABCDEFGHIJKL|                      (case 3 src rgn and mask)
        //     // DEST:                                    |     qrstuvwxyz0 123456789ABCDEFG|    desired result -- CASE 4: clip right edge. ascending mode required. no expansion because the shifted region is not visible.
        //     // SRC:                    |qrstuvwxyz012345 6789ABCDEFG-----|                      (case 3 src rgn and mask)

        //     // so this all works pretty well when source region is word-aligned, and the dest SIZE is word-aligned.

        //     // same masked area, but different dest blit position.

        //     // constexpr auto logicBmaskA = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
        //     // constexpr auto logicB = ABC | ABNC | NABC | NABNC;      // just use B. so all the [B]
        //     // constexpr auto logicA = ABC | ABNC | ANBC | ANBNC;      // just use A. so all the [A]
        //     // int16_t shift = f & 15;
        //     // custom->bltcon0 = (logicBmaskA) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
        //     // custom->bltcon1 = (shift << BSHIFTSHIFT) | BLITREVERSE; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill

        //     // // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011C.html
        //     // //custom->bltcon0 = (A_TO_D) | (SRCA | DEST) | (calc.xShift << ASHIFTSHIFT); // A = src
        //     // custom->bltcon0 = (A_TO_D) | (SRCA | DEST) ;
        //     // // FOR USE WITH B CHANNEL: custom->bltcon1 = (calc.shift << BSHIFTSHIFT);                        // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill
        //     // custom->bltcon1 = 0; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // no shift, area mode, no fill
        //     // custom->bltbdat = 0;
        //     // custom->bltcdat = 0;

        //     // custom->bltafwm = 0xffff;// calc.srcFirstWordMask;
        //     // custom->bltalwm =0xffff;// calc.srcLastWordMask;
        //     // custom->bltamod = calc.srcModuloBytes;  // in BYTES, though must be word-aligned.
        //     // custom->bltdmod = calc.destModuloBytes; // in BYTES, though must be word-aligned.
        //     // custom->bltapt = (APTR)(src.mBitplanePointers[0] + calc.srcBeginOffsetFromBitplaneBytes);
        //     // custom->bltdpt = (APTR)(dest.mBitplanePointers[0] + calc.destBeginOffsetFromBitplaneBytes);

        //     // // sets area size and invokes the blitter.
        //     // // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001D.html.
        //     // // // bits 0-5 (6 bits) = width. 6-15 (10 bits) = width, both 1024 max in theory. width is in WORDs.
        //     // // DbgPrintF("h:%d len:%d", height * TbitplanesA, srcLineInfo.lengthWords);
        //     // custom->bltsize = ((height * TbitplanesA) << HSIZEBITS) | calc.widthWords;
        // }



        // i read that memcpy is faster than blitter. it's absolutely not the case.
