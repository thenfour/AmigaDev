// TODO: font with color. basically use the font as a mask, and color as some kind of constant.

#pragma once

#include "support/gcc8_c_support.h"
#include <exec/types.h>
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
#include <libraries/dos.h>
#include "base.hpp"

extern volatile Custom *custom;

namespace cc
{

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    enum class ImageFlags : UBYTE
    {
        None = 0,
        Interleaved = debug_resource_bitmap_interleaved,
        Masked = debug_resource_bitmap_masked,
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // struct Size
    // {
    //     const SHORT mWidth;
    //     const SHORT mHeight;
    //     Size(SHORT width, SHORT height) : mWidth(width), mHeight(height) {}
    // };

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    template <USHORT TentryCount>
    struct Palette
    {
        USHORT *mpData;
        const bool mOwnData = false;
        static constexpr USHORT mEntryCount = TentryCount;
        // when using KingCon, the output palette format is RAW,
        // and in WORDS. so the top nybble of each value is 0.
        // e.g. 2 colors $0456 and $abc are stored as [04 56 0a bc]
        explicit Palette(const void *existingData = nullptr) : //
                                                               mpData(existingData ? (USHORT *)existingData : (USHORT *)AllocMem(TentryCount * sizeof(USHORT), MEMF_ANY)),
                                                               mOwnData(!existingData)
        {
            debug_register_palette(mpData, "image.pal", TentryCount, 0 /* there are no flags for palettes */);
        }
        ~Palette()
        {
            if (mOwnData)
            {
                FreeMem(mpData, mEntryCount * sizeof(USHORT));
            }
        }
        USHORT operator[](USHORT i)
        {
            return mpData[i];
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // actually due to simplicity, i'm only supporting interleaved format.
    template <USHORT Twidth, USHORT Theight, USHORT TnonMaskBitplanes, ImageFlags Tflags = ImageFlags ::None>
    struct Image
    {
        struct DirtyRect
        {
            static constexpr USHORT mWidth = Twidth;
            static constexpr USHORT mHeight = Theight;

            bool mIsDirty = false;
            uint16_t mDirtyRectStartX = 0;
            uint16_t mDirtyRectStartY = 0;
            uint16_t mDirtyRectEndX = 0;// even though i prefer using width/height because it's semantically clearer, this is more efficient calculation.
            uint16_t mDirtyRectEndY = 0;

            uint16_t GetDirtyRectStartXPixel() const
            {
                return mDirtyRectStartX & 0xfff0;
            }
            uint16_t GetDirtyRectWidthPixels() const
            {
                // desired end
                const uint16_t sizeAccountingForStartRounding = mDirtyRectEndX - GetDirtyRectStartXPixel();
                return (sizeAccountingForStartRounding + 15) & 0xfff0;
            }
            uint16_t GetDirtyRectStartYLine() const
            {
                return mDirtyRectStartY;
            }
            uint16_t GetDirtyRectHeightLines() const
            {
                return mDirtyRectEndY - mDirtyRectStartY;
            }

            void MarkAsClean()
            {
                mIsDirty = false;
            }

            void MarkDirtyRect(uint16_t x, uint16_t y, uint16_t width, uint16_t height)
            {
                if (!mIsDirty)
                {
                    // set the rect.
                    mDirtyRectStartX = x;
                    mDirtyRectStartY = y;
                    mDirtyRectEndX = x + width;
                    mDirtyRectEndY = y + height;
                    mIsDirty = true;
                    return;
                }
                // expand rect.
                mDirtyRectStartX = std::min(mDirtyRectStartX, x);
                mDirtyRectStartY = std::min(mDirtyRectStartY, y);
                mDirtyRectEndX = std::max(mDirtyRectEndX, (uint16_t)(x + width));
                mDirtyRectEndY = std::max(mDirtyRectEndY, (uint16_t)(y + height));
            }

            void MarkDirtyRect(const DirtyRect& rhs)
            {
                if (!mIsDirty)
                {
                    *this = rhs;
                    return;
                }
                // expand rect.
                mDirtyRectStartX = std::min(mDirtyRectStartX, rhs.mDirtyRectStartX);
                mDirtyRectStartY = std::min(mDirtyRectStartY, rhs.mDirtyRectStartY);
                mDirtyRectEndX = std::max(mDirtyRectEndX, rhs.mDirtyRectEndX);
                mDirtyRectEndY = std::max(mDirtyRectEndY, rhs.mDirtyRectEndY);
            }

            void MarkWholeImageAsDirty()
            {
                mIsDirty = true;
                mDirtyRectStartX = 0;
                mDirtyRectStartY = 0;
                mDirtyRectEndX = mWidth;
                mDirtyRectEndY = mHeight;
            }
        };

        static_assert((Twidth & 0xf) == 0, "image width must always be word-aligned. otherwise many calculations regarding memory layout are impractical due to hardware limitations");
        using ThisT = Image<Twidth, Theight, TnonMaskBitplanes, Tflags>;
        static constexpr USHORT mWidth = Twidth;
        static constexpr USHORT mHeight = Theight;
        static constexpr USHORT mWidthWords = (Twidth + 15) / 16;
        static constexpr ImageFlags mFlags = Tflags;
        static constexpr USHORT mNonmaskBitplaneCount = TnonMaskBitplanes;
        static constexpr bool mIsInterleaved = is_flag_set(Tflags, ImageFlags::Interleaved);
        static constexpr bool mHasMask = is_flag_set(Tflags, ImageFlags::Masked);
        static constexpr USHORT mMaskBitplaneCount = mHasMask ? (mIsInterleaved ? TnonMaskBitplanes : 1) : 0;
        static constexpr USHORT mAllBitplanesCount = mMaskBitplaneCount + mNonmaskBitplaneCount;
        const uint8_t *mpData;
        const uint8_t *mpMask0 = nullptr;
        const bool mOwnPointer; // should this object free the pointer?

        const UBYTE *mNonmaskBitplanePointers[mNonmaskBitplaneCount];
        const UBYTE *mMaskBitplanePointers[mMaskBitplaneCount == 0 ? 1 : mMaskBitplaneCount]; // 1 element for safety.

        DirtyRect mDirtyRect;

        APTR AtByteOffset(size_t bytes)
        {
            return (APTR)(mpData + bytes);
        }

        APTR AtMaskByteOffset(size_t bytes)
        {
            return (APTR)(mpMask0 + bytes);
        }

        // width in bytes of a line. for 320-wide, it's 40 (8 pixels per byte = 320 / 8 = 40)
        static constexpr USHORT GetLineSizeBytes()
        {
            return (Twidth + 7) / 8;
        }

        static constexpr int GetImageSizeBytes()
        {
            return GetLineSizeBytes() * mAllBitplanesCount * mHeight;
        }

        // returns the # of memory lines per display line
        // TODO: I hate the name of this function.
        static constexpr USHORT GetStrideLines()
        {
            return (mIsInterleaved) ? mAllBitplanesCount : 1;
        }

        // returns the "modulo" which the blitter / display uses to skip bytes after processing each line.
        // for a full-screen non-interleaved image this is 0 because the first pixel of the next line is directly after the last pixel of the current.
        // for interleaved, the first pixel of the next line must skip the interleaved bitplane lines.
        static constexpr USHORT sGetWholeLineModuloBytes()
        {
            return (GetStrideLines() - 1) * GetLineSizeBytes();
        }
        constexpr USHORT GetWholeLineModuloBytes() const
        {
            return sGetWholeLineModuloBytes();
        }

        // the modulo is how many bytes to skip to reach the same position in the next line.
        // with interleaving & masking, need to account for skipping across bitplanes & mask.
        // when you blit interleaved, you don't skip bitplanes.
        uint16_t CalculateModuloBytes(int16_t blitWidthWords) const
        {
            if (mHasMask)
            {
                // double line size bytes to accommodate interleaved mask
                // and double width words to convert to bytes.
                // 2A - 2B is then rewritten (A-B)*2 to save an op.
                return (GetLineSizeBytes() - blitWidthWords) << 1;
            }
            // double width words to convert to bytes.
            return GetLineSizeBytes() - (blitWidthWords << 1);
        }

        // returns the bytes between bitplanes. IOW, if you have a location in bitplane0, you can add this to get the same location in bitplane1.
        static constexpr int GetBitplaneStrideBytes()
        {
            // for interleaved, each bitplane is 1 line apart.
            // for non-interleaved, each bitplane is a whole image apart.
            if (mIsInterleaved)
            {
                if (mHasMask)
                {
                    return GetLineSizeBytes() * 2; // each bitplane has a mask line to skip.
                }
                return GetLineSizeBytes(); // each bitplane right after each other.
            }
            return GetLineSizeBytes() * mHeight; // each bitplane a whole image apart.
        }

        // returns offset from a bitplane pointer. so for bitplane 0 location you'd do mBitplanePointers[b][ret] = x
        static constexpr USHORT GetLineOffsetBytes(USHORT line)
        {
            return GetLineSizeBytes() * GetStrideLines() * line;
        }

        explicit Image(const void *existingData, bool takeOwnership) : //
                                                                       mpData(!!existingData ? (uint8_t *)existingData : (uint8_t *)AllocMem(GetImageSizeBytes(), MEMF_CHIP)),
                                                                       mOwnPointer(takeOwnership)
        {
            debug_register_bitmap(mpData, "image.bpl", mWidth, mHeight, mNonmaskBitplaneCount, (debug_resource_flags)Tflags);

            for (int a = 0; a < mNonmaskBitplaneCount; a++)
            {
                mNonmaskBitplanePointers[a] = ((UBYTE *)mpData) + GetBitplaneStrideBytes() * a;
            }

            if (mHasMask)
            {
                // calc mask plane 0 location
                if (mIsInterleaved)
                {
                    // interleaved means the mask is directly after each bitplane.
                    mpMask0 = mpData + GetLineSizeBytes();
                }
                else
                {
                    // mask is at the end of the image.
                    mpMask0 = mpData + GetLineSizeBytes() * mHeight * mNonmaskBitplaneCount;
                }
                for (int a = 0; a < mMaskBitplaneCount; a++)
                {
                    mMaskBitplanePointers[a] = mpMask0 + GetBitplaneStrideBytes() * a;
                }
            }
        }

        ~Image()
        {
            if (mOwnPointer)
            {
                FreeMem((APTR)mpData, GetImageSizeBytes());
            }
        }

        ThisT MakeCopy() const
        {
            // duplicate, into chip memory.
            void *p = AllocMem(GetImageSizeBytes(), MEMF_CHIP);
            memcpy(p, this->mpData, GetImageSizeBytes());
            return ThisT{p, true};
        }

        // returns the pointer offset, in bytes, of a (x,y) location, where X is word-aligned.
        // the offset can be applied to any bitplane to reach that position in that bitplane. most of the time you'll apply this to bitplane0 or mask0.
        constexpr size_t GetOffsetOfXYInBitplane(int x, int y) const
        {
            // even though theoretically byte-alignment (not word) would work, it's absolutely never used.
            ASSERT((x & 0xf) == 0, "X locations must be word-aligned.");
            return GetLineOffsetBytes(y) + (x / 8);
        }

        // for descending mode, you sometimes need a pointer to the LAST word of a region.
        // with how interleaving works, it means returning the pointer to the LAST bitplane.
        constexpr size_t GetLastOffsetOfRectInBitplane(int x, int y, int width, int height) const
        {
            // even though theoretically byte-alignment (not word) would work, it's absolutely never used.
            ASSERT((x & 0xf) == 0, "X locations must be word-aligned.");
            ASSERT((width & 0xf) == 0, "Widths must be word-aligned.");
            ASSERT(width >= 16, "Widths must be non-zero.");
            ASSERT(height >= 1, "Height must be non-zero.");

            // get pointer to END of line, and back it off by a word.
            // so when x = 0, width = 16, (0 + 16) / 8 = 2 - 2 = 0;
            //    when x = 8, width = 16, (8 + 16) / 8 = 3 - 2 = 1)
            // etc.
            return GetLineOffsetBytes(y + height - 1) + ((x + width) / 8) - 2;
        }
    };

    // helper class so functions only need 1 template parameter per image.
    template <typename T>
    using ImageType = Image<T::Twidth, T::Theight, T::TnonMaskBitplanes, T::Tflags>;

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    struct Blitter
    {

        // constraints in using this function:
        // - (checked in Image) all image dimension WIDTHs are word-aligned.
        // - (checked in static_assert) source X position is word-aligned
        // - (checked in static_assert) blit width is word-aligned
        // - (checked in static_assert) blit size must not be bigger than the destination (prevent having to handle clipping on multiple edges)
        // - (checked in static_assert) source image is interleaved with interleaved mask (L0:BMBM L1:BMBM format)
        // - (checked in template constraint) all images involved are the same bitplane count
        // - (checked in template constraint) background & destination are the same size & format
        //
        // These are pretty reasonable constraints but i could imagine the need for truly arbitrary sized blits / source rects, in the case you need
        // to do fancy clipping or other interactions. but there should always be workarounds.
        //
        // returns true if the blit was performed, false if it was a NOP.
        //
        // example call:
        // cc::Blitter::BlitMasked<0, 0, 32, 32>(spritesWithMask, backgroundImage, backgroundSurface, 100, 100);
        // blits (0,0) size (32,32) to destination location (100, 100).
        template <int TsrcX, int TsrcY, int Twidth, int Theight,
                  USHORT TwidthSrc, USHORT TheightSrc, USHORT TnonMaskBitplanes, ImageFlags TflagsSrc,
                  USHORT TwidthDest, USHORT TheightDest, ImageFlags TflagsDest>
        static bool BlitMasked(const Image<TwidthSrc, TheightSrc, TnonMaskBitplanes, TflagsSrc> &src,
                               const Image<TwidthDest, TheightDest, TnonMaskBitplanes, TflagsDest> &background,
                               Image<TwidthDest, TheightDest, TnonMaskBitplanes, TflagsDest> &destination,
                               int16_t destX, int16_t destY)
        {
            static_assert((TsrcX & 0xf) == 0, "blit source X must be word-aligned due to hardware limitations");
            static_assert((Twidth & 0xf) == 0, "blit width must be word-aligned due to hardware limitations");
            static_assert(is_flag_set(TflagsSrc, CombineFlags(ImageFlags::Interleaved, ImageFlags::Masked)), "this function designed only for interleaved masked sources");

            static_assert(Twidth <= TwidthDest, "blit too wide");
            static_assert(Theight <= TheightDest, "blit too tall");

            static_assert(TsrcY >= 0, "blit source rect OOB (top)");
            static_assert(TsrcX >= 0, "blit source rect OOB (left)");
            static_assert(TsrcX + Twidth <= TwidthSrc, "blit source rect OOB (right)");
            static_assert(TsrcY + Theight <= TheightSrc, "blit source rect OOB (top)");

            // NOP: no area to blit.
            if (Twidth <= 0)
                return false;
            if (Theight <= 0)
                return false;

            // because Y coords are not aligned, preprocess them to avoid having to handle a bunch of cases.
            int16_t height = Theight;
            int16_t srcY = TsrcY;
            // doing the arith this way puts constexpr operands together so it's just a simple compare.
            if (destY >= destination.mHeight - Theight) // check if clipping off bottom of destination
            {
                height -= destY + (Theight - destination.mHeight);
                if (height <= 0)
                    return false; // re-check 0 size
            }
            if (destY < 0) // check if clipping off TOP of destination
            {
                height += destY; // destY is negative remember.
                srcY -= destY;
                destY = 0;
                if (height <= 0)
                    return false; // re-check 0 size
            }

            // at this point no Y clipping is present, and area is nonzero.

            // A = mask, B = src, C = background
            constexpr auto logic = NABC | NANBC | ABNC | ABC; // B masked by A. all not-a-[anyb]-c + A-B-[anyc]
            // constexpr auto logic = ABC | ABNC | NABC | NABNC; // just use B. so all the [B]
            // constexpr auto logic = ABC | ABNC | ANBC | ANBNC; // just use A. so all the [A]

            // we could try to handle all cases in a generic way, but it's ugly to mix this logic with hardware logic.
            // instead handle cases specifically.
            // case 1: clip left (masking, descending, shift left) -- criteria: dest x pos < 0
            // case 2: clip right (masking, ascending, shift right) -- criteria: dest end > dest width
            // case 3: no clipping (no masking, ascending, shift right) -- criteria: else.
            if (destX < 0)
            {
                // case 1: clip left (masking, descending, shift left) -- criteria: dest x pos < 0
                // check NOP due to X
                if (destX <= -Twidth) // (destX + Twidth <= 0)
                    return false;     // NOP (off screen to the left)

                //        offscreen....                     word0            word1            word2            word3
                //        ################|################|----------------|----------------|----------------|----------------|
                // SRC:                                    |abcdefghijklmnop qrstuvwxyz012345|                      src image slice
                // DEST:          |abcdefg hijklmnopqrstuvw xyz012345
                // DEST:                                   |xyz012345<<<<<<<|      actual blit will be this on dest
                // SRC:                                                     |qrstuvwxyz012345|       with source rgn
                // SRC:                                                     |-------#########|       with source mask

                int16_t clippedXWords = destX / -16; // make positive
                const int16_t rightShift = destX & 0xf;
                const int16_t shift = (16 - rightShift) & 0xf; // as we're shifting left instead of right
                int16_t srcX = TsrcX + clippedXWords * 16;
                int16_t widthWords = (Twidth / 16) - clippedXWords;
                ASSERT(widthWords > 0, "width is zero? we should detect this condition earlier.");
                const size_t srcOffset = src.GetLastOffsetOfRectInBitplane(srcX, srcY, widthWords * 16, height);
                const size_t destOffset = destination.GetLastOffsetOfRectInBitplane(0, destY, widthWords * 16, height);

                WaitBlit();

                custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
                custom->bltcon1 = (shift << BSHIFTSHIFT) | BLITREVERSE; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // shift, area mode, no fill, ascending

                // B & A = mask & src
                custom->bltapt = (APTR)(src.mMaskBitplanePointers[0] + srcOffset);
                custom->bltbpt = (APTR)(src.mNonmaskBitplanePointers[0] + srcOffset);
                custom->bltbmod = custom->bltamod = src.CalculateModuloBytes(widthWords); // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
                custom->bltafwm = 0xffff;
                custom->bltalwm = (rightShift == 0) ? 0xffff : setRightmostBits(rightShift);

                // C & D have a lot of similarities due to same sizes.
                custom->bltdmod = custom->bltcmod = destination.CalculateModuloBytes(widthWords); // D and C modulos will always be the same due to same dimensions. img width 320 - 32 = 2 (efficient to calc together with above)
                custom->bltcpt = (APTR)(background.mNonmaskBitplanePointers[0] + destOffset);
                custom->bltdpt = (APTR)(destination.mNonmaskBitplanePointers[0] + destOffset);

                custom->bltsize = ((height * TnonMaskBitplanes) << HSIZEBITS) | widthWords;
                destination.mDirtyRect.MarkDirtyRect(0, destY, widthWords * 16, height);
            }                                             // case 2
            else if (destX > destination.mWidth - Twidth) // a more efficient version of if ((destX + Twidth) > destination.mWidth) {
            {
                // case 2: clip right (masking, ascending, shift right) -- criteria: dest end > dest width

                if (destX >= destination.mWidth)
                    return false; // NOP (off screen to the right)

                //         word0            word1            word2            word3           |offscreen....
                //        |----------------|----------------|----------------|----------------|################|################|################
                // SRC:   |abcdefghijklmnop qrstuvwxyz012345|                                                                  src image slice
                // DEST:                                                     |>abcdefghijklmno pqrstuvwxyz01234 5
                // this case is only invoked when >= 1 words are offscreen to the right.
                // shift of 1 normally means expanding, in order to allow the shifted image to appear to the right of the dest blit rect.
                // but here, right-shifted pixels will always be offscreen so just ignore the "expansion".
                //
                // so, chop off invisible words, and create a mask to mask out all offscreen pixels.
                // actually in the end there will be NO offscreen pixels because everything is word-aligned. in the above example, 3 words of blit
                // now becomes just 1.
                // in this case there are 17 offscreen pixels with shifting; reducing to < 1 word = 1 word.
                // SRC:   |abcdefghijklmno-|   new src slice
                // SRC:   |###############-|   and the mask (=shift)

                const int16_t destXWord = destX / 16;
                const int16_t shift = destX & 0xf; // ignore expansion because it's clipped.
                int16_t widthWords = (destination.mWidth / 16) - destXWord;

                const size_t srcOffset = src.GetOffsetOfXYInBitplane(TsrcX, srcY);
                const size_t destOffset = destination.GetOffsetOfXYInBitplane(destXWord * 16, destY); // 40 * 5 * 150 + 10; // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels)

                WaitBlit();

                custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
                custom->bltcon1 = (shift << BSHIFTSHIFT); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // shift, area mode, no fill, ascending

                // B & A = mask & src
                custom->bltapt = (APTR)(src.mMaskBitplanePointers[0] + srcOffset);
                custom->bltbpt = (APTR)(src.mNonmaskBitplanePointers[0] + srcOffset);
                custom->bltbmod = custom->bltamod = src.CalculateModuloBytes(widthWords); // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
                custom->bltafwm = 0xffff;
                custom->bltalwm = setLeftmostBits(16 - shift);

                // C & D have a lot of similarities due to same sizes.
                custom->bltdmod = custom->bltcmod = destination.CalculateModuloBytes(widthWords); // D and C modulos will always be the same due to same dimensions. img width 320 - 32 = 2 (efficient to calc together with above)
                custom->bltcpt = (APTR)(background.mNonmaskBitplanePointers[0] + destOffset);
                custom->bltdpt = (APTR)(destination.mNonmaskBitplanePointers[0] + destOffset);

                custom->bltsize = ((height * TnonMaskBitplanes) << HSIZEBITS) | widthWords;
                destination.mDirtyRect.MarkDirtyRect(destXWord * 16, destY, widthWords * 16, height);

            } // case 1
            else
            {
                // case 3: no clipping (no masking, ascending, shift right) -- criteria: else.
                // width has already been checked, and it's not being clipped out so no NOP checking.

                // srcY, destY, height are already calculated properly.
                // we need to calculate 1) if expansion is needed (actual blit width)
                // then, dest position word, dest position right shift

                const int16_t destXWord = destX / 16;
                const int16_t shift = destX & 0xf;
                bool needsExpansion = shift != 0; // expansion is always needed if dest x is not word-aligned. bleed between words is inevitable.
                int16_t widthWords = needsExpansion ? (1 + (Twidth / 16)) : (Twidth / 16);

                // if the src has no more room to expand, it doesn't really matter because the masking will handle it.

                const size_t srcOffset = src.GetOffsetOfXYInBitplane(TsrcX, srcY);
                const size_t destOffset = destination.GetOffsetOfXYInBitplane(destXWord * 16, destY); // 40 * 5 * 150 + 10; // 40 pitch * 5 bitplanes * 150 lines, +20 (*8=160pixels)

                WaitBlit();

                custom->bltcon0 = (logic) | (SRCA | SRCB | SRCC | DEST) | (shift << ASHIFTSHIFT);
                custom->bltcon1 = (shift << BSHIFTSHIFT); // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // shift, area mode, no fill, ascending

                // B & A = mask & src
                custom->bltapt = (APTR)(src.mMaskBitplanePointers[0] + srcOffset);
                custom->bltbpt = (APTR)(src.mNonmaskBitplanePointers[0] + srcOffset);
                custom->bltbmod = custom->bltamod = src.CalculateModuloBytes(widthWords); // img width 64, + mask 64; blit width 32 means 16 remaining / 8 = 2 + mask 8 = 10
                custom->bltafwm = 0xffff;
                custom->bltalwm = needsExpansion ? 0 : 0xffff; // if expanding was required due to shift, mask out the unwanted extra word.

                // C & D have a lot of similarities due to same sizes.
                custom->bltdmod = custom->bltcmod = destination.CalculateModuloBytes(widthWords); // D and C modulos will always be the same due to same dimensions. img width 320 - 32 = 2 (efficient to calc together with above)
                custom->bltcpt = (APTR)(background.mNonmaskBitplanePointers[0] + destOffset);
                custom->bltdpt = (APTR)(destination.mNonmaskBitplanePointers[0] + destOffset);

                custom->bltsize = ((height * TnonMaskBitplanes) << HSIZEBITS) | widthWords;
                destination.mDirtyRect.MarkDirtyRect(destXWord * 16, destY, widthWords * 16, height);
            }
            return true;
        } // BlitMasked

        template <uint16_t width, uint16_t height, uint16_t Tmultiplier>
        struct BlitDupeHelper
        {
            // Check if height can be evenly divided by Tmultiplier
            static constexpr bool isHeightDivisible = (height % Tmultiplier) == 0;
            // Check if width * Tmultiplier is <= 63
            static constexpr bool isWidthWithinLimit = (width / 16 * Tmultiplier) <= 63;
            static constexpr bool success = isHeightDivisible && isWidthWithinLimit;
        };

        // copies a whole image to another.
        template <USHORT Twidth, USHORT Theight, USHORT TnonMaskBitplanes, ImageFlags Tflags>
        static bool BlitDupe(const Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &src,
                             Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &dest)
        {
            // because blocks are contiguous we can expand widthwords. the catch is that the height must be a multiple of the multiplier.
            // almost always the height will be a multiple of 2, 4, 8, or 16. so let's just support those 4 cases.
            // this can avoid multiple blits.

            constexpr uint16_t lineMultiplier = BlitDupeHelper<Twidth, Theight, 16>::success ? 16 :              //
                                                    (BlitDupeHelper<Twidth, Theight, 8>::success ? 8 :           //
                                                         (BlitDupeHelper<Twidth, Theight, 4>::success ? 4 :      //
                                                              (BlitDupeHelper<Twidth, Theight, 2>::success ? 2 : //
                                                                   1)));

            // A = src, D is dest.
            int16_t line = 0;
            int16_t linesRemaining = Theight * src.mAllBitplanesCount / lineMultiplier;
            constexpr auto lineSizeBytes = src.GetLineSizeBytes() * lineMultiplier;
            constexpr auto widthWords = src.mWidthWords * lineMultiplier;
            while (linesRemaining > 0)
            {
                WaitBlit();

                custom->bltcon0 = (A_TO_D) | (SRCA | DEST);
                custom->bltcon1 = 0; // B shift, ascending
                custom->bltamod = 0;
                custom->bltdmod = 0;
                custom->bltbdat = 0;
                custom->bltcdat = 0;

                custom->bltafwm = 0xffff;
                custom->bltalwm = 0xffff;

                auto offset = line * lineSizeBytes;
                custom->bltapt = (APTR)(src.mpData + offset);
                custom->bltdpt = (APTR)(dest.mpData + offset);

                // calculate lines to blit. gotta respect the fact that only 1024 lines are maximum.
                // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html#line82
                int16_t heightToBlit = std::min((int16_t)1024, linesRemaining);

                custom->bltsize = ((heightToBlit) << HSIZEBITS) | widthWords;

                linesRemaining -= heightToBlit;
                line += heightToBlit;
            }

            dest.mDirtyRect.MarkWholeImageAsDirty();

            return true;
        } // BlitDupe

        // copies a whole image to another.
        template <USHORT Twidth, USHORT Theight, USHORT TnonMaskBitplanes, ImageFlags Tflags>
        static bool Zero(const Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &dest)
        {
            // because blocks are contiguous we can expand widthwords. the catch is that the height must be a multiple of the multiplier.
            // almost always the height will be a multiple of 2, 4, 8, or 16. so let's just support those 4 cases.
            // this can avoid multiple blits.

            constexpr uint16_t lineMultiplier = BlitDupeHelper<Twidth, Theight, 16>::success ? 16 :              //
                                                    (BlitDupeHelper<Twidth, Theight, 8>::success ? 8 :           //
                                                         (BlitDupeHelper<Twidth, Theight, 4>::success ? 4 :      //
                                                              (BlitDupeHelper<Twidth, Theight, 2>::success ? 2 : //
                                                                   1)));

            // A = src, D is dest.
            int16_t line = 0;
            int16_t linesRemaining = Theight * dest.mAllBitplanesCount / lineMultiplier;
            constexpr auto lineSizeBytes = dest.GetLineSizeBytes() * lineMultiplier;
            constexpr auto widthWords = dest.mWidthWords * lineMultiplier;
            while (linesRemaining > 0)
            {
                WaitBlit();

                custom->bltcon0 = (A_TO_D) | (DEST);
                custom->bltcon1 = 0; // B shift, ascending
                custom->bltadat = 0;
                custom->bltbdat = 0;
                custom->bltcdat = 0;
                custom->bltdmod = 0;
                custom->bltafwm = 0xffff;
                custom->bltalwm = 0xffff;

                auto offset = line * lineSizeBytes;
                custom->bltdpt = (APTR)(dest.mpData + offset);

                // calculate lines to blit. gotta respect the fact that only 1024 lines are maximum.
                // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html#line82
                int16_t heightToBlit = std::min((int16_t)1024, linesRemaining);

                custom->bltsize = ((heightToBlit) << HSIZEBITS) | widthWords;

                linesRemaining -= heightToBlit;
                line += heightToBlit;
            } // Zero

            dest.mDirtyRect.MarkWholeImageAsDirty();

            return true;
        } // Zero()

        // between images of same attributes. no clipping is supported, no checking for nops.
        // all coords are word-aligned.
        template <USHORT TimageWidth, USHORT TimageHeight, USHORT TnonMaskBitplanes, ImageFlags TimageFlags>
        static bool BlitNonMasked(const Image<TimageWidth, TimageHeight, TnonMaskBitplanes, TimageFlags> &src,
                                  Image<TimageWidth, TimageHeight, TnonMaskBitplanes, TimageFlags> &destination,
                                  int16_t x, int16_t y, int16_t width, int16_t height)
        {
            // static_assert((TsrcX & 0xf) == 0, "blit source X must be word-aligned due to hardware limitations");
            // static_assert((Twidth & 0xf) == 0, "blit width must be word-aligned due to hardware limitations");
            static_assert(is_flag_set(TimageFlags, ImageFlags::Interleaved), "this function designed only for interleaved");
            ASSERT(x >= 0, "this fn optimized for in-range word-aligned coords.");
            ASSERT((x & 15) == 0, "this fn optimized for in-range word-aligned coords.");
            ASSERT((width & 15) == 0, "this fn optimized for in-range word-aligned coords.");
            ASSERT((x + width) <= TimageWidth, "invalid blit size");
            ASSERT((y + height) <= TimageHeight, "invalid blit size");

            // unfortunately because these are not complete lines, cannot use line multiplication to avoid multiple blits.
            int16_t line = y;
            int16_t linesRemaining = height;
            int16_t widthWords = width / 16;

            while (linesRemaining > 0)
            {
                const size_t offset = src.GetOffsetOfXYInBitplane(x, line);

                WaitBlit();

                custom->bltcon0 = (A_TO_D) | (SRCA | DEST);
                custom->bltcon1 = 0; // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node001A.html // shift, area mode, no fill, ascending
                custom->bltbdat = 0;
                custom->bltcdat = 0;
                custom->bltafwm = 0xffff;
                custom->bltalwm = 0xffff;

                custom->bltapt = (APTR)(src.mNonmaskBitplanePointers[0] + offset);
                custom->bltamod = custom->bltdmod = src.CalculateModuloBytes(widthWords);

                custom->bltdpt = (APTR)(destination.mNonmaskBitplanePointers[0] + offset);

                // calculate lines to blit. gotta respect the fact that only 1024 lines are maximum.
                // http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node011B.html#line82
                int16_t heightToBlit = std::min((int16_t)(1024 / TnonMaskBitplanes), linesRemaining);

                custom->bltsize = ((heightToBlit * TnonMaskBitplanes) << HSIZEBITS) | widthWords;

                linesRemaining -= heightToBlit;
                line += heightToBlit;
            }

            destination.mDirtyRect.MarkDirtyRect(x, y, width, height);

            return true;
        } // BlitNonMasked

        // "cleans" the dirty rect using a same-sized source.
        template <USHORT Twidth, USHORT Theight, USHORT TnonMaskBitplanes, ImageFlags Tflags>
        static void CleanRectWithSource(const Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &cleanImage,
                                            Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &offscreen)
        {
            if (!offscreen.mDirtyRect.mIsDirty)
                return ;
            auto startXPixel = offscreen.mDirtyRect.GetDirtyRectStartXPixel();
            auto widthPixels = offscreen.mDirtyRect.GetDirtyRectWidthPixels();
            auto height = offscreen.mDirtyRect.GetDirtyRectHeightLines();
            BlitNonMasked(cleanImage, offscreen, startXPixel, offscreen.mDirtyRect.mDirtyRectStartY, widthPixels, height);
            offscreen.mDirtyRect.MarkAsClean();
        }

        // when blitting from double-buffered offscreen to the live screen, use this.
        // it will blit from offscreen to live, but only the offscreen dirty rect.
        // the dirty rect must be a union of the previous dirty rect + the new one, in order to erase the previous frame's changes.
        template <USHORT Twidth, USHORT Theight, USHORT TnonMaskBitplanes, ImageFlags Tflags>
        static void BlitDirtyRect(const Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &offscreen,
                                      Image<Twidth, Theight, TnonMaskBitplanes, Tflags> &live)
        {
            if (!offscreen.mDirtyRect.mIsDirty)
                return;
            // auto startXPixel = offscreen.mDirtyRect.GetDirtyRectStartXPixel();
            // auto widthPixels = offscreen.mDirtyRect.GetDirtyRectWidthPixels();
            // auto height = offscreen.mDirtyRect.GetDirtyRectHeightLines();
            auto rgn = offscreen.mDirtyRect;
            rgn.MarkDirtyRect(live.mDirtyRect);
            BlitNonMasked(offscreen, live, rgn.GetDirtyRectStartXPixel(), rgn.mDirtyRectStartY, rgn.GetDirtyRectWidthPixels(), rgn.GetDirtyRectHeightLines());
            live.mDirtyRect = offscreen.mDirtyRect;
        }
    };
}
