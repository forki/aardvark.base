﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aardvark.Base
{
    /// <summary>
    /// Various coordinate transformations.
    /// 
    /// Abbreviations:
    /// ll = lower left, lr = lower right, ul = upper left, ur = upper right
    /// 
    /// 2D:
    /// Normalized Image Pos:
    /// Our coord-exchange format; Independent of reolution.
    /// (0,0)=ul edge of sensor = ul edge of ul pixel, (1,1) = lr edge of sensor = lr edge of lr pixel.
    /// 
    /// Pixel Center:
    /// (0,0)=center of ul pixel, (w-1, h-1)=center of lr pixel.
    /// 
    /// Pixel Edge:
    /// (0,0)=ul edge of ul pixel, (w-1, h-1)= lr edge of lr pixel.
    /// </summary>
    public static class CoordTransforms
    {
        public static readonly V2d V2dHalf = new V2d(0.5, 0.5);

        /// <summary>
        /// Convert from pixel-center position to normalized image position [0,1][0,1].
        /// (The inverse of toPixelCenter.)
        /// </summary>
        /// <param name="pos">The pixel location defined in pixel space: (0,0)=center of upper left pixel, (w-1,h-1)=center of lower right pixel.</param>
        /// <param name="imgSizeInPixel">The size of the image (as V2d to safe lots of conversions).</param>
        /// <returns>A normalized image position in [0,1][0,1].</returns>
        public static V2d PixelCenterToNormalizedImgPos(V2d pos, V2d imgSizeInPixel)
        {
            return (pos + V2dHalf) / imgSizeInPixel;
        }
        public static V2d PixelCenterToNormalizedImgPos(V2i pos, V2d imgSizeInPixel)
        {
            return PixelCenterToNormalizedImgPos((V2d)pos, imgSizeInPixel);
        }
        public static V2d PixelCenterToNormalizedImgPos(int x, int y, V2d imgSizeInPixel)
        {
            return PixelCenterToNormalizedImgPos(new V2d(x, y), imgSizeInPixel);
        }

        /// <summary>
        /// Returns a Matrix to 
        /// convert from pixel-center position to normalized image position [0,1][0,1].
        /// (The inverse of toPixelCenter.)
        /// </summary>
        public static M33d PixelCenterToNormalizedImgMat(V2d imgSizeInPixel)
        {
            return M33d.Scale(V2d.II / imgSizeInPixel) * M33d.Translation(V2dHalf);
        }

        /// <summary>
        /// Convert from normalized image position [0,1][0,1] to pixel-center position
        /// (The inverse of toNormalizedImgPos.)
        /// </summary>
        /// <param name="pos">The pixel location defined in pixel space: (0,0)=center of upper left pixel, (w-1,h-1)=center of lower right pixel.</param>
        /// <param name="imgSizeInPixel">The size of the image (as V2d to safe lots of conversions).</param>
        /// <returns>A image position in [-0.5,imgSizeInPixel.X-0.5][-0.5,imgSizeInPixel.Y-0.5].</returns>
        public static V2d NormalizedImagePosToPixelCenter(V2d pos, V2i imgSizeInPixel)
        {
            return new V2d(pos.X * imgSizeInPixel.X, pos.Y * imgSizeInPixel.Y) - V2dHalf;
        }

        /// <summary>
        /// Convert from normalized image position [0,1][0,1] to pixel-center position
        /// (The inverse of toNormalizedImgPos.)
        /// </summary>
        /// <param name="pos">The pixel location defined in pixel space: (0,0)=center of upper left pixel, (w-1,h-1)=center of lower right pixel.</param>
        /// <param name="imgSizeInPixel">The size of the image (as V2d to safe lots of conversions).</param>
        /// <returns>A image position in [-0.5,imgSizeInPixel.X-0.5][-0.5,imgSizeInPixel.Y-0.5].</returns>
        public static V2d NormalizedImagePosToPixelCenter(V2d pos, V2l imgSizeInPixel)
        {
            return new V2d(pos.X * imgSizeInPixel.X, pos.Y * imgSizeInPixel.Y) - V2dHalf;
        }

        /// <summary>
        /// Convert from normalized image position [0,1][0,1] to already rounded pixel-center position.
        /// </summary>
        /// <param name="pos">The pixel location defined in pixel space: (0,0)=center of upper left pixel, (w-1,h-1)=center of lower right pixel.</param>
        /// <param name="imgSizeInPixel">The size of the image (as V2d to safe lots of conversions).</param>
        /// <returns>A normalized image position in [0, imgSizeInPixel.X-1][0, imgSizeInPixel.Y-1].</returns>
        public static V2i NormalizedImagePosToPixelCenterRound(V2d pos, V2i imgSizeInPixel)
        {
            return (V2i)NormalizedImagePosToPixelCenter(pos, imgSizeInPixel).Copy(v => System.Math.Round(v));
        }

        /// <summary>
        /// Convert from normalized image position [0,1][0,1] to already rounded pixel-center position.
        /// </summary>
        /// <param name="pos">The pixel location defined in pixel space: (0,0)=center of upper left pixel, (w-1,h-1)=center of lower right pixel.</param>
        /// <param name="imgSizeInPixel">The size of the image (as V2d to safe lots of conversions).</param>
        /// <returns>A normalized image position in [0, imgSizeInPixel.X-1][0, imgSizeInPixel.Y-1].</returns>
        public static V2l NormalizedImagePosToPixelCenterRound(V2d pos, V2l imgSizeInPixel)
        {
            return (V2l)NormalizedImagePosToPixelCenter(pos, imgSizeInPixel).Copy(v => System.Math.Round(v));
        }

        /// <summary>
        /// Returns a Matrix to 
        /// convert from normalized image position [0,1][0,1] to pixel-center position
        /// (The inverse of toNormalizedImgPos.)
        /// </summary>
        public static M33d NormalizedImagePosToPixelCenterMat(V2d imgSizeInPixel)
        {
            return M33d.Translation(-V2dHalf) * M33d.Scale(imgSizeInPixel);
        }

        //[ISSUE 20090819 andi] add docu
        /////////////////////////
        public static V2d PixelEdgeToNormalizedImgPos(V2d pos, V2d imgSizeInPixel)
        {
            return pos / (imgSizeInPixel-1);
        }
        public static V2d PixelEdgeToNormalizedImgPos(V2i pos, V2d imgSizeInPixel)
        {
            return PixelEdgeToNormalizedImgPos((V2d)pos, imgSizeInPixel);
        }
        public static M33d PixelEdgeToNormalizedImgMat(V2d imgSizeInPixel)
        {
            return M33d.Scale(V2d.II / (imgSizeInPixel-1));
        }
        public static V2d NormalizedImagePosToPixelEdge(V2d pos, V2d imgSizeInPixel)
        {
            return pos * (imgSizeInPixel-1);
        }
        public static V2i NormalizedImagePosToPixelEdgeRound(V2d pos, V2d imgSizeInPixel)
        {
            return (V2i)NormalizedImagePosToPixelEdge(pos, imgSizeInPixel).Copy(v => System.Math.Round(v));
        }
        public static M33d NormalizedImagePosToPixelEdgeMat(V2d imgSizeInPixel)
        {
            return M33d.Scale(imgSizeInPixel-1);
        }

    }
}
