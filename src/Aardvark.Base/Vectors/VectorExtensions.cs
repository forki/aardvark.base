using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;

namespace Aardvark.Base
{
    public static class VectorExtensions
    {
        #region Sum of vectors

        public static V2i Sum(this IEnumerable<V2i> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V2l Sum(this IEnumerable<V2l> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V2f Sum(this IEnumerable<V2f> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V2d Sum(this IEnumerable<V2d> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V3i Sum(this IEnumerable<V3i> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V3l Sum(this IEnumerable<V3l> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V3f Sum(this IEnumerable<V3f> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V3d Sum(this IEnumerable<V3d> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V4i Sum(this IEnumerable<V4i> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V4l Sum(this IEnumerable<V4l> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V4f Sum(this IEnumerable<V4f> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }
        public static V4d Sum(this IEnumerable<V4d> vs)
        {
            return vs.Aggregate((a, v) => a + v);
        }

        #endregion


    }
}
