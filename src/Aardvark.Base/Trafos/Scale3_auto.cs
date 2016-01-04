using System;
using System.Linq;
using System.Runtime.InteropServices;

namespace Aardvark.Base
{
    /// <summary>
    /// A three dimensional scaling transform with different scaling values
    /// in each dimension.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public partial struct Scale3f
    {
        public V3f V;

        #region Constructors

        /// <summary>
        /// Initializes a <see cref="Scale3f"/> using three floats.
        /// </summary>
        public Scale3f(float x, float y, float z)
        {
            V = new V3f(x, y, z);
        }

        public Scale3f(V3f v)
        {
            V = v;
        }

        /// <summary>
        /// Initializes a <see cref="Scale3f"/> using a uniform float value.
        /// </summary>
        public Scale3f(float uniform)
        {
            V = new V3f(uniform, uniform, uniform);
        }

        /// <summary>
        /// Initializes a <see cref="Scale3f"/> class from float-array.
        /// </summary>
        public Scale3f(float[] array)
        {
            V = new V3f(array);
        }

        /// <summary>
        /// Initializes a <see cref="Scale3f"/> class from float-array starting from passed index
        /// </summary>
        public Scale3f(float[] array, int start)
        {
            V = new V3f(array, start);
        }

        #endregion

        #region Properties

        /// <summary>
        /// Gets and sets the X coordinate.
        /// </summary>
        public float X
        {
            get { return V.X; }
            set { V.X = value; }
        }

        /// <summary>
        /// Gets and sets the Y coordinate.
        /// </summary>
        public float Y
        {
            get { return V.Y; }
            set { V.Y = value; }
        }

        /// <summary>
        /// Gets and sets the Z coordinate.
        /// </summary>
        public float Z
        {
            get { return V.Z; }
            set { V.Z = value; }
        }

        /// <summary>
        /// Calculates the length of a <see cref="Scale3f"/>.
        /// </summary>
        public float Length
        {
            get { return V.Length; }
        }

        /// <summary>
        /// Calculates the squared length of a <see cref="Scale3f"/>.
        /// </summary>
        public float LengthSquared
        {
            get { return V.LengthSquared; }
        }

        public Scale3f Inverse
        {
            get { return new Scale3f(V.Reciprocal); }
        }

        #endregion

        #region Constants

        public static readonly Scale3f Identity = new Scale3f(1, 1, 1);

        /// <summary>
        /// A <see cref="Scale3d"/> single-precision floating point zero shift vector.
        /// </summary>
        public static readonly Scale3f Zero = new Scale3f(0, 0, 0);

        /// <summary>
        /// A <see cref="Scale3d"/> single-precision floating point X-Axis shift vector.
        /// </summary>
        public static readonly Scale3f XAxis = new Scale3f(1, 0, 0);

        /// <summary>
        /// A <see cref="Scale3d"/> single-precision floating point Y-Axis shift vector.
        /// </summary>
        public static readonly Scale3f YAxis = new Scale3f(0, 1, 0);

        /// <summary>
        /// A <see cref="Scale3d"/> single-precision floating point Z-Axis shift vector.
        /// </summary>
        public static readonly Scale3f ZAxis = new Scale3f(0, 0, 1);

        #endregion

        #region Vector Arithmetics

        /// <summary>
        /// Multiplacation of a float scalar with a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f Multiply(Scale3f scale, float value)
        {
            return new Scale3f(scale.X * value, scale.Y * value, scale.Z * value);
        }

        /// <summary>
        /// Multiplication of two <see cref="Scale3f"/>s.
        /// </summary>
        public static Scale3f Multiply(Scale3f scale0, Scale3f scale1)
        {
            return new Scale3f(scale0.X * scale1.X, scale0.Y * scale1.Y, scale0.Z * scale1.Z);
        }

        public static M33f Multiply(Scale3f scale, Rot3f rot)
        {
            return Scale3f.Multiply(scale, (M33f)rot);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="V2f"/>.
        /// </summary>
        public static V2f Multiply(Scale3f scale, V2f v)
        {
            return new V2f(v.X * scale.X, v.Y * scale.Y);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="V3f"/>.
        /// </summary>
        public static V3f Multiply(Scale3f scale, V3f v)
        {
            return new V3f(scale.X * v.X, scale.Y * v.Y, scale.Z * v.Z);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="V4f"/>.
        /// </summary>
        public static V4f Multiply(Scale3f scale, V4f vec)
        {
            return new V4f(scale.X * vec.X,
                            scale.Y * vec.Y,
                            scale.Z * vec.Z,
                            vec.W);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="M22f"/>.
        /// </summary>
        public static M33f Multiply(Scale3f scale, M22f mat)
        {
            return new M33f(scale.X * mat.M00,
                             scale.X * mat.M01,
                             0,

                             scale.Y * mat.M10,
                             scale.Y * mat.M11,
                             0,

                             0,
                             0,
                             scale.Z);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="M33f"/>.
        /// </summary>
        public static M33f Multiply(Scale3f scale, M33f mat)
        {
            return new M33f(scale.X * mat.M00,
                             scale.X * mat.M01,
                             scale.X * mat.M02,

                             scale.Y * mat.M10,
                             scale.Y * mat.M11,
                             scale.Y * mat.M12,

                             scale.Z * mat.M20,
                             scale.Z * mat.M21,
                             scale.Z * mat.M22);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="M34f"/>.
        /// </summary>
        public static M34f Multiply(Scale3f scale, M34f mat)
        {
            return new M34f(
                    scale.X * mat.M00,
                    scale.X * mat.M01,
                    scale.X * mat.M02,
                    scale.X * mat.M03,

                    scale.Y * mat.M10,
                    scale.Y * mat.M11,
                    scale.Y * mat.M12,
                    scale.Y * mat.M13,

                    scale.Z * mat.M20,
                    scale.Z * mat.M21,
                    scale.Z * mat.M22,
                    scale.Z * mat.M23
                    );
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="M44f"/>.
        /// </summary>
        public static M44f Multiply(Scale3f scale, M44f mat)
        {
            return new M44f(
                    scale.X * mat.M00,
                    scale.X * mat.M01,
                    scale.X * mat.M02,
                    scale.X * mat.M03,

                    scale.Y * mat.M10,
                    scale.Y * mat.M11,
                    scale.Y * mat.M12,
                    scale.Y * mat.M13,

                    scale.Z * mat.M20,
                    scale.Z * mat.M21,
                    scale.Z * mat.M22,
                    scale.Z * mat.M23,

                    mat.M30,
                    mat.M31,
                    mat.M32,
                    mat.M33
                    );
        }

        public static M33f Multiply(Scale3f scale, Rot2f rot)
        {
            return Scale3f.Multiply(scale, (M22f)rot);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3f"/> with a <see cref="Shift3f"/>.
        /// </summary>
        public static M34f Multiply(Scale3f scale, Shift3f shift)
        {
            return new M34f(
                    scale.X,
                    0,
                    0,
                    scale.X * shift.X,

                    0,
                    scale.Y,
                    0,
                    scale.Y * shift.Y,

                    0,
                    0,
                    scale.Z,
                    scale.Z * shift.Z

                    );
        }

        /// <summary>
        /// Division of a <see cref="Scale3f"/> instance with a float scalar.
        /// </summary>
        public static Scale3f Divide(Scale3f scale, float val)
        {
            return Multiply(scale, 1 / val);
        }

        /// <summary>
        /// Division of a float scalar with a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f Divide(float val, Scale3f scale)
        {
            return Multiply(Reciprocal(scale), val);
        }

        /// <summary>
        /// Negates all values of a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f Negate(Scale3f scale)
        {
            return new Scale3f(-scale.X, -scale.Y, -scale.Z);
        }

        /// <summary>
        /// Calculates the reciprocal of a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f Reciprocal(Scale3f scale)
        {
            return new Scale3f(1 / scale.X, 1 / scale.Y, 1 / scale.Z);
        }

        #endregion

        #region Arithmetic Operators

        /// <summary>
        /// Negates the values of a <see cref="Scale3f"/> instance.
        /// </summary>
        public static Scale3f operator -(Scale3f scalingVector)
        {
            return Scale3f.Negate(scalingVector);
        }

        /// <summary>
        /// Calculates the multiplacation of a float scalar with a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f operator *(Scale3f scale, float scalar)
        {
            return Scale3f.Multiply(scale, scalar);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="V2f"/>.
        /// </summary>
        public static V2f operator *(Scale3f scale, V2f vector)
        {
            return Scale3f.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="V3f"/>.
        /// </summary>
        public static V3f operator *(Scale3f scale, V3f vector)
        {
            return Scale3f.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="V4f"/>.
        /// </summary>
        public static V4f operator *(Scale3f scale, V4f vector)
        {
            return Scale3f.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="M22f"/>.
        /// </summary>
        public static M33f operator *(Scale3f scale, M22f matrix)
        {
            return Scale3f.Multiply(scale, matrix);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="M33f"/>.
        /// </summary>
        public static M33f operator *(Scale3f scale, M33f matrix)
        {
            return Scale3f.Multiply(scale, matrix);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="M34f"/>.
        /// </summary>
        public static M34f operator *(Scale3f scale, M34f matrix)
        {
            return Scale3f.Multiply(scale, matrix);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="M44f"/>.
        /// </summary>
        public static M44f operator *(Scale3f scale, M44f matrix)
        {
            return Scale3f.Multiply(scale, matrix);
        }

        /// <summary>
        /// </summary>
        public static M33f operator *(Scale3f scale, Rot3f quaternion)
        {
            return Scale3f.Multiply(scale, quaternion);
        }

        /// <summary>
        /// </summary>
        public static M33f operator *(Scale3f scale, Rot2f rotation)
        {
            return Scale3f.Multiply(scale, rotation);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a float scalar.
        /// </summary>
        public static Scale3f operator *(float scalar, Scale3f scale)
        {
            return Scale3f.Multiply(scale, scalar);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f operator *(Scale3f scale1, Scale3f scale2)
        {
            return Scale3f.Multiply(scale1, scale2);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="V3f"/> with a <see cref="Scale3f"/>.
        /// </summary>
        public static V3f operator *(V3f vector, Scale3f scale)
        {
            return Scale3f.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3f"/> with a <see cref="Shift3f"/>.
        /// </summary>
        public static M34f operator *(Scale3f scale, Shift3f shift)
        {
            return Scale3f.Multiply(scale, shift);
        }

        /// <summary>
        /// Calculates the division of a <see cref="Scale3f"/> with a float scalar.
        /// </summary>
        public static Scale3f operator /(Scale3f scale, float value)
        {
            return Scale3f.Divide(scale, value);
        }

        /// <summary>
        /// Calculates the division of a float scalar with a <see cref="Scale3f"/>.
        /// </summary>
        public static Scale3f operator /(float value, Scale3f scale)
        {
            return Scale3f.Divide(value, scale);
        }

        #endregion

        #region Comparison Operators

        /// <summary>
        /// Checks whether two <see cref="Scale3f"/>s are equal.
        /// </summary>
        public static bool operator ==(Scale3f s0, Scale3f s1)
        {
            return s0.X == s1.X && s0.Y == s1.Y && s0.Z == s1.Z;
        }

        /// <summary>
        /// Checks whether two <see cref="Scale3f"/> instances are different.
        /// </summary>
        public static bool operator !=(Scale3f s0, Scale3f s1)
        {
            return s0.X != s1.X || s0.Y != s1.Y || s0.Z != s1.Z;
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3f"/> 
        /// are bigger then the components of the second <see cref="Scale3f"/>.
        /// </summary>
        public static bool operator >(Scale3f s0, Scale3f s1)
        {
            return s0.X > s1.X && s0.Y > s1.Y && s0.Z > s1.Z;
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3f"/> 
        /// are bigger or equal then the components of the second <see cref="Scale3f"/>.
        /// </summary>
        public static bool operator >=(Scale3f s0, Scale3f s1)
        {
            return s0.X >= s1.X && s0.Y >= s1.Y && s0.Z >= s1.Z;
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3f"/> 
        /// are smaller then the components of the second <see cref="Scale3f"/>.
        /// </summary>
        public static bool operator <(Scale3f s0, Scale3f s1)
        {
            return ((s0.X < s1.X) && (s0.Y < s1.Y) && (s0.Z < s1.Z));
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3f"/> 
        /// are smaller or equal then the components of the second <see cref="Scale3f"/>.
        /// </summary>
        public static bool operator <=(Scale3f s0, Scale3f s1)
        {
            return s0.X <= s1.X && s0.Y <= s1.Y && s0.Z <= s1.Z;
        }

        #endregion

        #region Convert to Array

        public static explicit operator M33f(Scale3f s)
        {
            return new M33f(s.X, 0, 0,
                            0, s.Y, 0,
                            0, 0, s.Z);
        }

        public static explicit operator M44f(Scale3f s)
        {
            return new M44f(s.X, 0, 0, 0,
                            0, s.Y, 0, 0,
                            0, 0, s.Z, 0,
                            0, 0, 0, 1);
        }

        public static explicit operator M34f(Scale3f s)
        {
            return new M34f(s.X, 0, 0, 0,
                            0, s.Y, 0, 0,
                            0, 0, s.Z, 0);
        }

        /// <summary>
        /// Returns all values of a <see cref="Scale3f"/> instance
        /// in a float[] array.
        /// </summary>
        public static explicit operator float[](Scale3f s)
        {
            float[] array = new float[3];
            array[0] = s.X;
            array[1] = s.Y;
            array[2] = s.Z;
            return array;
        }

        #endregion

        #region Indexing

        public float this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0: return V.X;
                    case 1: return V.Y;
                    case 2: return V.Z;
                    default: throw new InvalidOperationException();
                }
            }
            set
            {
                switch (index)
                {
                    case 0: V.X = value; return;
                    case 1: V.Y = value; return;
                    case 2: V.Z = value; return;
                    default: throw new InvalidOperationException();
                }
            }
        }

        #endregion

        #region Overrides

        public override int GetHashCode()
        {
            return V.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if (obj is Scale3f)
            {
                Scale3f scalingVector = (Scale3f)obj;
                return (X == scalingVector.X) && (Y == scalingVector.Y) && (Z == scalingVector.Z);
            }
            return false;
        }

        public override string ToString()
        {
            return string.Format(Localization.FormatEnUS, "[{0}, {1}, {2}]", X, Y, Z);
        }

        public static Scale3f Parse(string s)
        {
            var x = s.NestedBracketSplitLevelOne().ToArray();
            return new Scale3f(
                float.Parse(x[0], Localization.FormatEnUS),
                float.Parse(x[1], Localization.FormatEnUS),
                float.Parse(x[2], Localization.FormatEnUS)
            );
        }

        #endregion
    }
    /// <summary>
    /// A three dimensional scaling transform with different scaling values
    /// in each dimension.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public partial struct Scale3d
    {
        public V3d V;

        #region Constructors

        /// <summary>
        /// Initializes a <see cref="Scale3d"/> using three doubles.
        /// </summary>
        public Scale3d(double x, double y, double z)
        {
            V = new V3d(x, y, z);
        }

        public Scale3d(V3d v)
        {
            V = v;
        }

        /// <summary>
        /// Initializes a <see cref="Scale3d"/> using a uniform double value.
        /// </summary>
        public Scale3d(double uniform)
        {
            V = new V3d(uniform, uniform, uniform);
        }

        /// <summary>
        /// Initializes a <see cref="Scale3d"/> class from double-array.
        /// </summary>
        public Scale3d(double[] array)
        {
            V = new V3d(array);
        }

        /// <summary>
        /// Initializes a <see cref="Scale3d"/> class from double-array starting from passed index
        /// </summary>
        public Scale3d(double[] array, int start)
        {
            V = new V3d(array, start);
        }

        #endregion

        #region Properties

        /// <summary>
        /// Gets and sets the X coordinate.
        /// </summary>
        public double X
        {
            get { return V.X; }
            set { V.X = value; }
        }

        /// <summary>
        /// Gets and sets the Y coordinate.
        /// </summary>
        public double Y
        {
            get { return V.Y; }
            set { V.Y = value; }
        }

        /// <summary>
        /// Gets and sets the Z coordinate.
        /// </summary>
        public double Z
        {
            get { return V.Z; }
            set { V.Z = value; }
        }

        /// <summary>
        /// Calculates the length of a <see cref="Scale3d"/>.
        /// </summary>
        public double Length
        {
            get { return V.Length; }
        }

        /// <summary>
        /// Calculates the squared length of a <see cref="Scale3d"/>.
        /// </summary>
        public double LengthSquared
        {
            get { return V.LengthSquared; }
        }

        public Scale3d Inverse
        {
            get { return new Scale3d(V.Reciprocal); }
        }

        #endregion

        #region Constants

        public static readonly Scale3d Identity = new Scale3d(1, 1, 1);

        /// <summary>
        /// A <see cref="Scale3d"/> double-precision floating point zero shift vector.
        /// </summary>
        public static readonly Scale3d Zero = new Scale3d(0, 0, 0);

        /// <summary>
        /// A <see cref="Scale3d"/> double-precision floating point X-Axis shift vector.
        /// </summary>
        public static readonly Scale3d XAxis = new Scale3d(1, 0, 0);

        /// <summary>
        /// A <see cref="Scale3d"/> double-precision floating point Y-Axis shift vector.
        /// </summary>
        public static readonly Scale3d YAxis = new Scale3d(0, 1, 0);

        /// <summary>
        /// A <see cref="Scale3d"/> double-precision floating point Z-Axis shift vector.
        /// </summary>
        public static readonly Scale3d ZAxis = new Scale3d(0, 0, 1);

        #endregion

        #region Vector Arithmetics

        /// <summary>
        /// Multiplacation of a double scalar with a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d Multiply(Scale3d scale, double value)
        {
            return new Scale3d(scale.X * value, scale.Y * value, scale.Z * value);
        }

        /// <summary>
        /// Multiplication of two <see cref="Scale3d"/>s.
        /// </summary>
        public static Scale3d Multiply(Scale3d scale0, Scale3d scale1)
        {
            return new Scale3d(scale0.X * scale1.X, scale0.Y * scale1.Y, scale0.Z * scale1.Z);
        }

        public static M33d Multiply(Scale3d scale, Rot3d rot)
        {
            return Scale3d.Multiply(scale, (M33d)rot);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="V2d"/>.
        /// </summary>
        public static V2d Multiply(Scale3d scale, V2d v)
        {
            return new V2d(v.X * scale.X, v.Y * scale.Y);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="V3d"/>.
        /// </summary>
        public static V3d Multiply(Scale3d scale, V3d v)
        {
            return new V3d(scale.X * v.X, scale.Y * v.Y, scale.Z * v.Z);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="V4d"/>.
        /// </summary>
        public static V4d Multiply(Scale3d scale, V4d vec)
        {
            return new V4d(scale.X * vec.X,
                            scale.Y * vec.Y,
                            scale.Z * vec.Z,
                            vec.W);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="M22d"/>.
        /// </summary>
        public static M33d Multiply(Scale3d scale, M22d mat)
        {
            return new M33d(scale.X * mat.M00,
                             scale.X * mat.M01,
                             0,

                             scale.Y * mat.M10,
                             scale.Y * mat.M11,
                             0,

                             0,
                             0,
                             scale.Z);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="M33d"/>.
        /// </summary>
        public static M33d Multiply(Scale3d scale, M33d mat)
        {
            return new M33d(scale.X * mat.M00,
                             scale.X * mat.M01,
                             scale.X * mat.M02,

                             scale.Y * mat.M10,
                             scale.Y * mat.M11,
                             scale.Y * mat.M12,

                             scale.Z * mat.M20,
                             scale.Z * mat.M21,
                             scale.Z * mat.M22);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="M34d"/>.
        /// </summary>
        public static M34d Multiply(Scale3d scale, M34d mat)
        {
            return new M34d(
                    scale.X * mat.M00,
                    scale.X * mat.M01,
                    scale.X * mat.M02,
                    scale.X * mat.M03,

                    scale.Y * mat.M10,
                    scale.Y * mat.M11,
                    scale.Y * mat.M12,
                    scale.Y * mat.M13,

                    scale.Z * mat.M20,
                    scale.Z * mat.M21,
                    scale.Z * mat.M22,
                    scale.Z * mat.M23
                    );
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="M44d"/>.
        /// </summary>
        public static M44d Multiply(Scale3d scale, M44d mat)
        {
            return new M44d(
                    scale.X * mat.M00,
                    scale.X * mat.M01,
                    scale.X * mat.M02,
                    scale.X * mat.M03,

                    scale.Y * mat.M10,
                    scale.Y * mat.M11,
                    scale.Y * mat.M12,
                    scale.Y * mat.M13,

                    scale.Z * mat.M20,
                    scale.Z * mat.M21,
                    scale.Z * mat.M22,
                    scale.Z * mat.M23,

                    mat.M30,
                    mat.M31,
                    mat.M32,
                    mat.M33
                    );
        }

        public static M33d Multiply(Scale3d scale, Rot2d rot)
        {
            return Scale3d.Multiply(scale, (M22d)rot);
        }

        /// <summary>
        /// Multiplacation of a <see cref="Scale3d"/> with a <see cref="Shift3d"/>.
        /// </summary>
        public static M34d Multiply(Scale3d scale, Shift3d shift)
        {
            return new M34d(
                    scale.X,
                    0,
                    0,
                    scale.X * shift.X,

                    0,
                    scale.Y,
                    0,
                    scale.Y * shift.Y,

                    0,
                    0,
                    scale.Z,
                    scale.Z * shift.Z

                    );
        }

        /// <summary>
        /// Division of a <see cref="Scale3d"/> instance with a double scalar.
        /// </summary>
        public static Scale3d Divide(Scale3d scale, double val)
        {
            return Multiply(scale, 1 / val);
        }

        /// <summary>
        /// Division of a double scalar with a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d Divide(double val, Scale3d scale)
        {
            return Multiply(Reciprocal(scale), val);
        }

        /// <summary>
        /// Negates all values of a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d Negate(Scale3d scale)
        {
            return new Scale3d(-scale.X, -scale.Y, -scale.Z);
        }

        /// <summary>
        /// Calculates the reciprocal of a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d Reciprocal(Scale3d scale)
        {
            return new Scale3d(1 / scale.X, 1 / scale.Y, 1 / scale.Z);
        }

        #endregion

        #region Arithmetic Operators

        /// <summary>
        /// Negates the values of a <see cref="Scale3d"/> instance.
        /// </summary>
        public static Scale3d operator -(Scale3d scalingVector)
        {
            return Scale3d.Negate(scalingVector);
        }

        /// <summary>
        /// Calculates the multiplacation of a double scalar with a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d operator *(Scale3d scale, double scalar)
        {
            return Scale3d.Multiply(scale, scalar);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="V2d"/>.
        /// </summary>
        public static V2d operator *(Scale3d scale, V2d vector)
        {
            return Scale3d.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="V3d"/>.
        /// </summary>
        public static V3d operator *(Scale3d scale, V3d vector)
        {
            return Scale3d.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="V4d"/>.
        /// </summary>
        public static V4d operator *(Scale3d scale, V4d vector)
        {
            return Scale3d.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="M22d"/>.
        /// </summary>
        public static M33d operator *(Scale3d scale, M22d matrix)
        {
            return Scale3d.Multiply(scale, matrix);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="M33d"/>.
        /// </summary>
        public static M33d operator *(Scale3d scale, M33d matrix)
        {
            return Scale3d.Multiply(scale, matrix);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="M34d"/>.
        /// </summary>
        public static M34d operator *(Scale3d scale, M34d matrix)
        {
            return Scale3d.Multiply(scale, matrix);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="M44d"/>.
        /// </summary>
        public static M44d operator *(Scale3d scale, M44d matrix)
        {
            return Scale3d.Multiply(scale, matrix);
        }

        /// <summary>
        /// </summary>
        public static M33d operator *(Scale3d scale, Rot3d quaternion)
        {
            return Scale3d.Multiply(scale, quaternion);
        }

        /// <summary>
        /// </summary>
        public static M33d operator *(Scale3d scale, Rot2d rotation)
        {
            return Scale3d.Multiply(scale, rotation);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a double scalar.
        /// </summary>
        public static Scale3d operator *(double scalar, Scale3d scale)
        {
            return Scale3d.Multiply(scale, scalar);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d operator *(Scale3d scale1, Scale3d scale2)
        {
            return Scale3d.Multiply(scale1, scale2);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="V3d"/> with a <see cref="Scale3d"/>.
        /// </summary>
        public static V3d operator *(V3d vector, Scale3d scale)
        {
            return Scale3d.Multiply(scale, vector);
        }

        /// <summary>
        /// Calculates the multiplacation of a <see cref="Scale3d"/> with a <see cref="Shift3d"/>.
        /// </summary>
        public static M34d operator *(Scale3d scale, Shift3d shift)
        {
            return Scale3d.Multiply(scale, shift);
        }

        /// <summary>
        /// Calculates the division of a <see cref="Scale3d"/> with a double scalar.
        /// </summary>
        public static Scale3d operator /(Scale3d scale, double value)
        {
            return Scale3d.Divide(scale, value);
        }

        /// <summary>
        /// Calculates the division of a double scalar with a <see cref="Scale3d"/>.
        /// </summary>
        public static Scale3d operator /(double value, Scale3d scale)
        {
            return Scale3d.Divide(value, scale);
        }

        #endregion

        #region Comparison Operators

        /// <summary>
        /// Checks whether two <see cref="Scale3d"/>s are equal.
        /// </summary>
        public static bool operator ==(Scale3d s0, Scale3d s1)
        {
            return s0.X == s1.X && s0.Y == s1.Y && s0.Z == s1.Z;
        }

        /// <summary>
        /// Checks whether two <see cref="Scale3d"/> instances are different.
        /// </summary>
        public static bool operator !=(Scale3d s0, Scale3d s1)
        {
            return s0.X != s1.X || s0.Y != s1.Y || s0.Z != s1.Z;
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3d"/> 
        /// are bigger then the components of the second <see cref="Scale3d"/>.
        /// </summary>
        public static bool operator >(Scale3d s0, Scale3d s1)
        {
            return s0.X > s1.X && s0.Y > s1.Y && s0.Z > s1.Z;
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3d"/> 
        /// are bigger or equal then the components of the second <see cref="Scale3d"/>.
        /// </summary>
        public static bool operator >=(Scale3d s0, Scale3d s1)
        {
            return s0.X >= s1.X && s0.Y >= s1.Y && s0.Z >= s1.Z;
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3d"/> 
        /// are smaller then the components of the second <see cref="Scale3d"/>.
        /// </summary>
        public static bool operator <(Scale3d s0, Scale3d s1)
        {
            return ((s0.X < s1.X) && (s0.Y < s1.Y) && (s0.Z < s1.Z));
        }

        /// <summary>
        /// Checks whether all components of a <see cref="Scale3d"/> 
        /// are smaller or equal then the components of the second <see cref="Scale3d"/>.
        /// </summary>
        public static bool operator <=(Scale3d s0, Scale3d s1)
        {
            return s0.X <= s1.X && s0.Y <= s1.Y && s0.Z <= s1.Z;
        }

        #endregion

        #region Convert to Array

        public static explicit operator M33d(Scale3d s)
        {
            return new M33d(s.X, 0, 0,
                            0, s.Y, 0,
                            0, 0, s.Z);
        }

        public static explicit operator M44d(Scale3d s)
        {
            return new M44d(s.X, 0, 0, 0,
                            0, s.Y, 0, 0,
                            0, 0, s.Z, 0,
                            0, 0, 0, 1);
        }

        public static explicit operator M34d(Scale3d s)
        {
            return new M34d(s.X, 0, 0, 0,
                            0, s.Y, 0, 0,
                            0, 0, s.Z, 0);
        }

        /// <summary>
        /// Returns all values of a <see cref="Scale3d"/> instance
        /// in a double[] array.
        /// </summary>
        public static explicit operator double[](Scale3d s)
        {
            double[] array = new double[3];
            array[0] = s.X;
            array[1] = s.Y;
            array[2] = s.Z;
            return array;
        }

        #endregion

        #region Indexing

        public double this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0: return V.X;
                    case 1: return V.Y;
                    case 2: return V.Z;
                    default: throw new InvalidOperationException();
                }
            }
            set
            {
                switch (index)
                {
                    case 0: V.X = value; return;
                    case 1: V.Y = value; return;
                    case 2: V.Z = value; return;
                    default: throw new InvalidOperationException();
                }
            }
        }

        #endregion

        #region Overrides

        public override int GetHashCode()
        {
            return V.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if (obj is Scale3d)
            {
                Scale3d scalingVector = (Scale3d)obj;
                return (X == scalingVector.X) && (Y == scalingVector.Y) && (Z == scalingVector.Z);
            }
            return false;
        }

        public override string ToString()
        {
            return string.Format(Localization.FormatEnUS, "[{0}, {1}, {2}]", X, Y, Z);
        }

        public static Scale3d Parse(string s)
        {
            var x = s.NestedBracketSplitLevelOne().ToArray();
            return new Scale3d(
                double.Parse(x[0], Localization.FormatEnUS),
                double.Parse(x[1], Localization.FormatEnUS),
                double.Parse(x[2], Localization.FormatEnUS)
            );
        }

        #endregion
    }
}
