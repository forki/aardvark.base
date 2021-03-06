﻿using System.Runtime.InteropServices;
using System.Runtime.Serialization;

namespace Aardvark.Base
{
    /// <summary>
    /// Represents an integral fraction.
    /// </summary>
    [DataContract]
    [StructLayout(LayoutKind.Sequential)]
    public struct Fraction
    {
        [DataMember]
        public long Numerator;
        [DataMember]
        public long Denominator;

        #region Constructors

        public Fraction(long value)
        {
            Numerator = value;
            Denominator = 1;
        }

        public Fraction(long numerator, long denominator)
        {
            // ensure positive denominator
            Numerator = denominator < 0 ? -numerator : numerator;
            Denominator = Fun.Abs(denominator);
        }

        #endregion

        #region Properties

        public double Value
        {
            get { return (double)Numerator / Denominator; }
        }

        public Fraction Reduced
        {
            get
            {
                long gcd = Fun.GreatestCommonDivisor(Numerator, Denominator);
                return new Fraction(Numerator / gcd, Denominator / gcd);
            }
        }

        #endregion

        #region Operators

        public static Fraction operator +(Fraction a)
        {
            return a;
        }

        public static Fraction operator -(Fraction a)
        {
            return new Fraction(-a.Numerator, a.Denominator);
        }

        public static Fraction operator +(Fraction a, Fraction b)
        {
            long gcd = Fun.GreatestCommonDivisor(a.Denominator, b.Denominator);
            long aDenomDivGcd = a.Denominator / gcd;

            return new Fraction(
                a.Numerator * (b.Denominator / gcd)
                + b.Numerator * aDenomDivGcd,
                aDenomDivGcd * b.Denominator
                );
        }

        public static Fraction operator -(Fraction a, Fraction b)
        {
            return a + (-b);
        }

        public static Fraction operator *(Fraction a, Fraction b)
        {
            return new Fraction(
                a.Numerator * b.Numerator,
                a.Denominator * b.Denominator
                );
        }

        public static Fraction operator /(Fraction a, Fraction b)
        {
            if (b.Numerator < 0)        // ensure positive denominator
                return new Fraction(
                    -a.Numerator * b.Denominator,
                    -b.Numerator * a.Denominator
                    );

            return new Fraction(
                a.Numerator * b.Denominator,
                a.Denominator * b.Numerator
                );
        }

        public static bool operator <(Fraction a, Fraction b)
        {
            return a.Value < b.Value;
        }

        public static bool operator <=(Fraction a, Fraction b)
        {
            return a.Value <= b.Value;
        }

        public static bool operator ==(Fraction a, Fraction b)
        {
            return a.Value == b.Value;
        }

        public static bool operator !=(Fraction a, Fraction b)
        {
            return a.Value != b.Value;
        }

        public static bool operator >=(Fraction a, Fraction b)
        {
            return a.Value >= b.Value;
        }

        public static bool operator >(Fraction a, Fraction b)
        {
            return a.Value > b.Value;
        }

        #endregion

        #region Constants

        /// <summary>
        /// Represents a <see cref="Fraction"/> that evaluates to 0.
        /// This field is constant.
        /// </summary>
        public static readonly Fraction Zero = new Fraction(0, 1);

        /// <summary>
        /// Represents a <see cref="Fraction"/> that evaluates to 1.
        /// This field is constant.
        /// </summary>
        public static readonly Fraction One = new Fraction(1, 1);

        /// <summary>
        /// Represents the smallest positive <see cref="Fraction"/>
        /// value greater than zero. This field is constant.
        /// </summary>
        public static readonly Fraction Epsilon = new Fraction(1, long.MaxValue);

        /// <summary>
        /// Represents the smallest possible value of a <see cref="Fraction"/>.
        /// This field is constant.
        /// </summary>
        public static readonly Fraction MinValue = new Fraction(long.MinValue, 1);

        /// <summary>
        /// Represents the largest possible value of a <see cref="Fraction"/>.
        /// This field is constant.
        /// </summary>
        public static readonly Fraction MaxValue = new Fraction(long.MaxValue, 1);

        /// <summary>
        /// Represents a value that is not a number (NaN).
        /// This field is constant.
        /// </summary>
        public static readonly Fraction NaN = new Fraction(0, 0);

        /// <summary>
        /// Represents negative infinity.
        /// This field is constant.
        /// </summary>
        public static readonly Fraction NegativeInfinity = new Fraction(-1, 0);
       
        /// <summary>
        /// Represents positive infinity.
        /// This field is constant.
        /// </summary>
        public static readonly Fraction PositiveInfinity = new Fraction(+1, 0);

        #endregion

        /// <summary>
        /// Returns whether the specified <see cref="Fraction"/>
        /// evaluates to negative or positive infinity.
        /// </summary>
        public static bool IsInfinity(Fraction f)
        {
            return f.Denominator == 0;
        }

        /// <summary>
        /// Returns whether the specified <see cref="Fraction"/>
        /// evaluates to negative infinity.
        /// </summary>
        public static bool IsNegativeInfinity(Fraction f)
        {
            return f.Denominator == 0 && f.Numerator < 0;
        }

        /// <summary>
        /// Returns whether the specified <see cref="Fraction"/>
        /// evaluates to positive infinity.
        /// </summary>
        public static bool IsPositiveInfinity(Fraction f)
        {
            return f.Denominator == 0 && f.Numerator > 0;
        }

        /// <summary>
        /// Returns whether the specified <see cref="Fraction"/>
        /// evaluates to a value that is not a number (Fraction.NaN).
        /// </summary>
        public static bool IsNaN(Fraction f)
        {
            return f.Denominator == 0;
        }

        public override bool Equals(object obj)
        {
            return (obj is Fraction) ? (this == (Fraction)obj) : false;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override string ToString()
        {
            return Numerator + "/" + Denominator;
        }

        public static Fraction Parse(string s)
        {
            int sep = s.IndexOf('/');
            if (sep < 0) return new Fraction(long.Parse(s));
            return new Fraction(
                long.Parse(s.Substring(0, sep)),
                long.Parse(s.Substring(sep+1, s.Length-sep-1))
                );
        }

    }
}
