﻿using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;

namespace Aardvark.Base
{
    #region IndexedValue

    public struct IndexedValue<T>
    {
        public readonly int Index;
        public readonly T Value;

        #region Constructor

        public IndexedValue(int index, T value)
        {
            Index = index; Value = value;
        }

        #endregion
    }

    #endregion

    #region ComparableIndexedValue

    public struct ComparableIndexedValue<T> : IComparable<ComparableIndexedValue<T>>
        where T : IComparable<T>
    {
        public readonly int Index;
        public readonly T Value;

        #region Constructor

        public ComparableIndexedValue(int index, T value)
        {
            Index = index; Value = value;
        }

        #endregion

        #region IComparable<IndexedValue<T>> Members

        public int CompareTo(ComparableIndexedValue<T> other)
        {
            return Value.CompareTo(other.Value);
        }

        #endregion
    }

    #endregion

    #region Volatile Array

    /// <summary>
    /// A wrapper for a normal array for safe parallel (multi-threaded)
    /// writing.
    /// </summary>
    public struct VolatileArray<T>
    {
        public volatile T[] Array;

        public VolatileArray(T[] array) { Array = array; }
    }

    #endregion

    #region StructsExtensions

    public static class StructsExtensions
    {
        #region IndexedValue

        public static IndexedValue<T> IndexedValue<T>(
                this T self, int index)
        {
            return new IndexedValue<T>(index, self);
        }

        #endregion

        #region ComparableIndexedValue

        public static ComparableIndexedValue<T> ComparableIndexedValue<T>(
                this T self, int index)
            where T : IComparable<T>
        {
            return new ComparableIndexedValue<T>(index, self);
        }

        #endregion

        #region VolatileArray

        public static VolatileArray<T> ToVolatile<T>(this T[] array)
        {
            return new VolatileArray<T>(array);
        }

        #endregion
    }

    #endregion
}