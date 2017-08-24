using Aardvark.Base;
using System;
using System.Collections.Generic;

namespace Aardvark.VRVis
{
    public static class TypeInfoVersion
    {
        public static void Add(Type type, int version)
        {
            Add(type, version, null, null, null);
        }

        public static void Add(
                Type type, int version,
                Type targetType, Type newestType,
                Action<Convertible, Convertible> converter)
        {
            lock (s_lock)
            {
                TypeInfo.Add(type, version);
                TypeInfo typeInfo = TypeInfo.OfType[type];

                List<Type> oldTypes;
                if (s_oldTypeListOfType.TryGetValue(typeInfo.Type,
                                                    out oldTypes))
                {
                    foreach (var oldType in oldTypes)
                        typeInfo.AddVersion(TypeInfo.OfType[oldType]);
                    s_oldTypeListOfType.Remove(typeInfo.Type);
                }

                Tup<string, Action<Convertible, Convertible>> conversion;
                if (s_conversionOfTargetType.TryGetValue(typeInfo.Type,
                                                         out conversion))
                {
                    Converter.Global.Register(conversion.E0, typeInfo.Name, conversion.E1);
                    s_conversionOfTargetType.Remove(typeInfo.Type);
                }

                if (newestType == null) return;

                TypeInfo newestTypeInfo;
                if (!TypeInfo.OfType.TryGetValue(newestType, out newestTypeInfo))
                {
                    List<Type> oldTypeList;
                    if (!s_oldTypeListOfType.TryGetValue(newestType, out oldTypeList))
                    {
                        oldTypeList = new List<Type>();
                        s_oldTypeListOfType.Add(newestType, oldTypeList);
                    }
                    oldTypeList.Add(type);
                }
                else
                    newestTypeInfo.AddVersion(typeInfo);

                TypeInfo targetTypeInfo;
                if (!TypeInfo.OfType.TryGetValue(targetType, out targetTypeInfo))
                    s_conversionOfTargetType.Add(targetType,
                        Tup.Create(typeInfo.Name, converter));
                else
                    Converter.Global.Register(typeInfo.Name, targetTypeInfo.Name, converter);
            }
        }

        private static Dictionary<Type, List<Type>> s_oldTypeListOfType =
            new Dictionary<Type, List<Type>>();

        private static Dictionary<Type, Tup<string, Action<Convertible, Convertible>>>
            s_conversionOfTargetType
            = new Dictionary<Type, Tup<string, Action<Convertible, Convertible>>>();

        private static object s_lock = new object();
    }
}