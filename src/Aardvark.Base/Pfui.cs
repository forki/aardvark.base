using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aardvark.Base
{
    public enum DebuggerBrowsableState { Never };
    class DebuggerBrowsable : Attribute
    {
        public DebuggerBrowsable(DebuggerBrowsableState a) { }
    }

    public enum LayoutKind { Sequential };
    class StructLayout : Attribute
    {
        public StructLayout(LayoutKind kind) { }
    }

}
