using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aardvark.Base.Web
{
    public enum EditorBrowsableState { Never };
    class DebuggerBrowsable : Attribute
    {
        public DebuggerBrowsable(EditorBrowsableState a) { }
    }
}
