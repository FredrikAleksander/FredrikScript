using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public abstract class Expression
    {
        WeakReference<Context> _context;

        public Expression(Context context, SourceInformation sourceInformation)
        {
            _context = new WeakReference<Context>(context);
            SourceInformation = sourceInformation;
        }

        public SourceInformation SourceInformation { get; }
        public Context Context
        {
            get
            {
                if (_context.TryGetTarget(out var context))
                    return context;
                return null;
            }
        }
        public virtual IType ExpressionType
        {
            get
            {
                return Context?.Types["void"];
            }
        }
    }
}
