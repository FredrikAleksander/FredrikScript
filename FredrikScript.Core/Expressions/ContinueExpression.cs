using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class ContinueExpression : Expression
    {
        public ContinueExpression(Context context, SourceInformation sourceInformation) : base(context, sourceInformation)
        {
        }
    }
}
