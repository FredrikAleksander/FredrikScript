using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class NopExpression : Expression
    {
        public NopExpression(Context context, SourceInformation sourceInformation) : base(context, sourceInformation)
        {
        }
    }
}
