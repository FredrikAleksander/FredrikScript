using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class BlockExpression : Expression
    {
        public BlockExpression(Context context, SourceInformation sourceInformation, Expression[] expressions) : base(context, sourceInformation)
        {
            Expressions = expressions;
        }

        public Expression[] Expressions { get; }
    }
}
