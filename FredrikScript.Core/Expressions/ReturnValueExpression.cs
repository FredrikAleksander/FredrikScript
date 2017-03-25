using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class ReturnValueExpression : UnaryExpression
    {
        public ReturnValueExpression(Context context, SourceInformation sourceInformation, Expression value) : base(context, sourceInformation, value)
        {
        }

        public override IType ExpressionType => Context?.Types["void"];
        public virtual IType ValueType => Value.ExpressionType;
    }
}
