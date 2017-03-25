using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class BooleanExpression : Expression
    {
        public BooleanExpression(Context context, SourceInformation sourceInformation, bool value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public bool Value { get; }

        public override IType ExpressionType => Context?.Types["bool"];
    }
}
