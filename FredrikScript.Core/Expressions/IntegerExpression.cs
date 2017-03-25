using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class IntegerExpression : Expression
    {
        public IntegerExpression(Context context, SourceInformation sourceInformation, int value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public int Value { get; }

        public override IType ExpressionType => Context?.Types["int"];
    }
}
