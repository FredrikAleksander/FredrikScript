using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class FloatExpression : Expression
    {
        public FloatExpression(Context context, SourceInformation sourceInformation, float value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public float Value { get; }

        public override IType ExpressionType => Context?.Types["float"];
    }
}
