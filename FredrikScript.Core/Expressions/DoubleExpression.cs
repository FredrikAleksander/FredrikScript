using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class DoubleExpression : Expression
    {
        public DoubleExpression(Context context, SourceInformation sourceInformation, double value) : base(context, sourceInformation)
        {
        }

        public override IType ExpressionType => Context?.Types["double"];
    }
}
