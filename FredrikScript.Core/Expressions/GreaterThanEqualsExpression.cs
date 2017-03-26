using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class GreaterThanEqualsExpression : BinaryExpression
    {
        public GreaterThanEqualsExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
        }

        public override IType ExpressionType => Context?.Types["bool"];
    }
}
