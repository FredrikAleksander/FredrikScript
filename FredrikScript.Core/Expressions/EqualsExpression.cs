using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class EqualsExpression : BinaryExpression
    {
        public EqualsExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
        }

        public override IType ExpressionType => Context?.Types["bool"];
    }
}
