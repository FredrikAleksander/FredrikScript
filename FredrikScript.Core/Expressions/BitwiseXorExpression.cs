using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class BitwiseXorExpression : BinaryExpression
    {
        public BitwiseXorExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
        }

        public override IType ExpressionType => LeftValue.ExpressionType;
    }
}
