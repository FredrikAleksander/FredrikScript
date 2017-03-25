namespace FredrikScript.Core.Expressions
{
    public class DivideExpression : BinaryExpression
    {
        public DivideExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
        }
    }
}
