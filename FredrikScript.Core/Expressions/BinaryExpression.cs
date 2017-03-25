namespace FredrikScript.Core.Expressions
{
    public abstract class BinaryExpression : Expression
    {
        public BinaryExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation)
        {
        }

        public Expression LeftValue { get; }
        public Expression RightValue { get; }
    }
}
