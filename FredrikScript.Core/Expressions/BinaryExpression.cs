namespace FredrikScript.Core.Expressions
{
    public abstract class BinaryExpression : Expression
    {
        public BinaryExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation)
        {
            LeftValue = leftValue;
            RightValue = rightValue;
        }

        public Expression LeftValue { get; }
        public Expression RightValue { get; }
    }
}
