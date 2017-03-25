namespace FredrikScript.Core.Expressions
{
    public abstract class TernaryExpression : Expression
    {
        public TernaryExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression middleValue, Expression rightValue) : base(context, sourceInformation)
        {
            LeftValue = leftValue;
            MiddleValue = middleValue;
            RightValue = rightValue;
        }

        public Expression LeftValue { get; }
        public Expression MiddleValue { get; }
        public Expression RightValue { get; }
    }
}
