namespace FredrikScript.Core.Expressions
{
    public abstract class UnaryExpression : Expression
    {
        public UnaryExpression(Context context, SourceInformation sourceInformation, Expression value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public Expression Value { get; }
    }
}
