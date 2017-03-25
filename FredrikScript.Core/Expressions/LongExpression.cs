using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class LongExpression : Expression
    {
        public LongExpression(Context context, SourceInformation sourceInformation, long value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public long Value { get; }

        public override IType ExpressionType => Context?.Types["long"];
    }
}
