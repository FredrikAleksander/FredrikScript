using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class StringExpression : Expression
    {
        public StringExpression(Context context, SourceInformation sourceInformation, string value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public string Value { get; }

        public override IType ExpressionType => Context?.Types["string"];
    }
}
