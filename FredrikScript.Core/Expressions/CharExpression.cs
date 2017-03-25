using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class CharExpression : Expression
    {
        public CharExpression(Context context, SourceInformation sourceInformation, char value) : base(context, sourceInformation)
        {
            Value = value;
        }

        public char Value { get; }

        public override IType ExpressionType => Context?.Types["char"];
    }
}
