using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class VariableExpression : Expression
    {
        private readonly string _name;
        private readonly IType _type;

        public VariableExpression(Context context, SourceInformation sourceInformation, string name, IType type) : base(context, sourceInformation)
        {
            _name = name;
            _type = type;
        }

        public string Name => _name;
        public override IType ExpressionType => _type;
    }
}
