using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class DeclareAssignVariableExpression : Expression
    {
        private readonly string _name;
        private readonly IType _type;
        private readonly Expression _initExpression;

        public DeclareAssignVariableExpression(Context context, SourceInformation sourceInformation, string name, IType type, Expression initExpression) : base(context, sourceInformation)
        {
            _name = name ?? throw new ArgumentNullException(nameof(type));
            _initExpression = initExpression ?? throw new ArgumentNullException(nameof(initExpression));
            _type = type ?? initExpression.ExpressionType;
        }

        public string Name => _name;
        public IType Type => _type;
        public Expression InitExpression => _initExpression;
    }
}
