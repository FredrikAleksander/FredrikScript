using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class DeclareVariableExpression : Expression
    {
        private readonly string _name;
        private readonly IType _type;

        public DeclareVariableExpression(Context context, SourceInformation sourceInformation, string name, IType type) : base(context, sourceInformation)
        {
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _type = type ?? throw new ArgumentNullException(nameof(type));
        }

        public string Name => _name;
        public IType Type => _type;
    }
}
