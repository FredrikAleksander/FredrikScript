using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class DoWhileExpression : Expression
    {
        public DoWhileExpression(Context context, SourceInformation sourceInformation) : base(context, sourceInformation)
        {
        }

        public Expression Body { get; set; }
        public Expression Condition { get; set; }
    }
}
