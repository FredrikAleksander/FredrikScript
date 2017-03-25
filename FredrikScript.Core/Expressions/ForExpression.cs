using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Expressions
{
    public class ForExpression : Expression
    {
        private Expression firstExpr;
        private Expression middleExpr;
        private Expression lastExpr;
        private BlockExpression forBlockExpression;

        public ForExpression(Context context, SourceInformation sourceInformation, Expression firstExpr, Expression middleExpr, Expression lastExpr, BlockExpression forBlockExpression) : base(context, sourceInformation)
        {
            this.firstExpr = firstExpr;
            this.middleExpr = middleExpr;
            this.lastExpr = lastExpr;
            this.forBlockExpression = forBlockExpression;
        }

        public Expression LeftExpression { get; }
        public Expression MiddleExpression { get; }
        public Expression RightExpression { get; }
        public Expression Statements { get; }
    }
}
