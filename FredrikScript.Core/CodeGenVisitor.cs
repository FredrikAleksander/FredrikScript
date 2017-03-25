using FredrikScript.Core.Expressions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core
{
    public class CodeGenVisitor
    {
        protected void Visit(Expression expression)
        {
            if(expression is BlockExpression) { VisitBlock(expression as BlockExpression); }
            else if(expression is ReturnExpression) { VisitReturn(expression as ReturnExpression); }
            else if(expression is ReturnValueExpression) { VisitReturnValue(expression as ReturnValueExpression); }
            else if(expression is IntegerExpression) { VisitInteger(expression as IntegerExpression); }
        }

        protected virtual void VisitBlock(BlockExpression blockExpression)
        {
        }
        protected virtual void VisitReturn(ReturnExpression returnExpression)
        {
        }
        protected virtual void VisitReturnValue(ReturnValueExpression returnValueExpression)
        {
        }
        protected virtual void VisitInteger(IntegerExpression integerExpression)
        {
        }
    }
}
