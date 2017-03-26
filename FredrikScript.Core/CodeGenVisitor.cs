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
            else if(expression is AddExpression) { VisitAdd(expression as AddExpression); }
            else if(expression is SubtractExpression) { VisitSubtract(expression as SubtractExpression); }
            else if(expression is MultiplyExpression) { VisitMultiply(expression as MultiplyExpression); }
            else if(expression is DivideExpression) { VisitDivide(expression as DivideExpression); }
            else if(expression is ModulusExpression) { VisitModulus(expression as ModulusExpression); }
            else if(expression is BitwiseAndExpression) { VisitBitwiseAnd(expression as BitwiseAndExpression); }
            else if(expression is BitwiseOrExpression) { VisitBitwiseOr(expression as BitwiseOrExpression); }
            else if(expression is BitwiseXorExpression) { VisitBitwiseXor(expression as BitwiseXorExpression); }
            else if(expression is BitshiftLeftExpression) { VisitBitshiftLeft(expression as BitshiftLeftExpression); }
            else if(expression is BitshiftRightExpression) { VisitBitshiftRight(expression as BitshiftRightExpression); }
            else if(expression is LowerThanExpression) { VisitLowerThan(expression as LowerThanExpression); }
            else if(expression is LowerThanEqualsExpression) { VisitLowerThanEquals(expression as LowerThanEqualsExpression); }
            else if(expression is GreaterThanExpression) { VisitGreaterThan(expression as GreaterThanExpression); }
            else if(expression is GreaterThanEqualsExpression) { VisitGreaterThanEquals(expression as GreaterThanEqualsExpression); }
            else if(expression is EqualsExpression) { VisitEquals(expression as EqualsExpression); }
            else if(expression is NotEqualExpression) { VisitNotEqual(expression as NotEqualExpression); }
            else if(expression is VariableExpression) { VisitVariable(expression as VariableExpression); }
            else if(expression is DeclareVariableExpression) { VisitDeclareVariable(expression as DeclareVariableExpression); }
            else if(expression is DeclareAssignVariableExpression) { VisitDeclareAssignVariable(expression as DeclareAssignVariableExpression); }
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
        protected virtual void VisitBoolean(BooleanExpression booleanExpression)
        {
        }

        protected virtual void VisitDeclareVariable(DeclareVariableExpression varExpression)
        {
        }
        protected virtual void VisitDeclareAssignVariable(DeclareAssignVariableExpression varAssignExpression)
        {
        }
        protected virtual void VisitVariable(VariableExpression varExpression)
        {
        }
        protected virtual void VisitInteger(IntegerExpression integerExpression)
        {
        }
        protected virtual void VisitLong(LongExpression longExpression)
        {
        }
        protected virtual void VisitFloat(FloatExpression floatExpression)
        {
        }
        protected virtual void VisitDouble(DoubleExpression doubleExpression)
        {
        }
        protected virtual void VisitAdd(AddExpression addExpression)
        {
        }
        protected virtual void VisitSubtract(SubtractExpression subExpression)
        {
        }
        protected virtual void VisitMultiply(MultiplyExpression mulExpression)
        {
        }
        protected virtual void VisitDivide(DivideExpression divExpression)
        {
        }
        protected virtual void VisitModulus(ModulusExpression modExpression)
        {
        }
        protected virtual void VisitBitwiseAnd(BitwiseAndExpression andExpression)
        {
        }
        protected virtual void VisitBitwiseOr(BitwiseOrExpression orExpression)
        {
        }
        protected virtual void VisitBitwiseXor(BitwiseXorExpression xorExpression)
        {
        }
        protected virtual void VisitBitshiftLeft(BitshiftLeftExpression bslExpression)
        {
        }
        protected virtual void VisitBitshiftRight(BitshiftRightExpression bsrExpression)
        {
        }
        protected virtual void VisitEquals(EqualsExpression eqExpression)
        {
        }
        protected virtual void VisitNotEqual(NotEqualExpression neExpression)
        {
        }
        protected virtual void VisitLowerThan(LowerThanExpression ltExpression)
        {
        }
        protected virtual void VisitLowerThanEquals(LowerThanEqualsExpression lteExpression)
        {
        }
        protected virtual void VisitGreaterThan(GreaterThanExpression gtExpression)
        {
        }
        protected virtual void VisitGreaterThanEquals(GreaterThanEqualsExpression gteExpression)
        {
        }
    }
}
