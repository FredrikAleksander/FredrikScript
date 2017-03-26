using FredrikScript.Core.Types;
using System;

namespace FredrikScript.Core.Expressions
{
    public class MultiplyExpression : BinaryExpression
    {
        public MultiplyExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
            if (leftValue.ExpressionType != rightValue.ExpressionType)
                throw new System.Exception("Mismatching operand types");
            IType type = leftValue.ExpressionType;

            if (!(type.FullyQualifiedName == "double" ||
                type.FullyQualifiedName == "char" ||
                type.FullyQualifiedName == "float" ||
                type.FullyQualifiedName == "byte" ||
                type.FullyQualifiedName == "sbyte" ||
                type.FullyQualifiedName == "short" ||
                type.FullyQualifiedName == "ushort" ||
                type.FullyQualifiedName == "int" ||
                type.FullyQualifiedName == "uint" ||
                type.FullyQualifiedName == "long" ||
                type.FullyQualifiedName == "ulong"))
                throw new Exception($"Type '{type.FullyQualifiedName}' cannot be used in a multiply expression");
        }

        public override IType ExpressionType => LeftValue.ExpressionType;
    }
}
