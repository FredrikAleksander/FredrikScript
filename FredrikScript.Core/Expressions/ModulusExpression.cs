using FredrikScript.Core.Types;
using System;

namespace FredrikScript.Core.Expressions
{
    public class ModulusExpression : BinaryExpression
    {
        public ModulusExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
            if (leftValue.ExpressionType != rightValue.ExpressionType)
                throw new System.Exception("Mismatching operand types");
            IType type = leftValue.ExpressionType;

            if (!(type.FullyQualifiedName == "char" ||
                type.FullyQualifiedName == "byte" ||
                type.FullyQualifiedName == "sbyte" ||
                type.FullyQualifiedName == "short" ||
                type.FullyQualifiedName == "ushort" ||
                type.FullyQualifiedName == "int" ||
                type.FullyQualifiedName == "uint" ||
                type.FullyQualifiedName == "long" ||
                type.FullyQualifiedName == "ulong"))
                throw new Exception($"Type '{type.FullyQualifiedName}' cannot be used in a modulus expression");
        }

        public override IType ExpressionType => LeftValue.ExpressionType;
    }
}
