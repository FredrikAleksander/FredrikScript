using FredrikScript.Core;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Expressions;
using FredrikScript.LLVMCodeGen.Types;
using FredrikScript.Core.Types;

namespace FredrikScript.LLVMCodeGen
{
    public class LLVMCodeGenVisitor : CodeGenVisitor
    {
        protected class LocalVariable
        {
            private readonly string _name;
            private readonly ILLVMType _type;
            private readonly LLVMValueRef _alloca;
            private bool _initialized;

            public LocalVariable(string name, ILLVMType type, LLVMValueRef alloca)
            {
                _name = name;
                _type = type;
                _alloca = alloca;
            }

            public string Name => _name;
            public ILLVMType Type => _type;
            public bool Initialized => _initialized;
            public LLVMValueRef Alloca => _alloca;

            public void SetInitialized()
            {
                _initialized = true;
            }
        }
        private readonly Stack<Dictionary<string, LocalVariable>> _localVariableStack = new Stack<Dictionary<string, LocalVariable>>();
        private readonly LLVMContext _context;
        private readonly MethodBuilder _method;
        private readonly LLVMBasicBlockRef _block;
        private readonly Stack<LLVMValueRef> _valueStack = new Stack<LLVMValueRef>();

        public LLVMCodeGenVisitor(LLVMContext context, LLVMMethodBuilder methodBuilder)
        {
            _context = context;
            _block = LLVM.AppendBasicBlockInContext(context.LLVMHandle, methodBuilder.LLVMFunction, "entry");
            _method = methodBuilder;

        }

        protected virtual LocalVariable DefineVariable(string name, ILLVMType type)
        {
            var scopeVars = _localVariableStack.Peek();
            if (scopeVars.ContainsKey(name))
                throw new InvalidOperationException($"Variable '{name}' is already defined in scope");
            var builder = LLVM.CreateBuilderInContext(_context.LLVMHandle);
            LLVM.PositionBuilder(builder, _block, LLVM.GetFirstInstruction(_block));
            var allocaValue = LLVM.BuildAlloca(builder, type.LLVMType, name);
            LLVM.DisposeBuilder(builder);
            var localVar = new LocalVariable(name, type, allocaValue);
            scopeVars.Add(name, localVar);
            return localVar;
        }

        public void Apply()
        {
            LLVM.PositionBuilderAtEnd(_context.LLVMBuilder, _block);
            Dictionary<string, LocalVariable> argumentVariables = new Dictionary<string, LocalVariable>();
            _localVariableStack.Push(argumentVariables);
            for(int i = 0; i < _method.ParameterTypes.Length; i++)
            {
                var localVar = DefineVariable(_method.ParameterNames[i], _method.ParameterTypes[i] as ILLVMType);
                LLVM.BuildStore(_context.LLVMBuilder, LLVM.GetParam((_method as LLVMMethodBuilder).LLVMFunction, (uint)i), localVar.Alloca);
                localVar.SetInitialized();
            }
            Visit(_method.Body);
            _localVariableStack.Pop();
        }

        private LocalVariable GetLocalVariable(string name)
        {
            foreach(var scope in _localVariableStack.Reverse())
            {
                if (scope.ContainsKey(name))
                    return scope[name];
            }
            throw new KeyNotFoundException($"No local variable with name {name}");
        }

        protected override void VisitVariable(VariableExpression varExpression)
        {
            base.VisitVariable(varExpression);
            var localVar = GetLocalVariable(varExpression.Name);
            if (!localVar.Initialized)
                throw new Exception($"Local variable '{varExpression.Name}' is not initialized");
            _valueStack.Push(LLVM.BuildLoad(_context.LLVMBuilder, localVar.Alloca, varExpression.Name));
        }

        protected override void VisitBlock(BlockExpression blockExpression)
        {
            base.VisitBlock(blockExpression);
            foreach (var expr in blockExpression.Expressions)
                Visit(expr);
        }

        protected override void VisitBoolean(BooleanExpression booleanExpression)
        {
            base.VisitBoolean(booleanExpression);
            _valueStack.Push(LLVM.ConstInt((_context.Types["bool"] as ILLVMType).LLVMType, booleanExpression.Value ? 1UL : 0UL, false));
        }

        protected override void VisitInteger(IntegerExpression integerExpression)
        {
            base.VisitInteger(integerExpression);
            var bytes = BitConverter.GetBytes(integerExpression.Value);
            var uintValue = BitConverter.ToUInt32(bytes, 0);
            _valueStack.Push(LLVM.ConstInt((_context.Types["int"] as ILLVMType).LLVMType, uintValue, false));
        }

        protected override void VisitLong(LongExpression longExpression)
        {
            base.VisitLong(longExpression);
            var bytes = BitConverter.GetBytes(longExpression.Value);
            var uintValue = BitConverter.ToUInt64(bytes, 0);
            _valueStack.Push(LLVM.ConstInt((_context.Types["long"] as ILLVMType).LLVMType, uintValue, false));
        }

        protected override void VisitFloat(FloatExpression floatExpression)
        {
            base.VisitFloat(floatExpression);
            _valueStack.Push(LLVM.ConstReal((_context.Types["float"] as ILLVMType).LLVMType, floatExpression.Value));
        }

        protected override void VisitDouble(DoubleExpression doubleExpression)
        {
            base.VisitDouble(doubleExpression);
            _valueStack.Push(LLVM.ConstReal((_context.Types["double"] as ILLVMType).LLVMType, doubleExpression.Value));
        }

        protected override void VisitReturn(ReturnExpression returnExpression)
        {
            base.VisitReturn(returnExpression);
            LLVM.BuildRetVoid(_context.LLVMBuilder);
        }

        protected override void VisitReturnValue(ReturnValueExpression returnValueExpression)
        {
            base.VisitReturnValue(returnValueExpression);
            Visit(returnValueExpression.Value);
            LLVM.BuildRet(_context.LLVMBuilder, _valueStack.Pop());
        }

        protected override void VisitAdd(AddExpression addExpression)
        {
            base.VisitAdd(addExpression);
            Visit(addExpression.RightValue);
            Visit(addExpression.LeftValue);
            var fqn = addExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildAdd(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFAdd(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitBitwiseAnd(BitwiseAndExpression andExpression)
        {
            base.VisitBitwiseAnd(andExpression);
            Visit(andExpression.RightValue);
            Visit(andExpression.LeftValue);
            var fqn = andExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildAnd(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
            
        }

        protected override void VisitBitwiseOr(BitwiseOrExpression orExpression)
        {
            base.VisitBitwiseOr(orExpression);
            Visit(orExpression.RightValue);
            Visit(orExpression.LeftValue);
            var fqn = orExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildOr(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitBitwiseXor(BitwiseXorExpression xorExpression)
        {
            base.VisitBitwiseXor(xorExpression);
            Visit(xorExpression.RightValue);
            Visit(xorExpression.LeftValue);
            var fqn = xorExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildXor(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitDivide(DivideExpression divExpression)
        {
            base.VisitDivide(divExpression);
            Visit(divExpression.RightValue);
            Visit(divExpression.LeftValue);

            var fqn = divExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long")
                _valueStack.Push(LLVM.BuildSDiv(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildUDiv(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFDiv(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitModulus(ModulusExpression modExpression)
        {
            base.VisitModulus(modExpression);
            Visit(modExpression.RightValue);
            Visit(modExpression.LeftValue);

            var fqn = modExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long")
                _valueStack.Push(LLVM.BuildSRem(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildURem(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFRem(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitMultiply(MultiplyExpression mulExpression)
        {
            base.VisitMultiply(mulExpression);
            Visit(mulExpression.RightValue);
            Visit(mulExpression.LeftValue);

            var fqn = mulExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildMul(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFMul(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitSubtract(SubtractExpression subExpression)
        {
            base.VisitSubtract(subExpression);
            Visit(subExpression.RightValue);
            Visit(subExpression.LeftValue);
            var fqn = subExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildSub(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFMul(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitBitshiftLeft(BitshiftLeftExpression bslExpression)
        {
            base.VisitBitshiftLeft(bslExpression);
            Visit(bslExpression.RightValue);
            Visit(bslExpression.LeftValue);
            var fqn = bslExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildShl(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitBitshiftRight(BitshiftRightExpression bsrExpression)
        {
            base.VisitBitshiftRight(bsrExpression);
            Visit(bsrExpression.RightValue);
            Visit(bsrExpression.LeftValue);
            var fqn = bsrExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildAShr(_context.LLVMBuilder, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitEquals(EqualsExpression eqExpression)
        {
            base.VisitEquals(eqExpression);
            Visit(eqExpression.RightValue);
            Visit(eqExpression.LeftValue);
            var fqn = eqExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong" || eqExpression.LeftValue.ExpressionType is EnumBuilder)
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntEQ, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFCmp(_context.LLVMBuilder, LLVMRealPredicate.LLVMRealOEQ, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (eqExpression.LeftValue.ExpressionType is StructBuilder)
            {
                throw new NotImplementedException();
            }
            else if (eqExpression.LeftValue.ExpressionType is ClassBuilder)
            {
                throw new NotImplementedException();
            }
            else if (eqExpression.LeftValue.ExpressionType is InterfaceBuilder)
            {
                throw new NotImplementedException();
            }
            else
                throw new InvalidOperationException();
        }

        protected override void VisitGreaterThan(GreaterThanExpression gtExpression)
        {
            base.VisitGreaterThan(gtExpression);
            Visit(gtExpression.RightValue);
            Visit(gtExpression.LeftValue);
            var fqn = gtExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                gtExpression.LeftValue.ExpressionType is EnumBuilder)
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntSGT, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntUGT, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFCmp(_context.LLVMBuilder, LLVMRealPredicate.LLVMRealOGT, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitGreaterThanEquals(GreaterThanEqualsExpression gteExpression)
        {
            base.VisitGreaterThanEquals(gteExpression);
            Visit(gteExpression.RightValue);
            Visit(gteExpression.LeftValue);
            var fqn = gteExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                gteExpression.LeftValue.ExpressionType is EnumBuilder)
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntSGE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntUGE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFCmp(_context.LLVMBuilder, LLVMRealPredicate.LLVMRealOGE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitLowerThan(LowerThanExpression ltExpression)
        {
            base.VisitLowerThan(ltExpression);
            Visit(ltExpression.RightValue);
            Visit(ltExpression.LeftValue);
            var fqn = ltExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                ltExpression.LeftValue.ExpressionType is EnumBuilder)
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntSLT, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntULT, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFCmp(_context.LLVMBuilder, LLVMRealPredicate.LLVMRealOLT, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitLowerThanEquals(LowerThanEqualsExpression lteExpression)
        {
            base.VisitLowerThanEquals(lteExpression);
            Visit(lteExpression.RightValue);
            Visit(lteExpression.LeftValue);
            var fqn = lteExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                lteExpression.LeftValue.ExpressionType is EnumBuilder)
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntSLE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong")
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntULE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFCmp(_context.LLVMBuilder, LLVMRealPredicate.LLVMRealOLE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else
                throw new InvalidOperationException();
        }

        protected override void VisitNotEqual(NotEqualExpression neExpression)
        {
            base.VisitNotEqual(neExpression);
            Visit(neExpression.RightValue);
            Visit(neExpression.LeftValue);
            var fqn = neExpression.LeftValue.ExpressionType.FullyQualifiedName;
            if (fqn == "sbyte" || fqn == "short" || fqn == "int" || fqn == "long" ||
                fqn == "byte" || fqn == "ushort" || fqn == "uint" || fqn == "ulong" || neExpression.LeftValue.ExpressionType is EnumBuilder)
                _valueStack.Push(LLVM.BuildICmp(_context.LLVMBuilder, LLVMIntPredicate.LLVMIntNE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (fqn == "float" || fqn == "double")
                _valueStack.Push(LLVM.BuildFCmp(_context.LLVMBuilder, LLVMRealPredicate.LLVMRealONE, _valueStack.Pop(), _valueStack.Pop(), ""));
            else if (neExpression.LeftValue.ExpressionType is StructBuilder)
            {
                throw new NotImplementedException();
            }
            else if (neExpression.LeftValue.ExpressionType is ClassBuilder)
            {
                throw new NotImplementedException();
            }
            else if (neExpression.LeftValue.ExpressionType is InterfaceBuilder)
            {
                throw new NotImplementedException();
            }
            else
                throw new InvalidOperationException();
        }

        protected override void VisitDeclareVariable(DeclareVariableExpression varExpression)
        {
            base.VisitDeclareVariable(varExpression);
            DefineVariable(varExpression.Name, varExpression.Type as ILLVMType);
        }

        protected override void VisitDeclareAssignVariable(DeclareAssignVariableExpression varAssignExpression)
        {
            base.VisitDeclareAssignVariable(varAssignExpression);
            Visit(varAssignExpression.InitExpression);
            var initValue = _valueStack.Pop();
            var localVar = DefineVariable(varAssignExpression.Name, varAssignExpression.Type as ILLVMType);
            _valueStack.Push(LLVM.BuildStore(_context.LLVMBuilder, initValue, localVar.Alloca));
            localVar.SetInitialized();
        }
    }
}
