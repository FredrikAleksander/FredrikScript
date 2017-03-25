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
        
        private readonly LLVMContext _context;
        private readonly MethodBuilder _method;
        private readonly LLVMBasicBlockRef _block;
        private readonly Stack<LLVMValueRef> _valueStack = new Stack<LLVMValueRef>();

        public LLVMCodeGenVisitor(LLVMContext context, LLVMMethodBuilder methodBuilder)
        {
            _context = context;
            _block = LLVM.AppendBasicBlockInContext(context.LLVMHandle, methodBuilder.LLVMFunction, "entry");
            LLVM.PositionBuilderAtEnd(_context.LLVMBuilder, _block);
            _method = methodBuilder;
        }

        public void Apply()
        {
            Visit(_method.Body);
        }

        protected override void VisitBlock(BlockExpression blockExpression)
        {
            foreach (var expr in blockExpression.Expressions)
                Visit(expr);
        }

        protected override void VisitInteger(IntegerExpression integerExpression)
        {
            var bytes = BitConverter.GetBytes(integerExpression.Value);
            var uintValue = BitConverter.ToUInt32(bytes, 0);
            _valueStack.Push(LLVM.ConstInt((_context.Types["int"] as ILLVMType).LLVMType, uintValue, false));
        }

        protected override void VisitReturn(ReturnExpression returnExpression)
        {
            LLVM.BuildRetVoid(_context.LLVMBuilder);
        }

        protected override void VisitReturnValue(ReturnValueExpression returnValueExpression)
        {
            Visit(returnValueExpression.Value);
            LLVM.BuildRet(_context.LLVMBuilder, _valueStack.Pop());
        }
    }
}
