using FredrikScript.Core;
using FredrikScript.Core.Expressions;
using FredrikScript.Core.Types;
using FredrikScript.ParserFS;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Parser
{
    public class ParseExpressionVisitor : CompilerVisitorBase
    {
        public ParseExpressionVisitor(Context context) : base(context)
        {
        }

        private Stack<Expression> _expressionStack = new Stack<Expression>();

        private SourceInformation GetSourceInformation(Ast.ContextInfo ci)
        {
            return new SourceInformation(ci.StreamName, ci.Line, ci.Column);
        }

        private void VisitExpression(Ast.Expression expr)
        {
            switch(expr.Tag)
            {
                case Ast.Expression.Tags.Integer:
                    var intExpr = expr as Ast.Expression.Integer;
                    _expressionStack.Push(new IntegerExpression(_context, GetSourceInformation(intExpr.Item1), intExpr.Item2));
                    break;
                case Ast.Expression.Tags.Long:
                    var longExpr = expr as Ast.Expression.Long;
                    _expressionStack.Push(new LongExpression(_context, GetSourceInformation(longExpr.Item1), longExpr.Item2));
                    break;
                case Ast.Expression.Tags.Boolean:
                    var boolExpr = expr as Ast.Expression.Boolean;
                    _expressionStack.Push(new BooleanExpression(_context, GetSourceInformation(boolExpr.Item1), boolExpr.Item2));
                    break;
                case Ast.Expression.Tags.Float:
                    var floatExpr = expr as Ast.Expression.Float;
                    _expressionStack.Push(new FloatExpression(_context, GetSourceInformation(floatExpr.Item1), floatExpr.Item2));
                    break;
                case Ast.Expression.Tags.Char:
                    var charExpr = expr as Ast.Expression.Char;
                    _expressionStack.Push(new CharExpression(_context, GetSourceInformation(charExpr.Item1), charExpr.Item2));
                    break;
                case Ast.Expression.Tags.String:
                    var strExpr = expr as Ast.Expression.String;
                    _expressionStack.Push(new StringExpression(_context, GetSourceInformation(strExpr.Item1), strExpr.Item2));
                    break;
            }
        }
        private void VisitStatement(Ast.Statement statement)
        {
            switch(statement.Tag)
            {
                case Ast.Statement.Tags.Break:
                    var breakStatement = statement as Ast.Statement.Break;
                    var breakExpr = new BreakExpression(_context, GetSourceInformation(breakStatement.Item));
                    _expressionStack.Push(breakExpr);
                    break;
                case Ast.Statement.Tags.Continue:
                    var continueStatement = statement as Ast.Statement.Continue;
                    var continueExpr = new ContinueExpression(_context, GetSourceInformation(continueStatement.Item));
                    _expressionStack.Push(continueExpr);
                    break;
                case Ast.Statement.Tags.DoWhile:
                    var doWhileStatement = statement as Ast.Statement.DoWhile;
                    var doWhileExpr = new DoWhileExpression(_context, GetSourceInformation(doWhileStatement.Item1));
                    var oldStack = _expressionStack;
                    _expressionStack = new Stack<Expression>();
                    foreach(var subStatement in doWhileStatement.Item2)
                        VisitStatement(subStatement);
                    var results = _expressionStack.ToArray();
                    _expressionStack = oldStack;
                    var blockInfo = results.Length > 0 ? results[0].SourceInformation : doWhileExpr.SourceInformation;
                    var blockExpression = new BlockExpression(_context, blockInfo, results);
                    _expressionStack.Push(doWhileExpr);
                    break;
                case Ast.Statement.Tags.Empty:
                    break;
                case Ast.Statement.Tags.Expression:
                    VisitExpression((statement as Ast.Statement.Expression).Item2);
                    break;
                case Ast.Statement.Tags.For:
                    var forStatement = statement as Ast.Statement.For;
                    VisitStatement(forStatement.Item2);
                    VisitExpression(forStatement.Item3);
                    VisitExpression(forStatement.Item4);

                    var lastExpr = _expressionStack.Pop();
                    var middleExpr = _expressionStack.Pop();
                    var firstExpr = _expressionStack.Pop();

                    var forOldStack = _expressionStack;
                    _expressionStack = new Stack<Expression>();
                    foreach (var st in forStatement.Item5)
                        VisitStatement(st);
                    var statementExpressions = _expressionStack.ToArray();
                    _expressionStack = forOldStack;
                    var forBlockExpression = new BlockExpression(_context, statementExpressions.Length > 0
                        ? statementExpressions[0].SourceInformation
                        : lastExpr.SourceInformation, statementExpressions);
                    _expressionStack.Push(new ForExpression(_context, GetSourceInformation(forStatement.Item1), firstExpr, middleExpr, lastExpr, forBlockExpression));
                    break;
                case Ast.Statement.Tags.ForEach:
                    break;
                case Ast.Statement.Tags.If:
                    break;
                case Ast.Statement.Tags.Return:
                    var returnStatement = statement as Ast.Statement.Return;
                    Expression returnExpression;
                    if (Utilities.TryGetValue(returnStatement.Item2, out var returnValueExpression)) {
                        VisitExpression(returnValueExpression);
                        returnExpression = new ReturnValueExpression(_context, GetSourceInformation(returnStatement.Item1), _expressionStack.Pop());
                    }
                    else
                        returnExpression = new ReturnExpression(_context, GetSourceInformation(returnStatement.Item1));
                    _expressionStack.Push(returnExpression);
                    break;
                case Ast.Statement.Tags.VariableDeclaration:
                    break;
                case Ast.Statement.Tags.While:
                    break;
            }
        }


        private void VisitMethod(Ast.TypeMember.Method method, string n)
        {
            _expressionStack.Clear();
            var declaringTypeName = n;
            var declaringTypeNamespace = Namespace;
            var declaringTypeFqn = string.IsNullOrWhiteSpace(Namespace) ? declaringTypeName : Namespace + "." + n;
            var declaringType = _context.Types[declaringTypeFqn];
            var parameterTypes = method.Item5.Select(x => ResolveType(x.Item3)).ToArray();
            var parameterNamesMangled = string.Join(",", parameterTypes.Select(parameterType => parameterType.FullyQualifiedName));
            var storageClass = StorageClassFromAst(method.Item3);
            var separator = storageClass == StorageClass.Instance ? "::" : ".";
            var methodName = $"{declaringTypeFqn}{separator}{method.Item4.Item}({parameterNamesMangled})";
            foreach(var statement in method.Item7)
                VisitStatement(statement);

            var results = _expressionStack.ToArray();
            var result = results.Length > 1
                        ? new BlockExpression(_context, results[0].SourceInformation, results)
                        : results.Length == 0
                        ? new BlockExpression(_context, GetSourceInformation(method.Item1), new Expression[] { })
                        : results[0];
            _expressionStack.Clear();
            if (declaringType is ClassBuilder)
                (declaringType as ClassBuilder).GetMethodFromSymbol(methodName).SetMethodBody(result);
            else
                (declaringType as StructBuilder).GetMethodFromSymbol(methodName).SetMethodBody(result);
        }

        protected override void VisitClassMethod(Ast.TypeMember.Method method)
        {
            VisitMethod(method, CurrentClass.Item3.Item);
        }

        protected override void VisitStructMethod(Ast.TypeMember.Method method)
        {
            VisitMethod(method, CurrentStruct.Item3.Item);
        }
    }
}
