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

        private Stack<Dictionary<string, IType>> _localVariableStack = new Stack<Dictionary<string, IType>>();
        private Stack<Expression> _expressionStack = new Stack<Expression>();

        private SourceInformation GetSourceInformation(Ast.ContextInfo ci)
        {
            return new SourceInformation(ci.StreamName, ci.Line, ci.Column);
        }

        private IType GetLocalVariable(string name)
        {
            foreach(var scope in _localVariableStack.Reverse())
            {
                if (scope.ContainsKey(name))
                    return scope[name];
            }
            return null;
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
                case Ast.Expression.Tags.Add:
                    var addExpr = expr as Ast.Expression.Add;
                    VisitExpression(addExpr.Item3);
                    VisitExpression(addExpr.Item2);
                    var lhs = _expressionStack.Pop();
                    var rhs = _expressionStack.Pop();
                    _expressionStack.Push(new AddExpression(_context, GetSourceInformation(addExpr.Item1), lhs, rhs));
                    break;
                case Ast.Expression.Tags.Subtract:
                    var subExpr = expr as Ast.Expression.Subtract;
                    VisitExpression(subExpr.Item3);
                    VisitExpression(subExpr.Item2);
                    _expressionStack.Push(new SubtractExpression(_context, GetSourceInformation(subExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.Multiply:
                    var mulExpr = expr as Ast.Expression.Multiply;
                    VisitExpression(mulExpr.Item3);
                    VisitExpression(mulExpr.Item2);
                    _expressionStack.Push(new MultiplyExpression(_context, GetSourceInformation(mulExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.Divide:
                    var divExpr = expr as Ast.Expression.Divide;
                    VisitExpression(divExpr.Item3);
                    VisitExpression(divExpr.Item2);
                    _expressionStack.Push(new DivideExpression(_context, GetSourceInformation(divExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.Modulus:
                    var modExpr = expr as Ast.Expression.Modulus;
                    VisitExpression(modExpr.Item3);
                    VisitExpression(modExpr.Item2);
                    _expressionStack.Push(new ModulusExpression(_context, GetSourceInformation(modExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.BitwiseAnd:
                    var bandExpr = expr as Ast.Expression.BitwiseAnd;
                    VisitExpression(bandExpr.Item3);
                    VisitExpression(bandExpr.Item2);
                    _expressionStack.Push(new BitwiseAndExpression(_context, GetSourceInformation(bandExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.BitwiseOr:
                    var borExpr = expr as Ast.Expression.BitwiseOr;
                    VisitExpression(borExpr.Item3);
                    VisitExpression(borExpr.Item2);
                    _expressionStack.Push(new BitwiseOrExpression(_context, GetSourceInformation(borExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.BitwiseXor:
                    var bxorExpr = expr as Ast.Expression.BitwiseXor;
                    VisitExpression(bxorExpr.Item3);
                    VisitExpression(bxorExpr.Item2);
                    _expressionStack.Push(new BitwiseXorExpression(_context, GetSourceInformation(bxorExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.BitshiftLeft:
                    var bslExpr = expr as Ast.Expression.BitshiftLeft;
                    VisitExpression(bslExpr.Item3);
                    VisitExpression(bslExpr.Item2);
                    _expressionStack.Push(new BitshiftLeftExpression(_context, GetSourceInformation(bslExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.BitshiftRight:
                    var bsrExpr = expr as Ast.Expression.BitshiftRight;
                    VisitExpression(bsrExpr.Item3);
                    VisitExpression(bsrExpr.Item2);
                    _expressionStack.Push(new BitshiftRightExpression(_context, GetSourceInformation(bsrExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.Equals:
                    var equalsExpr = expr as Ast.Expression.Equals;
                    VisitExpression(equalsExpr.Item3);
                    VisitExpression(equalsExpr.Item2);
                    _expressionStack.Push(new EqualsExpression(_context, GetSourceInformation(equalsExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.NotEqual:
                    var neExpr = expr as Ast.Expression.NotEqual;
                    VisitExpression(neExpr.Item3);
                    VisitExpression(neExpr.Item2);
                    _expressionStack.Push(new NotEqualExpression(_context, GetSourceInformation(neExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.Or:
                    var lorExpr = expr as Ast.Expression.Or;
                    VisitExpression(lorExpr.Item3);
                    VisitExpression(lorExpr.Item2);
                    _expressionStack.Push(new OrExpression(_context, GetSourceInformation(lorExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.And:
                    var landExpr = expr as Ast.Expression.And;
                    VisitExpression(landExpr.Item3);
                    VisitExpression(landExpr.Item2);
                    _expressionStack.Push(new AndExpression(_context, GetSourceInformation(landExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.LowerThan:
                    var ltExpr = expr as Ast.Expression.LowerThan;
                    VisitExpression(ltExpr.Item3);
                    VisitExpression(ltExpr.Item2);
                    _expressionStack.Push(new LowerThanExpression(_context, GetSourceInformation(ltExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.LowerThanEquals:
                    var lteExpr = expr as Ast.Expression.LowerThanEquals;
                    VisitExpression(lteExpr.Item3);
                    VisitExpression(lteExpr.Item2);
                    _expressionStack.Push(new LowerThanEqualsExpression(_context, GetSourceInformation(lteExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.GreaterThan:
                    var gtExpr = expr as Ast.Expression.GreaterThan;
                    VisitExpression(gtExpr.Item3);
                    VisitExpression(gtExpr.Item2);
                    _expressionStack.Push(new GreaterThanExpression(_context, GetSourceInformation(gtExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.GreaterThanEquals:
                    var gteExpr = expr as Ast.Expression.GreaterThanEquals;
                    VisitExpression(gteExpr.Item3);
                    VisitExpression(gteExpr.Item2);
                    _expressionStack.Push(new GreaterThanEqualsExpression(_context, GetSourceInformation(gteExpr.Item1), _expressionStack.Pop(), _expressionStack.Pop()));
                    break;
                case Ast.Expression.Tags.Symbol:
                    var symExpr = expr as Ast.Expression.Symbol;
                    var localVar = GetLocalVariable(symExpr.Item2);
                    if (localVar != null)
                    {
                        _expressionStack.Push(new VariableExpression(_context, GetSourceInformation(symExpr.Item1), symExpr.Item2, localVar));
                    }
                    else
                        throw new NotImplementedException();
                    break;
            }
        }

        private bool IsLocalVariable(string varName)
        {
            foreach(var varScope in _localVariableStack)
            {
                if(varScope.ContainsKey(varName))
                {
                    return true;
                }
            }
            throw new NotImplementedException();
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
                    var results = _expressionStack.Reverse().ToArray();
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
                    var statementExpressions = _expressionStack.Reverse().ToArray();
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
                    var varStatement = statement as Ast.Statement.VariableDeclaration;
                    Ast.TypeName varTypeName = null;
                    IType varType = null; ;

                    if (Utilities.TryGetValue(varStatement.Item3, out varTypeName))
                        varType = _context.ResolveType(varTypeName.Item1, Namespace, UsingDirectives);

                    if (Utilities.TryGetValue(varStatement.Item4, out var initExpression))
                    {
                        VisitExpression(initExpression);
                        varType = _expressionStack.Peek().ExpressionType;
                        _expressionStack.Push(new DeclareAssignVariableExpression(_context, GetSourceInformation(varStatement.Item1), varStatement.Item2, varType, _expressionStack.Pop()));
                    }
                    else
                        if (varType != null)
                        _expressionStack.Push(new DeclareVariableExpression(_context, GetSourceInformation(varStatement.Item1), varStatement.Item2, varType));
                    else
                        throw new Exception("Variable declaration must have either a type annotation or a initializer");

                    _localVariableStack.Peek().Add(varStatement.Item2, varType);

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

            _localVariableStack.Push(new Dictionary<string, IType>());
            for(int i = 0; i < parameterTypes.Length; i++)
                _localVariableStack.Peek().Add(method.Item5[i].Item2.Item, parameterTypes[i]);
            foreach(var statement in method.Item7)
                VisitStatement(statement);
            _localVariableStack.Pop();

            var results = _expressionStack.Reverse().ToArray();
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
