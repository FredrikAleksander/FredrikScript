using System;
using System.Collections.Generic;

namespace FredrikScript.Sandbox
{
    public class VisitorBase : IVisitor
    {
        private List<string> _usingDirectives = new List<string>();
        private Stack<string> _namespaceStack = new Stack<string>();
        private Ast.CompilationUnit _currentCompilationUnit;
        private Ast.Type _currentType;
        private Ast.Type.Class _currentClass;
        private Ast.Type.Enum _currentEnum;
        private Ast.Type.Interface _currentInterface;
        private Ast.Type.Struct _currentStruct;
        private string _moduleId;

        public IEnumerable<string> UsingDirectives => _usingDirectives;
        public Ast.Type CurrentType => _currentType;
        public string Namespace => string.Join(".", _namespaceStack.ToArray());
        public Ast.Type.Class CurrentClass => _currentClass;
        public Ast.Type.Enum CurrentEnum => _currentEnum;
        public Ast.Type.Interface CurrentInterface => _currentInterface;
        public Ast.Type.Struct CurrentStruct => _currentStruct;

        public virtual void VisitCompilationUnit(Ast.CompilationUnit compilationUnit)
        {
            _currentCompilationUnit = compilationUnit;
            _usingDirectives.Clear();
            _namespaceStack.Clear();

            foreach (var usingDirective in compilationUnit.Item2)
                VisitUsingDirective(usingDirective);
            foreach (var tld in compilationUnit.Item3)
                switch(tld.Tag)
                {
                    case Ast.TopLevelDeclaration.Tags.Namespace:
                        VisitNamespace(tld as Ast.TopLevelDeclaration.Namespace);
                        break;
                    case Ast.TopLevelDeclaration.Tags.Type:
                        VisitType((tld as Ast.TopLevelDeclaration.Type).Item2);
                        break;
                }
        }
        protected virtual void VisitUsingDirective(Ast.UsingDirective usingDirective)
        {
            _usingDirectives.Add(usingDirective.Item2);
        }
        protected virtual void VisitNamespace(Ast.TopLevelDeclaration.Namespace ns)
        {
            _namespaceStack.Push(ns.Item2.Item);
            foreach(var item in ns.Item3)
            {
                switch(item.Tag)
                {
                    case Ast.TopLevelDeclaration.Tags.Namespace:
                        VisitNamespace(item as Ast.TopLevelDeclaration.Namespace);
                        break;
                    case Ast.TopLevelDeclaration.Tags.Type:
                        VisitType((item as Ast.TopLevelDeclaration.Type).Item2);
                        break;
                }
            }
            _namespaceStack.Pop();
        }
        private void VisitType(Ast.Type type)
        {
            _currentType = type;
            switch(type.Tag)
            {
                case Ast.Type.Tags.Enum:
                    VisitEnum(type as Ast.Type.Enum);
                    break;
                case Ast.Type.Tags.Interface:
                    VisitInterface(type as Ast.Type.Interface);
                    break;
                case Ast.Type.Tags.Struct:
                    VisitStruct(type as Ast.Type.Struct);
                    break;
                case Ast.Type.Tags.Class:
                    VisitClass(type as Ast.Type.Class);
                    break;
            }
            _currentType = null;
        }
        protected virtual void VisitEnum(Ast.Type.Enum eenum)
        {
            _currentEnum = eenum;
            foreach (var value in eenum.Item4)
                VisitEnumValue(value);
            _currentEnum = null;
        }

        protected virtual void VisitInterface(Ast.Type.Interface iface)
        {
            _currentInterface = iface;
            foreach(var member in iface.Item5)
            {
                VisitInterfaceMember(member);
            }
            _currentInterface = null;
        }

        protected virtual void VisitStruct(Ast.Type.Struct strct)
        {
            _currentStruct = strct;
            foreach(var member in strct.Item4)
            {
                switch(member.Tag)
                {
                    case Ast.TypeMember.Tags.Constructor:
                        VisitConstructor(member as Ast.TypeMember.Constructor);
                        break;
                    case Ast.TypeMember.Tags.Field:
                        VisitField(member as Ast.TypeMember.Field);
                        break;
                    case Ast.TypeMember.Tags.Method:
                        VisitMethod(member as Ast.TypeMember.Method);
                        break;
                    case Ast.TypeMember.Tags.MethodDefinition:
                        VisitMethodDefinition(member as Ast.TypeMember.MethodDefinition);
                        break;
                }
            }
            _currentStruct = null;
        }

        protected virtual void VisitClass(Ast.Type.Class clazz)
        {
            _currentClass = clazz;
            foreach(var member in clazz.Item5)
            {
                switch (member.Tag)
                {
                    case Ast.TypeMember.Tags.Constructor:
                        VisitConstructor(member as Ast.TypeMember.Constructor);
                        break;
                    case Ast.TypeMember.Tags.Field:
                        VisitField(member as Ast.TypeMember.Field);
                        break;
                    case Ast.TypeMember.Tags.Method:
                        VisitMethod(member as Ast.TypeMember.Method);
                        break;
                    case Ast.TypeMember.Tags.MethodDefinition:
                        VisitMethodDefinition(member as Ast.TypeMember.MethodDefinition);
                        break;
                }
            }
            _currentClass = null;
        }

        protected virtual void VisitEnumValue(Ast.EnumValue value)
        {
        }

        protected virtual void VisitInterfaceMember(Ast.InterfaceMember member)
        {
        }

        protected virtual void VisitField(Ast.TypeMember.Field field)
        {
        }

        protected virtual void VisitMethod(Ast.TypeMember.Method method)
        {
        }

        protected virtual void VisitMethodDefinition(Ast.TypeMember.MethodDefinition methodDefinition)
        {
        }

        protected virtual void VisitConstructor(Ast.TypeMember.Constructor ctor)
        {
        }
    }
}
