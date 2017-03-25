using FredrikScript.Core.Types;
using FredrikScript.ParserFS;
using System;
using System.Collections.Generic;

namespace FredrikScript.Parser
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
        private Ast.EnumValue _currentEnumValue;
        private Ast.InterfaceMember _currentInterfaceMember;
        private Ast.TypeMember _currentTypeMember;
        private string _moduleId;

        public IEnumerable<string> UsingDirectives => _usingDirectives;
        public Ast.Type CurrentType => _currentType;
        public string Namespace => string.Join(".", _namespaceStack.ToArray());
        public Ast.CompilationUnit CurrentCompilationUnit => _currentCompilationUnit;
        public Ast.Type.Class CurrentClass => _currentClass;
        public Ast.Type.Enum CurrentEnum => _currentEnum;
        public Ast.Type.Interface CurrentInterface => _currentInterface;
        public Ast.Type.Struct CurrentStruct => _currentStruct;

        public Visibility VisibilityFromAst(Ast.AccessModifier am)
        {
            switch(am.Tag)
            {
                case Ast.AccessModifier.Tags.Public: return Visibility.Public;
                case Ast.AccessModifier.Tags.Private: return Visibility.Private;
                case Ast.AccessModifier.Tags.Protected: return Visibility.Protected;
                case Ast.AccessModifier.Tags.Internal: return Visibility.Internal;
            }
            return Visibility.Private;
        }
        public StorageClass StorageClassFromAst(Ast.StorageClass sc)
        {
            switch(sc.Tag)
            {
                case Ast.StorageClass.Tags.Instance: return StorageClass.Instance;
                case Ast.StorageClass.Tags.Static: return StorageClass.Static;
                case Ast.StorageClass.Tags.Extern: return StorageClass.Extern;

            }
            return StorageClass.Instance;
        }

        public void Visit(Ast.CompilationUnit cu)
        {
            EnterCompilationUnit(cu);
            VisitCompilationUnit(cu);
            ExitCompilationUnit(cu);
        }


        private void EnterCompilationUnit(Ast.CompilationUnit cu)
        {
            _currentCompilationUnit = cu;
            _usingDirectives.Clear();
            _namespaceStack.Clear();
        }
        private void ExitCompilationUnit(Ast.CompilationUnit cu)
        {
            _usingDirectives.Clear();
            _namespaceStack.Clear();
            _currentCompilationUnit = null;
        }

        private void EnterNamespace(Ast.TopLevelDeclaration.Namespace ns)
        {
            _namespaceStack.Push(ns.Item2.Item);
        }
        private void ExitNamespace(Ast.TopLevelDeclaration.Namespace ns)
        {
            _namespaceStack.Pop();
        }

        private void EnterType(Ast.Type type)
        {
            _currentType = type;
        }

        private void ExitType(Ast.Type type)
        {
            _currentType = null;
        }

        private void EnterEnum(Ast.Type.Enum en)
        {
            _currentEnum = en;
        }

        private void ExitEnum(Ast.Type.Enum en)
        {
            _currentEnum = null;
        }

        private void EnterInterface(Ast.Type.Interface ifc)
        {
            _currentInterface = ifc;
        }

        private void ExitInterface(Ast.Type.Interface ifc)
        {
            _currentInterface = null;
        }

        private void EnterInterfaceMember(Ast.InterfaceMember mbr)
        {
            _currentInterfaceMember = mbr;
        }

        private void ExitInterfaceMember(Ast.InterfaceMember mbr)
        {
            _currentInterfaceMember = null;
        }

        private void EnterStruct(Ast.Type.Struct str)
        {
            _currentStruct = str;
        }

        private void ExitStruct(Ast.Type.Struct str)
        {
            _currentStruct = null;
        }

        private void EnterClass(Ast.Type.Class clazz)
        {
            _currentClass = clazz;
        }

        private void ExitClass(Ast.Type.Class clazz)
        {
            _currentClass = null;
        }

        protected virtual void VisitCompilationUnit(Ast.CompilationUnit compilationUnit)
        {
            foreach (var usingDirective in compilationUnit.Item2)
                VisitUsingDirective(usingDirective);
            foreach (var tld in compilationUnit.Item3)
                switch(tld.Tag)
                {
                    case Ast.TopLevelDeclaration.Tags.Namespace:
                        var ns = tld as Ast.TopLevelDeclaration.Namespace;
                        EnterNamespace(ns);
                        VisitNamespace(ns);
                        ExitNamespace(ns);
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
            foreach(var item in ns.Item3)
            {
                switch(item.Tag)
                {
                    case Ast.TopLevelDeclaration.Tags.Namespace:
                        var _ns = item as Ast.TopLevelDeclaration.Namespace;
                        EnterNamespace(_ns);
                        VisitNamespace(_ns);
                        ExitNamespace(_ns);
                        break;
                    case Ast.TopLevelDeclaration.Tags.Type:
                        var type = (item as Ast.TopLevelDeclaration.Type).Item2;
                        EnterType(type);
                        VisitType(type);
                        ExitType(type);
                        break;
                }
            }
        }

        private void VisitType(Ast.Type type)
        {
            switch(type.Tag)
            {
                case Ast.Type.Tags.Enum:
                    var en = type as Ast.Type.Enum;
                    EnterEnum(en);
                    VisitEnum(en);
                    ExitEnum(en);
                    break;
                case Ast.Type.Tags.Interface:
                    var ifc = type as Ast.Type.Interface;
                    EnterInterface(ifc);
                    VisitInterface(ifc);
                    ExitInterface(ifc);
                    break;
                case Ast.Type.Tags.Struct:
                    var str = type as Ast.Type.Struct;
                    EnterStruct(str);
                    VisitStruct(str);
                    ExitStruct(str);
                    break;
                case Ast.Type.Tags.Class:
                    var clazz = type as Ast.Type.Class;
                    EnterClass(clazz);
                    VisitClass(clazz);
                    ExitClass(clazz);
                    break;
            }
        }

        protected virtual void VisitEnum(Ast.Type.Enum eenum)
        {
            foreach (var value in eenum.Item4)
                VisitEnumValue(value);
        }

        protected virtual void VisitInterface(Ast.Type.Interface iface)
        {
            foreach(var member in iface.Item5)
            {
                EnterInterfaceMember(member);
                VisitInterfaceMember(member);
                ExitInterfaceMember(member);
            }
        }

        protected virtual void VisitStruct(Ast.Type.Struct strct)
        {
            _currentStruct = strct;
            foreach(var member in strct.Item4)
            {
                switch(member.Tag)
                {
                    case Ast.TypeMember.Tags.Constructor:
                        VisitStructConstructor(member as Ast.TypeMember.Constructor);
                        break;
                    case Ast.TypeMember.Tags.Field:
                        VisitStructField(member as Ast.TypeMember.Field);
                        break;
                    case Ast.TypeMember.Tags.Method:
                        VisitStructMethod(member as Ast.TypeMember.Method);
                        break;
                    case Ast.TypeMember.Tags.MethodDefinition:
                        VisitStructMethodDefinition(member as Ast.TypeMember.MethodDefinition);
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
                        VisitClassConstructor(member as Ast.TypeMember.Constructor);
                        break;
                    case Ast.TypeMember.Tags.Field:
                        VisitClassField(member as Ast.TypeMember.Field);
                        break;
                    case Ast.TypeMember.Tags.Method:
                        VisitClassMethod(member as Ast.TypeMember.Method);
                        break;
                    case Ast.TypeMember.Tags.MethodDefinition:
                        VisitClassMethodDefinition(member as Ast.TypeMember.MethodDefinition);
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

        protected virtual void VisitClassField(Ast.TypeMember.Field field)
        {
        }

        protected virtual void VisitStructField(Ast.TypeMember.Field field)
        {
        }

        protected virtual void VisitClassMethod(Ast.TypeMember.Method method)
        {
        }

        protected virtual void VisitStructMethod(Ast.TypeMember.Method method)
        {
        }

        protected virtual void VisitClassMethodDefinition(Ast.TypeMember.MethodDefinition methodDefinition)
        {
        }

        protected virtual void VisitStructMethodDefinition(Ast.TypeMember.MethodDefinition methodDefinition)
        {
        }

        protected virtual void VisitClassConstructor(Ast.TypeMember.Constructor ctor)
        {
        }

        protected virtual void VisitStructConstructor(Ast.TypeMember.Constructor ctor)
        {
        }
    }
}
