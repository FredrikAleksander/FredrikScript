using FredrikScript.Sandbox.TypeBuilders;
using Microsoft.FSharp.Collections;
using System;
using System.Collections.Generic;
using System.Linq;

namespace FredrikScript.Sandbox
{
    /// <summary>
    /// Enumerates all namespaces and types to provide proper global symbol resolution
    /// </summary>
    public class TypeVisitorPass1 : VisitorBase
    {
        private readonly CompilerContext _context;

        public TypeVisitorPass1(CompilerContext context)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
        }

        private void EnsureSymbolNamespace() // Throws if the current namespace conflicts with another symbol
        {
            if (_context.Types.ContainsKey(Namespace))
                throw new Exception($"Namespace conflicts with type name '{Namespace}'");
            if (!_context.Namespaces.Any(x => x == Namespace))
                _context.Namespaces.Add(Namespace);
        }

        private void AddType(TypeBuilder typeBuilder)
        {
            var fqn = typeBuilder.FullyQualifiedName;
            if (_context.Namespaces.Any(x => x == fqn))
                throw new Exception($"Type name '{fqn}' conflicts with namespace");
            if (_context.Types.ContainsKey(fqn))
                throw new Exception($"Type with type name '{fqn} already defined'");

            _context.Types[fqn] = typeBuilder;
        }

        protected override void VisitEnum(Ast.Type.Enum eenum)
        {
            EnsureSymbolNamespace();
            AddType(new EnumBuilder(_context, Namespace, eenum.Item3.Item));
            base.VisitEnum(eenum);
        }

        protected override void VisitInterface(Ast.Type.Interface iface)
        {
            EnsureSymbolNamespace();
            AddType(new InterfaceBuilder(_context, Namespace, iface.Item3.Item));
        }

        protected override void VisitStruct(Ast.Type.Struct strr)
        {
            EnsureSymbolNamespace();
            AddType(new StructBuilder(_context, Namespace, strr.Item3.Item));
        }

        protected override void VisitClass(Ast.Type.Class clazz)
        {
            EnsureSymbolNamespace();
            AddType(new ClassBuilder(_context, Namespace, clazz.Item3.Item));
        }
    }
    
    /// <summary>
    /// Enumerates all types and adds fields and generates class/interface descriptors (vtables, metadata)
    /// </summary>
    public class TypeVisitorPass2 : VisitorBase
    {
        private CompilerContext _context;

        public TypeVisitorPass2(CompilerContext context)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
        }

        protected override void VisitClass(Ast.Type.Class clazz)
        {
            var fqn = string.IsNullOrWhiteSpace(Namespace) ? clazz.Item3.Item : Namespace + "." + clazz.Item3.Item;
            var type = _context.Types[fqn] as ClassBuilder;
            SetBaseClassAndInterfaces(type, clazz.Item4);
            base.VisitClass(clazz);
        }

        private void SetBaseClassAndInterfaces(ClassBuilder builder, FSharpList<Ast.TypeName> typesAst)
        {
            var types = typesAst.Select((t) => _context.ResolveType(t.Item1, Namespace, UsingDirectives)).ToArray();
            if (types.Any(t => t.Kind != TypeKind.Interface && t.Kind != TypeKind.Class))
                throw new Exception($"Clas '{builder.FullyQualifiedName}' has invalid inheritance");
            if (types.Count(t => t.Kind == TypeKind.Class) > 1)
                throw new Exception($"Class '{builder.FullyQualifiedName}' cannot have more than one base class");
            var baseClass = types.SingleOrDefault(t => t.Kind == TypeKind.Class);
            var interfaces = types.Select(t => t.Kind == TypeKind.Interface).ToArray();
        }

        protected override void VisitField(Ast.TypeMember.Field field)
        {
            var name = CurrentClass != null ? CurrentClass.Item3.Item : CurrentStruct.Item3.Item;
            var fqn = string.IsNullOrWhiteSpace(Namespace) ? name : Namespace + "." + name;
            var type = _context.Types[fqn];
            if(type is ClassBuilder)
            {
                AddField(type as ClassBuilder, field);
            }
            else if(type is StructBuilder)
            {
                AddField(type as StructBuilder, field);
            }
        }

        protected override void VisitConstructor(Ast.TypeMember.Constructor ctor)
        {
        }

        protected override void VisitMethod(Ast.TypeMember.Method method)
        {
        }

        protected override void VisitMethodDefinition(Ast.TypeMember.MethodDefinition methodDefinition)
        {
        }

        private void AddField(ClassBuilder builder, Ast.TypeMember.Field field)
        {
        }

        private void AddField(StructBuilder builder, Ast.TypeMember.Field field)
        {
        }
    }
}