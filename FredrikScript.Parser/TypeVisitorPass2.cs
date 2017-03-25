using FredrikScript.Core;
using FredrikScript.Core.Types;
using FredrikScript.ParserFS;
using Microsoft.FSharp.Collections;
using System;
using System.Linq;

namespace FredrikScript.Parser
{
    /// <summary>
    /// Enumerates all types and forward declares all methods.
    /// </summary>
    public class TypeVisitorPass2 : CompilerVisitorBase
    {
        public TypeVisitorPass2(Context context) : base(context)
        {
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
                throw new Exception($"Class '{builder.FullyQualifiedName}' has invalid inheritance");
            if (types.Count(t => t.Kind == TypeKind.Class) > 1)
                throw new Exception($"Class '{builder.FullyQualifiedName}' cannot have more than one base class");
            var baseClass = types.SingleOrDefault(t => t.Kind == TypeKind.Class) ?? (builder.FullyQualifiedName != "object" ? _context.ResolveTypeFullyQualified("object") : null);
            if(baseClass != null && !(baseClass is ClassBuilder))
                throw new Exception($"'{baseClass.FullyQualifiedName}' is not a class type");
            var interfaces = types.Where(t =>
            {
                if (t is InterfaceBuilder r)
                    return true;
                if (!(t is ClassBuilder))
                    throw new Exception($"Type '{t.FullyQualifiedName}' is not a interface");
                return false;
            }).Cast<InterfaceBuilder>().ToArray();
            builder.SetBaseClassAndImplementations(baseClass as ClassBuilder, interfaces);
        }

        protected override void VisitStructField(Ast.TypeMember.Field field)
        {
            var name = CurrentStruct.Item3.Item;
            var fqn = string.IsNullOrWhiteSpace(Namespace) ? name : Namespace + "." + name;
            var type = _context.Types[fqn];
            AddField(type as StructBuilder, field);
        }

        protected override void VisitClassField(Ast.TypeMember.Field field)
        {
            var name = CurrentClass.Item3.Item;
            var fqn = string.IsNullOrWhiteSpace(Namespace) ? name : Namespace + "." + name;
            var type = _context.Types[fqn];
            AddField(type as ClassBuilder, field);
        }

        protected override void VisitClassConstructor(Ast.TypeMember.Constructor ctor)
        {
        }

        protected override void VisitStructConstructor(Ast.TypeMember.Constructor ctor)
        {
        }

        private void VisitMethod(Ast.TypeMember.Method method, string n, string ns)
        {
            var declaringTypeName = n;
            var declaringTypeNamespace = Namespace;
            var declaringTypeFqn = string.IsNullOrWhiteSpace(declaringTypeNamespace) ? declaringTypeName : declaringTypeNamespace + "." + declaringTypeName;
            var declaringType = _context.Types[declaringTypeFqn];
            var returnType = ResolveType(method.Item6);
            var parameterTypes = method.Item5.Select(x => ResolveType(x.Item3)).ToArray();
            var parameterNames = parameterTypes.Select(x => x.Name).ToArray();
            var parameterNamesMangled = string.Join(",", parameterTypes.Select(parameterType => parameterType.FullyQualifiedName));
            var visibility = VisibilityFromAst(method.Item2);
            var storageClass = StorageClassFromAst(method.Item3);
            var separator = storageClass == StorageClass.Instance ? "::" : ".";
            var methodName = $"{declaringTypeFqn}{separator}{method.Item4.Item}({parameterNamesMangled})";
            string externString = method.Item3.IsExtern ? (method.Item3 as Ast.StorageClass.Extern).Item : null;
            var module = _context.ModuleBuilder;
            //_context.CreateMethod(methodName, new MethodBuilder(module, method.Item4.Item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, externString));
            //var methodBuilder = _context.CreateMethod(module, method.Item4.Item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString);
            var classBuilder = declaringType as ClassBuilder;
            var structBuilder = declaringType as StructBuilder;
            MethodBuilder methodBuilder = null;
            if (classBuilder != null)
                methodBuilder = classBuilder.AddMethod(GetSourceInformation(method.Item1), module, method.Item4.Item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString);
            else if (structBuilder != null)
                methodBuilder = structBuilder.AddMethod(GetSourceInformation(method.Item1), module, method.Item4.Item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString);
        }

        private SourceInformation GetSourceInformation(Ast.ContextInfo ci)
        {
            return new SourceInformation(ci.StreamName, ci.Line, ci.Column);
        }

        protected override void VisitClassMethod(Ast.TypeMember.Method method)
        {
            VisitMethod(method, CurrentClass.Item3.Item, Namespace);
        }

        protected override void VisitStructMethod(Ast.TypeMember.Method method)
        {
            VisitMethod(method, CurrentStruct.Item3.Item, Namespace);
        }

        protected override void VisitClassMethodDefinition(Ast.TypeMember.MethodDefinition methodDefinition)
        {
        }

        protected override void VisitStructMethodDefinition(Ast.TypeMember.MethodDefinition methodDefinition)
        {
        }

        private void AddField(ClassBuilder builder, Ast.TypeMember.Field field)
        {
            var module = _context.ModuleBuilder;
            var visibility = VisibilityFromAst(field.Item2);
            var storageClass = StorageClassFromAst(field.Item3);
            builder.AddField(GetSourceInformation(field.Item1), module, field.Item5.Item, visibility, storageClass, ResolveType(field.Item4));
        }

        private void AddField(StructBuilder builder, Ast.TypeMember.Field field)
        {
        }
    }
}