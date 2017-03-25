using System;
using System.Collections.Generic;

namespace FredrikScript.Core.Types
{
    public class ClassBuilder : TypeBuilder, IType
    {
        private readonly string _namespace;
        private readonly string _name;
        private ClassBuilder _base = null;
        private readonly List<InterfaceBuilder> _interfaces = new List<InterfaceBuilder>();
        private readonly List<Field> _fields = new List<Field>();
        private readonly List<MethodBuilder> _methods = new List<MethodBuilder>();
        private readonly List<MethodBuilder> _constructors = new List<MethodBuilder>();

        public ClassBuilder(string ns, string name) : base(ns, name)
        {
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _namespace = ns ?? "";
        }

        public IReadOnlyList<Field> Fields => _fields;
        public IReadOnlyList<MethodBuilder> Methods => _methods;
        public IReadOnlyList<MethodBuilder> Constructors => _constructors;

        public override TypeKind Kind => TypeKind.Class;

        public Field AddField(SourceInformation sourceInformation, ModuleBuilder module, string name, Visibility visibility, StorageClass storageClass, IType fieldType)
        {
            var field = new Field(sourceInformation, module, name, this, fieldType, visibility, storageClass);
            _fields.Add(field);
            return field;
        }

        public MethodBuilder AddMethod(SourceInformation sourceInformation, ModuleBuilder module, string item, string methodName, Visibility visibility, StorageClass storageClass, IType declaringType, IType returnType, IType[] parameterTypes, string[] parameterNames, string externString)
        {
            var builder = CreateMethodBuilder(sourceInformation, module, item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString);
            _methods.Add(builder);
            return builder;
        }

        protected virtual MethodBuilder CreateMethodBuilder(SourceInformation sourceInformation, ModuleBuilder module, string item, string methodName, Visibility visibility, StorageClass storageClass, IType declaringType, IType returnType, IType[] parameterTypes, string[] parameterNames, string externString)
        {
            return new MethodBuilder(sourceInformation, module, item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString);
        }

        public MethodBuilder GetMethodFromSymbol(string symbol)
        {
            foreach (var method in _methods)
                if (method.SymbolName == symbol)
                    return method;
            return null;
        }

        public void SetBaseClassAndImplementations(ClassBuilder baseClass, InterfaceBuilder[] interfaces)
        {
            _base = baseClass;
            _interfaces.Clear();
            _interfaces.AddRange(interfaces);
        }
    }
}
