using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core
{
    public class Context
    {
        private string _moduleName;
        protected ModuleBuilder _moduleBuilder;
        protected readonly List<string> _namespaces = new List<string>();
        protected readonly Dictionary<string, IType> _types = new Dictionary<string, IType>();
        protected readonly Dictionary<string, MethodBuilder> _methods = new Dictionary<string, MethodBuilder>();

        public Context(string moduleName)
        {
            _moduleName = moduleName;
            AddBuiltinTypes();
        }

        public virtual void EnsureNamespace(string @namespace)
        {
            if (!_namespaces.Contains(@namespace))
            {
                if (_types.ContainsKey(@namespace))
                    throw new Exception($"Namespace '{@namespace}' conflicts with type");
                _namespaces.Add(@namespace);
            }
        }

        public ModuleBuilder ModuleBuilder
        {
            get
            {
                if(_moduleBuilder == null)
                    _moduleBuilder = CreateModule(_moduleName);
                return _moduleBuilder;
            }
        }

        public MethodBuilder GetEntryPoint(string name = null)
        {
            foreach(var type in _types)
            {
                var classBuilder = type.Value as ClassBuilder;
                var structBuilder = type.Value as StructBuilder;
                if (classBuilder != null)
                {
                    foreach(var method in classBuilder.Methods.Where(x => x.StorageClass == StorageClass.Static && x.Visibility == Visibility.Public && x.ReturnType == Types["int"] && x.ParameterTypes.Length == 1 && x.ParameterTypes[0].FullyQualifiedName == "int"))
                    {
                        if (method.Name == "main")
                            return method;
                    }
                }
                if(type.Value is StructBuilder)
                {

                }
            }
            return null;
        }

        protected virtual EnumBuilder CreateEnumBuilder(string @namespace, string name) { return new EnumBuilder(@namespace, name); }
        protected virtual ClassBuilder CreateClassBuilder(string @namespace, string name) { return new ClassBuilder(@namespace, name); }
        protected virtual StructBuilder CreateStructBuilder(string @namespace, string name) { return new StructBuilder(@namespace, name); }
        protected virtual ModuleBuilder CreateModule(string moduleName) { return new ModuleBuilder(moduleName); }

        protected virtual BooleanType CreateBooleanType() { return new BooleanType(); }
        protected virtual SbyteType CreateSbyteType() { return new SbyteType(); }
        protected virtual ByteType CreateByteType() { return new ByteType(); }
        protected virtual CharType CreateCharType() { return new CharType(); }
        protected virtual DoubleType CreateDoubleType() { return new DoubleType(); }
        protected virtual FloatType CreateFloatType() { return new FloatType(); }
        protected virtual IntType CreateIntType() { return new IntType(); }
        protected virtual LongType CreateLongType() { return new LongType(); }
        protected virtual ShortType CreateShortType() { return new ShortType(); }
        protected virtual StringType CreateStringType() { return new StringType(); }
        protected virtual UintType CreateUintType() { return new UintType(); }
        protected virtual UlongType CreateUlongType() { return new UlongType(); }
        protected virtual UshortType CreateUshortType() { return new UshortType(); }
        protected virtual VoidPointerType CreateVoidPointer() { return new VoidPointerType(); }
        protected virtual VoidType CreateVoidType() { return new VoidType(); }
        protected virtual ArrayBuilder CreateArray(IType elementType) { return new ArrayBuilder(elementType); }

        protected virtual void AddBuiltinTypes()
        {
            _types["double"] = CreateDoubleType();
            _types["float"] = CreateFloatType();
            _types["int"] = CreateIntType();
            _types["long"] = CreateLongType();
            _types["short"] = CreateShortType();
            _types["uint"] = CreateUintType();
            _types["ulong"] = CreateUlongType();
            _types["ushort"] = CreateUshortType();
            _types["void*"] = CreateVoidPointer();
            _types["void"] = CreateVoidType();
        }

        public IReadOnlyList<string> Namespaces => _namespaces;
        public IReadOnlyDictionary<string, IType> Types => _types;
        public IReadOnlyDictionary<string, MethodBuilder> Methods => _methods;

        public virtual EnumBuilder CreateEnum(string @namespace, string enumName)
        {
            var fqn = string.IsNullOrWhiteSpace(@namespace) ? enumName : @namespace + "." + enumName;
            if (Namespaces.Contains(fqn))
                throw new Exception($"Enum '{fqn}' conflicts with namespace");
            if (Types.ContainsKey(fqn))
                throw new Exception($"Type '{fqn} already defined'");
            var builder = CreateEnumBuilder(@namespace, enumName);
            _types[fqn] = builder;
            return builder;
        }

        public virtual ClassBuilder CreateClass(string @namespace, string className)
        {
            var fqn = string.IsNullOrWhiteSpace(@namespace) ? className : @namespace + "." + className;
            if (Namespaces.Contains(fqn))
                throw new Exception($"Class '{fqn}' conflicts with namespace");
            if (Types.ContainsKey(fqn))
                throw new Exception($"Type '{fqn} already defined'");
            var builder = CreateClassBuilder(@namespace, className);
            _types[fqn] = builder;
            return builder;
        }

        public virtual StructBuilder CreateStruct(string @namespace, string structName)
        {
            var fqn = string.IsNullOrWhiteSpace(@namespace) ? structName : @namespace + "." + structName;
            if (Namespaces.Contains(fqn))
                throw new Exception($"Struct '{fqn}' conflicts with namespace");
            if (Types.ContainsKey(fqn))
                throw new Exception($"Type '{fqn} already defined'");
            var builder = CreateStructBuilder(@namespace, structName);
            _types[fqn] = builder;
            return builder;
        }

        public virtual InterfaceBuilder CreateInterface(string @namespace, string interfaceName)
        {
            var fqn = string.IsNullOrWhiteSpace(@namespace) ? interfaceName : @namespace + "." + interfaceName;
            if (Namespaces.Contains(fqn))
                throw new Exception($"Interface '{fqn}' conflicts with namespace");
            if (Types.ContainsKey(fqn))
                throw new Exception($"Type '{fqn} already defined'");
            var builder = new InterfaceBuilder(@namespace, interfaceName);
            _types[fqn] = builder;
            return builder;
        }

        private string GetFullyQualifiedName(string ns, string name)
        {
            if (string.IsNullOrWhiteSpace((ns)))
                return name;
            return ns + "." + name;
        }
        private IType ScanTypes(string name, List<string> imports)
        {
            foreach (var import in imports)
            {
                var fqn = GetFullyQualifiedName(import, name);
                var tmp = ResolveTypeFullyQualified(fqn);
                if (tmp != null)
                    return tmp;
            }
            return null;
        }
        public IType ResolveTypeFullyQualified(string name)
        {
            if (_types.ContainsKey(name))
                return _types[name];
            if (name.EndsWith("[]"))
            {
                var s = name.LastIndexOf("[]");
                var elementTypeName = name.Substring(0, s);
                var elementType = ResolveTypeFullyQualified(elementTypeName) ?? throw new KeyNotFoundException();
                return _types[name] = CreateArray(elementType);
            }
            return null;
        }
        public IType ResolveType(string name, string currentNamespace, IEnumerable<string> usingNamespaces)
        {
            List<string> imports = new List<string>();
            imports.Add(currentNamespace);
            imports.Add("");
            imports.AddRange(usingNamespaces.Reverse());
            return ScanTypes(name, imports);
        }
    }
}
