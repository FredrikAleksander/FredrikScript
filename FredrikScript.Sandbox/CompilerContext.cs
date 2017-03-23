using FredrikScript.Sandbox.TypeBuilders;
using FredrikScript.Sandbox.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Sandbox
{
    public class CompilerContext : IDisposable
    {
        private readonly LLVMContextRef _context = LLVM.ContextCreate();
        private readonly List<string> _namespaces = new List<string>();
        private readonly Dictionary<string, IType> _types = new Dictionary<string, IType>();

        private LLVMTypeRef _voidPointerType;

        public CompilerContext()
        {
            AddBuiltinTypes();
        }

        private void AddBuiltinTypes()
        {
            _types["double"] = new DoubleType(this);
            _types["float"] = new FloatType(this);
            _types["int"] = new IntType(this);
            _types["long"] = new LongType(this);
            _types["short"] = new ShortType(this);
            _types["uint"] = new UintType(this);
            _types["ulong"] = new UlongType(this);
            _types["ushort"] = new UshortType(this);
            _types["void*"] = new VoidPointer(this);
        }

        public LLVMContextRef LLVMContext => _context;

        public List<string> Namespaces => _namespaces;
        public Dictionary<string, IType> Types => _types;

        #region IDisposable Support
        private bool disposedValue = false; // To detect redundant calls

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                if (disposing)
                {
                }
                _context.ContextDispose();
                disposedValue = true;
            }
        }
         ~CompilerContext()
        {
            Dispose(false);
        }

        // This code added to correctly implement the disposable pattern.
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        private string GetFullyQualifiedName(string ns, string name)
        {
            if (string.IsNullOrWhiteSpace((ns)))
                return name;
            return ns + "." + name;
        }

        private IType ScanTypes(string name, List<string> imports)
        {
            foreach(var import in imports)
            {
                var fqn = GetFullyQualifiedName(import, name);
                return ResolveType(fqn);
            }
            return null;
        }

        private IType ResolveType(string name)
        {
            if (_types.ContainsKey(name))
                return _types[name];
            if (name.EndsWith("[]"))
            {
                var s = name.LastIndexOf("[]");
                var elementTypeName = name.Substring(0, s);
                var elementType = ResolveType(elementTypeName) ?? throw new KeyNotFoundException();
                return _types[name] = new ArrayBuilder(this, elementType);
            }
            return null;
        }

        public IType ResolveType(string name, string currentNamespace, IEnumerable<string> usingNamespaces)
        {
            List<string> imports = new List<string>();
            imports.Add(currentNamespace);
            imports.AddRange(usingNamespaces.Reverse());
            imports.Add("");
            return ScanTypes(name, imports);
        }
        #endregion
    }
}
