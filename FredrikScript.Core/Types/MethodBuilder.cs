using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class MethodBuilder
    {
        private readonly SourceInformation _sourceInformation;
        private readonly string _name;
        private readonly string _symbolName;
        private readonly Visibility _visibility;
        private readonly StorageClass _storageClass;
        private readonly IType _declaringType;
        private readonly IType _returnType;
        private readonly IType[] _parameterTypes;
        private readonly ExternalLinkageOptions _externalLinkageOptions;
        private Expressions.Expression _body;
        private readonly string[] _parameterNames;

        public MethodBuilder(SourceInformation sourceInformation, ModuleBuilder module, string name, string symbolName, Visibility visibility, StorageClass storageClass, IType declaringType, IType returnType, IType[] parameterTypes, string[] parameterNames, string externString)
        {
            _sourceInformation = sourceInformation ?? throw new ArgumentNullException(nameof(sourceInformation));
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _symbolName = symbolName ?? throw new ArgumentNullException(nameof(symbolName));
            _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
            _returnType = returnType ?? throw new ArgumentNullException(nameof(returnType));
            _parameterTypes = parameterTypes ?? throw new ArgumentNullException(nameof(parameterTypes));
            _parameterNames = parameterNames ?? throw new ArgumentNullException(nameof(parameterNames));
            _visibility = visibility;
            _storageClass = storageClass;
            if (storageClass == StorageClass.Extern)
                _externalLinkageOptions = ExternalLinkageOptions.Parse(externString);
        }

        public void SetMethodBody(Expressions.Expression expressions)
        {
            _body = expressions;
        }

        public Expressions.Expression Body => _body;

        public string Name => _name;
        public string SymbolName => _symbolName;
        public Visibility Visibility => _visibility;
        public StorageClass StorageClass => _storageClass;
        public IType DeclaringType => _declaringType;
        public IType ReturnType => _returnType;
        public IType[] ParameterTypes => _parameterTypes;
        public string[] ParameterNames => _parameterNames;
        public ExternalLinkageOptions ExternalLinkageOptions => _externalLinkageOptions;
    }
}
