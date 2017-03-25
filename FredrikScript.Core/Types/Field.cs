using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class Field
    {
        private ModuleBuilder _module;
        private readonly SourceInformation _sourceInformation;
        private readonly string _name;
        private readonly WeakReference<IType> _declaringType;
        private readonly IType _fieldType;
        private readonly Visibility _visibility;
        private readonly StorageClass _storageClass;

        public Field(SourceInformation sourceInformation, ModuleBuilder module, string name, IType declaringType, IType fieldType, Visibility visibility, StorageClass storageClass)
        {
            _sourceInformation = sourceInformation ?? throw new ArgumentNullException(nameof(sourceInformation));
            _module = module ?? throw new ArgumentNullException(nameof(module));
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _declaringType = new WeakReference<IType>(declaringType ?? throw new ArgumentNullException(nameof(declaringType)));
            _fieldType = fieldType ?? throw new ArgumentNullException(nameof(fieldType));
            _visibility = visibility;
            _storageClass = storageClass;
        }

        public string Name => _name;
        public IType DeclaringType
        {
            get
            {
                if (_declaringType.TryGetTarget(out var declaringType))
                    return declaringType;
                return null;
            }
        }
        public IType FieldType { get; }
        public Visibility Visibility { get; }
        public StorageClass StorageClass { get; }
    }
}
