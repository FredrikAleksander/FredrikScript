using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class InvokeInstance : Expression
    {
        public InvokeInstance(Context context, SourceInformation sourceInformation, MethodBuilder method, Expression instance, Expression[] arguments) : base(context, sourceInformation)
        {
            Method = method;
            Instance = instance;
            Arguments = arguments;

            if (method.ParameterTypes.Length != arguments.Length)
                throw new System.Exception("Invalid number of arguments");
            for(int i = 0; i < Arguments.Length; i++)
            {
                if(Arguments[i].ExpressionType != method.ParameterTypes[i])
                {
                    // TODO: Check if argument is a derivative of the parameter type

                    throw new System.Exception($"Argument '{method.ParameterNames[i]}' of type '{method.ParameterTypes[i].FullyQualifiedName}' cannot be called with type '{arguments[i].ExpressionType.FullyQualifiedName}'");
                }
            }
        }

        public MethodBuilder Method { get; set; }
        public Expression Instance { get; set; }
        public Expression[] Arguments { get; set; }
    }
}
