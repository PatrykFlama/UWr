namespace Zad2
{
    public class Context
    {
        private Dictionary<string, bool> _localVariables = new Dictionary<string, bool>();


        public bool Get(string variableName)
        {
            if (_localVariables.ContainsKey(variableName))
            {
                return _localVariables[variableName];
            } else
            {
                throw new ArgumentException($"Variable '{variableName}' was not defined in context");
            }
        }

        public void Put(string variableName, bool variableValue)
        {
            if (_localVariables.ContainsKey(variableName))
            {
                _localVariables.Remove(variableName);
            }
            _localVariables.Add(variableName, variableValue);
        }
    }

    public abstract class AbstractExpression
    {
        public abstract bool Interpret(Context context);
    }

    public class BinExpression : AbstractExpression
    {
        public AbstractExpression Left;
        public AbstractExpression Right;
        public string Operator;

        public override bool Interpret(Context context)
        {
            switch (Operator)
            {
                case "&":
                    return Left.Interpret(context) && Right.Interpret(context);
                case "|":
                    return Left.Interpret(context) || Right.Interpret(context);
                default:
                    throw new ArgumentException($"Unknown operator: {Operator}");
            }
        }
    }

    public class UnaryExpression : AbstractExpression
    {
        public AbstractExpression Exp;

        public override bool Interpret(Context context)
        {
            return !Exp.Interpret(context);
        }
    }

    public class ConstExpression : AbstractExpression
    {
        private readonly bool _value;

        public ConstExpression(bool value)
        {
            _value = value;
        }

        public override bool Interpret(Context context)
        {
            return _value;
        }
    }

    public class VariableExpression : AbstractExpression
    {
        public string VariableName;

        public override bool Interpret(Context context)
        {
            return context.Get(VariableName);
        }
    }

}
