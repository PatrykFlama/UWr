namespace Zad2
{
    using System;
    using System.Collections.Generic;
    using System.Dynamic;
    using System.Linq.Expressions;

    class MyDynamicObject : DynamicObject
    {
        private Dictionary<string, object> properties = new Dictionary<string, object>();

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            string propertyName = binder.Name;
            return properties.TryGetValue(propertyName, out result);
        }

        public override bool TrySetMember(SetMemberBinder binder, object value)
        {
            string propertyName = binder.Name;
            properties[propertyName] = value;
            return true;
        }

        public override bool TryGetIndex(GetIndexBinder binder, object[] indexes, out object result)
        {
            if (indexes.Length == 1 && indexes[0] is string key)
            {
                return properties.TryGetValue(key, out result);
            }
            result = null;
            return false;
        }

        public override bool TrySetIndex(SetIndexBinder binder, object[] indexes, object value)
        {
            if (indexes.Length == 1 && indexes[0] is string key)
            {
                properties[key] = value;
                return true;
            }
            return false;
        }

        public override bool TryInvoke(InvokeBinder binder, object[] args, out object result)
        {
            if (args.Length == 1 && args[0] is string methodName)
            {
                Console.WriteLine($"metoda {methodName} została wywołana");
                result = null;
                return true;
            }
            result = null;
            return false;
        }

        public override bool TryUnaryOperation(UnaryOperationBinder binder, out object result)
        {
            if (binder.Operation == ExpressionType.Negate)
            {
                if (properties.TryGetValue("Value", out object value))
                {
                    if (value is int intValue)
                    {
                        result = -intValue;
                        return true;
                    }
                }
            }
            result = null;
            return false;
        }

        public override bool TryBinaryOperation(BinaryOperationBinder binder, object arg, out object result)
        {
            if (binder.Operation == ExpressionType.Add)
            {
                if (properties.TryGetValue("Value", out object value) && arg is int otherValue)
                {
                    if (value is int intValue)
                    {
                        result = intValue + otherValue;
                        return true;
                    }
                }
            }
            result = null;
            return false;
        }
    }

    class Program2
    {
        public static void Main(string[] args)
        {
            dynamic dynamicObject = new MyDynamicObject();

            dynamicObject.FirstName = "John";
            Console.WriteLine(dynamicObject.FirstName);

            dynamicObject["LastName"] = "Doe";
            Console.WriteLine(dynamicObject["LastName"]);

            dynamicObject("SomeMethod");

            dynamicObject = -dynamicObject;

            dynamicObject += 10;
        }
    }
}
