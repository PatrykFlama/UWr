using System;
using System.Linq;
using System.Linq.Expressions;


namespace Zad4
{
    public class PrintExpressionVisitor : ExpressionVisitor
    {
        protected override Expression VisitBinary(BinaryExpression expression)
        {
            Console.WriteLine("{0} {1} {2}",
                expression.Left, expression.NodeType, expression.Right);
            return base.VisitBinary(expression);
        }

        protected override Expression VisitLambda<T>(Expression<T> expression)
        {
            Console.WriteLine("{0} -> {1}",
                expression.Parameters.Aggregate(string.Empty, (a, e) => a + e),
                expression.Body);
            return base.VisitLambda<T>(expression);
        }

        protected override Expression VisitUnary(UnaryExpression node)
        {
            Console.WriteLine("Unary: {0} ({1})", node.NodeType, node.Operand);
            return base.VisitUnary(node);
        }

        protected override Expression VisitConstant(ConstantExpression node)
        {
            Console.WriteLine("Constant: {0}", node.Value);
            return base.VisitConstant(node);
        }

        protected override Expression VisitConditional(ConditionalExpression expression)
        {
            Console.WriteLine("if {0} then {1} else {2}",
                expression.Test, expression.IfTrue, expression.IfFalse);
            return base.VisitConditional(expression);
        }
    }
}
