using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using Zad4;

namespace Zad4Tests
{
    public class TrackingVisitor : ExpressionVisitor
    {
        public List<string> Visited = new List<string>();

        protected override Expression VisitUnary(UnaryExpression node)
        {
            Visited.Add($"Unary:{node.NodeType}");
            return base.VisitUnary(node);
        }

        protected override Expression VisitConditional(ConditionalExpression node)
        {
            Visited.Add("Conditional");
            return base.VisitConditional(node);
        }

        protected override Expression VisitConstant(ConstantExpression node)
        {
            Visited.Add($"Constant:{node.Value}");
            return base.VisitConstant(node);
        }
    }


    public class zad4Tests
    {
        [Fact]
        public void VisitUnary_ShouldTrackUnaryExpression()
        {
            // -5
            Expression expr = Expression.Negate(Expression.Constant(5));

            var visitor = new TrackingVisitor();
            visitor.Visit(expr);

            Assert.Contains("Unary:Negate", visitor.Visited);
        }

        [Fact]
        public void VisitConstant_ShouldTrackConstantExpression()
        {
            Expression expr = Expression.Constant(42);

            var visitor = new TrackingVisitor();
            visitor.Visit(expr);

            Assert.Contains("Constant:42", visitor.Visited);
        }

        [Fact]
        public void VisitConditional_ShouldTrackConditionalExpression()
        {
            // n > 0 ? n : -n
            var param = Expression.Parameter(typeof(int), "n");
            var test = Expression.GreaterThan(param, Expression.Constant(0));
            var ifTrue = param;
            var ifFalse = Expression.Negate(param);
            var conditional = Expression.Condition(test, ifTrue, ifFalse);

            var lambda = Expression.Lambda<Func<int, int>>(conditional, param);

            var visitor = new TrackingVisitor();
            visitor.Visit(lambda);

            Assert.Contains("Conditional", visitor.Visited);
        }
    }

}
