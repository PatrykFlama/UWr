using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using Zad2;


namespace Zad2Tests
{

    public class zad2Tests
    {
        [Fact]
        public void ConstExpression_ReturnsCorrectValue()
        {
            var trueExpr = new ConstExpression(true);
            var falseExpr = new ConstExpression(false);

            Assert.True(trueExpr.Interpret(new Context()));
            Assert.False(falseExpr.Interpret(new Context()));
        }

        [Fact]
        public void VariableExpression_ReturnsValueFromContext()
        {
            var context = new Context();
            context.Put("A", true);
            context.Put("B", false);

            var exprA = new VariableExpression { VariableName = "A" };
            var exprB = new VariableExpression { VariableName = "B" };

            Assert.True(exprA.Interpret(context));
            Assert.False(exprB.Interpret(context));
        }

        [Fact]
        public void UnaryExpression_NegatesCorrectly()
        {
            var context = new Context();
            var expr = new UnaryExpression { Exp = new ConstExpression(true) };

            Assert.False(expr.Interpret(context));
        }

        [Fact]
        public void BinExpression_EvaluatesAndCorrectly()
        {
            var context = new Context();
            var expr = new BinExpression
            {
                Operator = "&",
                Left = new ConstExpression(true),
                Right = new ConstExpression(false)
            };

            Assert.False(expr.Interpret(context));
        }

        [Fact]
        public void BinExpression_EvaluatesOrCorrectly()
        {
            var context = new Context();
            var expr = new BinExpression
            {
                Operator = "|",
                Left = new ConstExpression(true),
                Right = new ConstExpression(false)
            };

            Assert.True(expr.Interpret(context));
        }

        [Fact]
        public void BinExpression_ThrowsOnUnknownOperator()
        {
            var context = new Context();
            var expr = new BinExpression
            {
                Operator = "^",
                Left = new ConstExpression(true),
                Right = new ConstExpression(true)
            };

            Assert.Throws<ArgumentException>(() => expr.Interpret(context));
        }

        [Fact]
        public void Context_ThrowsOnUndefinedVariable()
        {
            var context = new Context();

            Assert.Throws<ArgumentException>(() => context.Get("X"));
        }

        [Fact]
        public void ComplexExpression_EvaluatesCorrectly()
        {
            var context = new Context();
            context.Put("A", true);
            context.Put("B", false);
            context.Put("C", true);

            // (A & B)
            var andExpr = new BinExpression
            {
                Operator = "&",
                Left = new VariableExpression { VariableName = "A" },
                Right = new VariableExpression { VariableName = "B" }
            };

            // ~(A & B)
            var notExpr = new UnaryExpression
            {
                Exp = andExpr
            };

            // ~(A & B) | C
            var orExpr = new BinExpression
            {
                Operator = "|",
                Left = notExpr,
                Right = new VariableExpression { VariableName = "C" }
            };

            bool result = orExpr.Interpret(context);
            Assert.True(result);
        }
    }

}
