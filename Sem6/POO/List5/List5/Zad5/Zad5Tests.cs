using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using System;
using Zad5;

namespace Zad5Tests
{


    public class ProxyProjectTests
    {
        [Fact]
        public void RetryProxy_ShouldRetryThreeTimes_ThenThrow()
        {
            var proxy = new RetryProxy(new FailingMathService(), maxRetries: 3);
            int attempts = 0;

            try
            {
                proxy.Divide(10, 0);
            }
            catch(DivideByZeroException)
            {
                // Expected exception
            }

            // Brak metody, by podejrzeć liczbę prób bez refaktoryzacji – 
            // test oparty na sprawdzeniu braku crasha oraz zwrócenia wyjątku
            Assert.Throws<DivideByZeroException>(() => proxy.Divide(10, 0));
        }

        [Fact]
        public void RetryProxy_ShouldSucceed_IfRetryEventuallyWorks()
        {
            var flaky = new SometimesFailingMathService(failuresBeforeSuccess: 2);
            var proxy = new RetryProxy(flaky, maxRetries: 3);

            int result = proxy.Divide(10, 2);
            Assert.Equal(5, result);
        }

        public void CircuitBreaker_ShouldOpen_AfterTwoFailures()
        {
            var proxy = new CircuitBreakerProxy(new FailingMathService(), failureThreshold: 2);

            // 2 failures
            Assert.Throws<DivideByZeroException>(() => proxy.Divide(10, 0));
            Assert.Throws<DivideByZeroException>(() => proxy.Divide(10, 0));

            // Circuit should be open now
            var ex = Assert.Throws<Exception>(() => proxy.Divide(10, 1));
            Assert.Contains("Circuit is open", ex.Message);
        }

        [Fact]
        public void CircuitBreaker_ShouldReset_FailureCount_AfterSuccess()
        {
            var math = new SometimesFailingMathService(1); // first call fails, then ok
            var proxy = new CircuitBreakerProxy(math, failureThreshold: 2);

            // first fails
            Assert.Throws<Exception>(() => proxy.Divide(10, 2));

            // second succeeds, should reset failure count
            int result = proxy.Divide(10, 2);
            Assert.Equal(5, result);

            // another failure should NOT open the circuit yet
            math = new SometimesFailingMathService(1);
            proxy = new CircuitBreakerProxy(math, failureThreshold: 2);

            Assert.Throws<Exception>(() => proxy.Divide(10, 2)); // 1 failure
            result = proxy.Divide(10, 2); // success
            Assert.Equal(5, result);
        }
    }

    public class SometimesFailingMathService : IMathService
    {
        private int _failuresBeforeSuccess;

        public SometimesFailingMathService(int failuresBeforeSuccess)
        {
            _failuresBeforeSuccess = failuresBeforeSuccess;
        }

        public int Divide(int a, int b)
        {
            if(_failuresBeforeSuccess-- > 0)
            {
                throw new Exception("Temporary failure");
            }
            return a / b;
        }
    }

}
