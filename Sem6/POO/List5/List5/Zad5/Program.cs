namespace Zad5
{
    public interface IMathService
    {
        int Divide(int a, int b); // może rzucić wyjątek, np. DivideByZero
    }

    public class FailingMathService : IMathService
    {
        public int Divide(int a, int b)
        {
            throw new DivideByZeroException("Simulated failure");
        }
    }

    public class MathService : IMathService
    {
        public int Divide(int a, int b)
        {
            Console.WriteLine($"[MathService] Divide({a}, {b})");
            return a / b;
        }
    }

    public class RetryProxy : IMathService
    {
        private readonly IMathService _inner;
        private readonly int _maxRetries;

        public RetryProxy(IMathService inner, int maxRetries = 3)
        {
            _inner = inner;
            _maxRetries = maxRetries;
        }

        public int Divide(int a, int b)
        {
            int attempts = 0;
            while(true)
            {
                try
                {
                    return _inner.Divide(a, b);
                }
                catch(Exception ex)
                {
                    attempts++;
                    Console.WriteLine($"[RetryProxy] Attempt {attempts}: {ex.Message}");
                    if(attempts >= _maxRetries)
                        throw;
                }
            }
        }
    }

    public class CircuitBreakerProxy : IMathService
    {
        private readonly IMathService _inner;
        private readonly int _failureThreshold;
        private int _failureCount = 0;
        private bool _isOpen = false;

        public CircuitBreakerProxy(IMathService inner, int failureThreshold = 3)
        {
            _inner = inner;
            _failureThreshold = failureThreshold;
        }

        public int Divide(int a, int b)
        {
            if(_isOpen)
            {
                throw new Exception("[CircuitBreaker] Circuit is open, call blocked.");
            }

            try
            {
                int result = _inner.Divide(a, b);
                _failureCount = 0; // reset
                return result;
            }
            catch(Exception ex)
            {
                _failureCount++;
                Console.WriteLine($"[CircuitBreaker] Failure {_failureCount}/{_failureThreshold}: {ex.Message}");
                if(_failureCount >= _failureThreshold)
                {
                    _isOpen = true;
                    Console.WriteLine("[CircuitBreaker] Circuit opened.");
                }
                throw;
            }
        }
    }

    public static class MathServiceProxyFactory
    {
        public static IMathService CreateWithRetry()
        {
            return new RetryProxy(new MathService(), maxRetries: 3);
        }

        public static IMathService CreateWithCircuitBreaker()
        {
            return new CircuitBreakerProxy(new MathService(), failureThreshold: 2);
        }
    }

    //public class Program
    //{
    //    static void Main(string[] args) { }
    //}
}
