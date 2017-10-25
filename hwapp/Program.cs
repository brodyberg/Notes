using System;
using static System.Console;

namespace hwapp
{
    public class Either<T,E> where T : class
    {
        private readonly T t;
        private readonly E e;

        private readonly bool isSuccess; 

        public Either(T t, E e)
        {
            if (t == null &&
                e == null)
            {
                throw new InvalidOperationException("Can't both be null");
            }

            if (t != null &&
                e != null)
            {
                throw new InvalidOperationException("Can't both be not null");
            }

            if (t != null)
            {
                this.t = t;
                this.isSuccess = true;
            }
            else
            {
                this.e = e;
                this.isSuccess = false;
            }
        }

        public bool IsSuccess => isSuccess;

        public T Success => t;
        public E Error => e;

        public static Either<T,E> Bind(
            Either<T, E> first, 
            Func<T, Either<T, E>> f)
        {
            if (first.isSuccess)
            {
                return f(first.Success);
            }
            else
            {
                return new Either<T, E>(default(T), first.Error); 
            }
        }

        public override string ToString()
        {
            if (isSuccess)
            {
                return Success.ToString();
            }
            else{
                return Error.ToString();
            }
        }
    }

    public static class EitherExtensions
    {
       public static Either<T,E> Bind<T,E>(
            this Either<T, E> first, 
            Func<T, Either<T, E>> f) where T : class
        {
            if (first.IsSuccess)
            {
                return f(first.Success);
            }
            else
            {
                return new Either<T, E>(default(T), first.Error); 
            }
        }
    }

    public class IntWrapper 
    {
        private readonly int i;

        public IntWrapper(int i)
        {
            this.i = i;
        }

        public int Value => i;

        public override string ToString()
        {
            return i.ToString();
        }
    }

    // Left "boom" >>= \n -> return (n + 1)
    // Right 100 >>= \n -> return(n + 1)
    // Right [100] >>= \n -> return(33:n)
    // Right (4,'a') >>= \(l,r) -> return (l,r,5) 
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");

            Either<IntWrapper, string> x = new Either<IntWrapper, string>(new IntWrapper(100), null);

            var blue = 
                (new Either<IntWrapper, string>(new IntWrapper(100), null))
                    .Bind(n => new Either<IntWrapper, string>(new IntWrapper(n.Value + 1), null));

            var yellow = 
                (new Either<IntWrapper, string>(null, "Boom!"))
                    .Bind(n => new Either<IntWrapper, string>(new IntWrapper(n.Value + 1), null));

            WriteLine("Blue: {0}", blue.ToString());
            WriteLine("Blue: {0}", yellow.ToString());
        }
    }
}
