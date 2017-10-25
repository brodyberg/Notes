using System;
using System.Collections.Generic;
using static System.Console;
using System.Linq;

namespace EitherTest
{
    public static class Either
    {
        public static Either<T, E> Right<T, E>(T t) where T : class => Either<T, E>.Right(t);
        public static Either<T, E> Left<T, E>(E e) where T : class => Either<T, E>.Left(e);
    }

    public class Either<T, E> where T : class
    {
        private readonly T t;
        private readonly E e;

        private readonly bool isSuccess;

        public static Either<T, E> Right(T t) => new Either<T, E>(t, default(E));
        public static Either<T, E> Left(E e) => new Either<T, E>(default(T), e);

        private Either(T t, E e)
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

        public static Either<T, E> Bind(
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
            else
            {
                return Error.ToString();
            }
        }
    }

    public static class EitherExtensions
    {
        // allows for the transformation of T to B through f
        public static Either<B, E> Bind<T, E, B>(
            this Either<T, E> first,
            Func<T, Either<B, E>> f)
            where T : class
            where B : class
        {
            if (first.IsSuccess)
            {
                return f(first.Success);
            }
            else
            {
                return Either.Left<B, E>(first.Error);
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
            Either.Right<IntWrapper, string>(new IntWrapper(100));

            var blue =
                Either.Right<IntWrapper, string>(new IntWrapper(100))
                    .Bind(n => Either.Right<List<IntWrapper>, string>(new List<IntWrapper>() { n, new IntWrapper(101) }));

            var yellow =
                Either.Left<IntWrapper, string>("Boom!")
                    .Bind(n => Either.Right<IntWrapper, string>(new IntWrapper(1)));

            WriteLine("Blue: {0}", string.Join(", ", blue.Success.Select(s => s.Value)));
            WriteLine("Blue: {0}", yellow.ToString());
        }
    }
}
