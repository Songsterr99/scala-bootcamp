package com.evolutiongaming.bootcamp.cats

import cats.Functor

object TypeHierarchy {

  /*
    Semigroup[A] {
      def combine(y: A): A
    }
   */

  /*
    Monoid[A] {
      def combine(y: A): A
    }

    def empty: A
   */


  /*
    Functor[F[A]] {
      def map(f: A => B): F[B]
    }
   */

  /*
    Applicative[F[A]] {
      def ap(f: F[A => B]): F[B]
    }

    def pure[A](x: A): F[A]
   */


  /*
    Monad[F[A]] {
      def flatMap(f: A => F[B]): F[B]
    }

    def pure[A](x: A): F[A]
   */


  // Laws
}

object Excercises {

  trait Applicative[F[_]] extends Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = unit(map(fab)(f => map(fa)(a => f(a))))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = apply(map(fa)(a => (b: B) => (a, b)))(fb)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map(product(fa, fb))(a => f(a._1, a._2))


    def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(unit(List.empty[A])) {(a, result) =>
      map2(a, result)(_ :: _)
    }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List.empty[B])) { (a, result) =>
      map2(f(a), result)(_ :: _)
    }
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def apply[A, B](mab: M[A => B])(ma: M[A]): M[B] = unit(map(mab)(f => map(ma)(a => f(a))))

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] = apply(map(fa)(a => (b: B) => (a, b)))(fb)

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

    def map[A, B](ma: M[A])(f: A => B): M[B] = apply(unit(f))(ma)

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = map(product(ma, mb))(a => f(a._1, a._2))
  }

}
