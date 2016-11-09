package com.softwaremill

import com.twitter.algebird._

import scala.collection.immutable.Seq
import scala.util.Random

trait Fold[I, O] {
  type M
  def m: Monoid[M]

  def tally: I => M
  def summarize: M => O
}

object Fold {
  def apply[I, O, _M](_m: Monoid[_M])(_tally: I => _M, _summarize: _M => O): Fold[I, O] = new Fold[I, O] {
    override type M = _M
    override def m = _m
    override def tally = _tally
    override def summarize = _summarize
  }

  def fold[I, O](input: Seq[I])(f: Fold[I, O]): O = {
    val reduced = input.foldLeft(f.m.zero) { case (a, i) => f.m.plus(a, f.tally(i)) }
    f.summarize(reduced)
  }

  def sum[MM](implicit mm: Monoid[MM]): Fold[MM, MM] = new Fold[MM, MM] {
    type M = MM
    val m = mm
    def tally = identity
    def summarize = identity
  }

  def sum2[M](implicit m: Monoid[M]): Fold[M, M] = Fold(m)(identity, identity)

  def combine[I, O1, O2](f1: Fold[I, O1], f2: Fold[I, O2]): Fold[I, (O1, O2)] = new Fold[I, (O1, O2)] {
    override type M = (f1.M, f2.M)
    override def m = new Monoid[M] {
      override def zero = (f1.m.zero, f2.m.zero)
      override def plus(l: (f1.M, f2.M), r: (f1.M, f2.M)) = (f1.m.plus(l._1, r._1), f2.m.plus(l._2, r._2))
    }
    override def tally = i => (f1.tally(i), f2.tally(i))
    override def summarize = x => (f1.summarize(x._1), f2.summarize(x._2))
  }
}

object Example1 extends App {
  import Fold._

  println(fold(1 to 10)(sum))
  println(fold(1L to 1000000000L)(sum)) // much slower - fold not specialized
}

object Example2 extends App {
  import Fold._
  import Fractional.Implicits._

  case class Average[A](numerator: A, denominator: Int)

  implicit def averageMonoid[A: Numeric] = new Monoid[Average[A]] {
    override def zero = Average(implicitly[Numeric[A]].zero, 0)
    override def plus(l: Average[A], r: Average[A]) = Average(
      implicitly[Numeric[A]].plus(l.numerator, r.numerator),
      l.denominator+r.denominator)
  }

  /*
  def average[A: Fractional]: Fold[A, A] = new Fold[A, A] {
    type M = Average[A]
    val m = averageMonoid
    def tally = Average[A](_, 1)
    def summarize = a => a.numerator / implicitly[Fractional[A]].fromInt(a.denominator)
  }
  */

  def average[A: Fractional]: Fold[A, A] = Fold(averageMonoid)(
    Average[A](_, 1),
    a => a.numerator / implicitly[Fractional[A]].fromInt(a.denominator))

  println(fold(1.0 to 10.0 by 1.0)(average))
}

object Example3 extends App {
  import Fold._
  import Monoid._

  def productMonoid[A: Numeric] = new Monoid[A] {
    override def zero = implicitly[Numeric[A]].one
    override def plus(l: A, r: A) = implicitly[Numeric[A]].times(l, r)
  }

  def firstMonoid[T] = new Monoid[Option[T]] {
    override def zero = None
    override def plus(l: Option[T], r: Option[T]) = l.orElse(r)
  }

  def lastMonoid[T] = new Monoid[Option[T]] {
    override def zero = None
    override def plus(l: Option[T], r: Option[T]) = r.orElse(l)
  }

  def first[T]: Fold[T, Option[T]] = Fold(firstMonoid[T])(i => Some(i), identity)
  def last[T]: Fold[T, Option[T]] = Fold(lastMonoid[T])(i => Some(i), identity)
  def all[A](p: A => Boolean): Fold[A, Boolean] = Fold(AndValMonoid)(i => AndVal(p(i)), _.get)
  def any[A](p: A => Boolean): Fold[A, Boolean] = Fold(OrValMonoid)(i => OrVal(p(i)), _.get)
  def product[A: Numeric]: Fold[A, A] = Fold(productMonoid)(identity, identity)
  def length[A]: Fold[A, Int] = Fold(intMonoid)(_ => 1, identity)

  def even(i: Int) = i%2 == 0
  def negative(i: Int) = i < 0

  println(fold(1 to 10)(first))
  println(fold(1 to 10)(last))
  println(fold(1 to 10)(all(even)))
  println(fold(1 to 10)(any(even)))
  println(fold(1 to 10)(any(negative)))
  println(fold(1 to 10)(product))
  println(fold(1 to 10)(length))
}

object Example4 extends App {
  // cannot implement EMA as Scala's Numeric/Fractional type classes don't have ^ (exponentiation)
}

object Example5 extends App {
  import Fold._

  def uniques[I](hash: I => Long): Fold[I, Int] = Fold(Max.intMonoid)(
    i => Max(java.lang.Long.numberOfLeadingZeros(hash(i))),
    m => Math.pow(2, m.get).toInt
  )

  val random = new Random()
  val randomLongs: Stream[Long] = Stream.continually(random.nextLong()).filter(_ > 0).take(10)
  val randomLoopedLongs: Stream[Long] = randomLongs.append(randomLoopedLongs).take(10000000) // 1000000000

  println(fold(randomLoopedLongs)(uniques(identity)))
}

object Example6 {
  def foldFunctor[I] = new Functor[({type F[X]=Fold[I, X]})#F] {
    override def map[T, U](f: Fold[I, T])(fn: T => U) = Fold(f.m)(f.tally, f.summarize.andThen(fn))
  }

  val unitMonoid = new Monoid[Unit] {
    override def zero = ()
    override def plus(l: Unit, r: Unit) = ()
  }
  
  def foldApplicative[I] = new Applicative[({type F[X]=Fold[I, X]})#F] {
    override def map[T, U](f: Fold[I, T])(fn: T => U) = Fold(f.m)(f.tally, f.summarize.andThen(fn))
    override def apply[T](v: T) = Fold(unitMonoid)(_ => (), _ => v)
    override def join[T, U](mt: Fold[I, T], mu: Fold[I, U]) = new Fold[I, (T, U)] {
      override type M = (mt.M, mu.M)
      override def m = new Monoid[M] {
        override def zero = (mt.m.zero, mu.m.zero)
        override def plus(l: (mt.M, mu.M), r: (mt.M, mu.M)) = (mt.m.plus(l._1, r._1), mu.m.plus(l._2, r._2))
      }
      override def tally = i => (mt.tally(i), mu.tally(i))
      override def summarize = x => (mt.summarize(x._1), mu.summarize(x._2))
    }
  }


}