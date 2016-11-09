package com.softwaremill

import cats._
import cats.data._
import cats.implicits._
import monocle.{Lens, Optional, Prism}

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
    val reduced = input.foldLeft(f.m.empty) { case (a, i) => f.m.combine(a, f.tally(i)) }
    f.summarize(reduced)
  }

  def sum[MM](implicit mm: Monoid[MM]): Fold[MM, MM] = new Fold[MM, MM] {
    type M = MM
    val m = mm
    def tally = identity
    def summarize = identity
  }

  def sum2[M](implicit m: Monoid[M]): Fold[M, M] = Fold(m)(identity, identity)

  /*
  def combine[I, O1, O2](f1: Fold[I, O1], f2: Fold[I, O2]): Fold[I, (O1, O2)] = new Fold[I, (O1, O2)] {
    override type M = (f1.M, f2.M)
    override def m = new Monoid[M] {
      override def empty = (f1.m.empty, f2.m.empty)
      override def combine(l: (f1.M, f2.M), r: (f1.M, f2.M)) = (f1.m.combine(l._1, r._1), f2.m.combine(l._2, r._2))
    }
    override def tally = i => (f1.tally(i), f2.tally(i))
    override def summarize = x => (f1.summarize(x._1), f2.summarize(x._2))
  }
  */
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
    override def empty = Average(implicitly[Numeric[A]].zero, 0)
    override def combine(l: Average[A], r: Average[A]) = Average(
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
  import CustomMonoids._

  def first[T]: Fold[T, Option[T]] = Fold(firstMonoid[T])(i => Some(i), identity)
  def last[T]: Fold[T, Option[T]] = Fold(lastMonoid[T])(i => Some(i), identity)
  def all[A](p: A => Boolean): Fold[A, Boolean] = Fold(andMonoid)(i => p(i), identity)
  def any[A](p: A => Boolean): Fold[A, Boolean] = Fold(orMonoid)(i => p(i), identity)
  def product[A: Numeric]: Fold[A, A] = Fold(numProductMonoid)(identity, identity)
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
  import CustomMonoids._

  def uniques[I](hash: I => Long): Fold[I, Int] = Fold(maxIntMonoid)(
    i => Max(java.lang.Long.numberOfLeadingZeros(hash(i))),
    m => Math.pow(2, m.v).toInt
  )

  val random = new Random()
  val randomLongs: Stream[Long] = Stream.continually(random.nextLong()).filter(_ > 0).take(10)
  val randomLoopedLongs: Stream[Long] = randomLongs.append(randomLoopedLongs).take(10000000) // 1000000000

  println(fold(randomLoopedLongs)(uniques(identity)))
}

object Example6 extends App {
  import Fold._
  import Example3.product
  import cats.syntax.cartesian._

  def foldFunctor[I] = new Functor[({type F[X]=Fold[I, X]})#F] {
    override def map[T, U](f: Fold[I, T])(fn: T => U) = Fold(f.m)(f.tally, f.summarize.andThen(fn))
  }

  implicit def foldApplicative[I] = new Applicative[({type F[X]=Fold[I, X]})#F] {
    override def pure[A](x: A) = Fold(catsKernelStdAlgebraForUnit)(_ => (), _ => x)
    override def ap[A, B](ff: Fold[I, (A) => B])(fa: Fold[I, A]): Fold[I, B] = Fold(ff.m product fa.m)(
      i => (ff.tally(i), fa.tally(i)),
      { case (mf, ma) => ff.summarize(mf)(fa.summarize(ma)) }
    )
  }

  def combine[I, O1, O2](f1: Fold[I, O1], f2: Fold[I, O2]): Fold[I, (O1, O2)] = foldApplicative.map2(f1, f2)((_, _))

  println(fold(1 to 10)(combine(sum, product)))
  println(fold(1 to 10)((sum[Int] |@| product).map((_, _))))
}

object Example7 extends App {
  import Fold._
  import Example6._
  import Fractional.Implicits._

  def length[A: Numeric: Monoid]: Fold[A, A] = Fold(implicitly[Monoid[A]])(_ => implicitly[Numeric[A]].one, identity)

  def average[A: Fractional: Monoid]: Fold[A, A] = (sum[A] |@| length[A]).map(_ / _)

  println(fold(1.0 to 10.0 by 1.0)(average[Double]))
}

object Example8 extends App {
  import Fold._
  import Example6._
  import Example7._

  implicit def foldIsNumeric[A, B: Numeric]: Numeric[Fold[A, B]] = new Numeric[Fold[A, B]] {
    import Numeric.Implicits._

    override def plus(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ + _)
    override def minus(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ - _)
    override def times(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ * _)
    override def negate(x: Fold[A, B]) = x.map(- _)

    override def fromInt(x: Int) = foldApplicative[A].pure(implicitly[Numeric[B]].fromInt(x))

    override def toInt(x: Fold[A, B]) = ???
    override def toLong(x: Fold[A, B]) = ???
    override def toFloat(x: Fold[A, B]) = ???
    override def toDouble(x: Fold[A, B]) = ???
    override def compare(x: Fold[A, B], y: Fold[A, B]) = ???
  }

  implicit def foldIsFractional[A, B: Fractional]: Fractional[Fold[A, B]] = new Fractional[Fold[A, B]] {
    import Fractional.Implicits._

    override def plus(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ + _)
    override def minus(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ - _)
    override def times(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ * _)
    override def negate(x: Fold[A, B]) = x.map(- _)
    override def div(x: Fold[A, B], y: Fold[A, B]) = (x |@| y).map(_ / _)

    override def fromInt(x: Int) = foldApplicative[A].pure(implicitly[Numeric[B]].fromInt(x))

    override def toInt(x: Fold[A, B]) = ???
    override def toLong(x: Fold[A, B]) = ???
    override def toFloat(x: Fold[A, B]) = ???
    override def toDouble(x: Fold[A, B]) = ???
    override def compare(x: Fold[A, B], y: Fold[A, B]) = ???
  }

  {
    import Numeric.Implicits._
    println(fold(1 to 10)(length[Int] - foldIsNumeric[Int, Int].fromInt(1)))
  }

  {
    import Fractional.Implicits._

    println(fold(1.0 to 10.0 by 1.0)(sum[Double] / length))
  }
}

object Example9 extends App {
  import Fold._
  import Example7._

  def foldNested[I, O](input: Seq[Seq[I]])(f: Fold[I, O]): O = {
    val nestedReduced = input
      .par
      .map(ii => f.m.combineAll(ii.iterator.map(f.tally)))

    val reduced = nestedReduced.foldLeft(f.m.empty)(f.m.combine(_, _))

    f.summarize(reduced)
  }

  println(foldNested(List.fill(4)(1.0 to 100000.0 by 1.0))(average))
}

object Example10 extends App {

}

object Example11 extends App {
  import Fold._
  import Example3.length

  def focus[I1, I2, O](lens: Optional[I2, I1], f: Fold[I1, O]): Fold[I2, O] = Fold(f.m)(
    i => lens.getOption(i).map(f.tally).getOrElse(f.m.empty),
    f.summarize
  )
  def focus[I1, I2, O](lens: Lens[I2, I1], f: Fold[I1, O]): Fold[I2, O] = focus(lens.asOptional, f)
  def focus[I1, I2, O](lens: Prism[I2, I1], f: Fold[I1, O]): Fold[I2, O] = focus(lens.asOptional, f)

  def _1[A, B]: Lens[(A, B), A] = Lens[(A, B), A](_._1)(e => p => p.copy(_1 = e))
  println(fold(List((1, "x"), (2, "y"), (3, "z")))(focus(_1[Int, String], sum[Int])))

  def _Some[A]: Prism[Option[A], A] = Prism[Option[A], A](identity)(Some(_))
  def _None[A]: Prism[Option[A], Unit] = Prism[Option[A], Unit](_.fold(Option(()))(_ => None))(_ => None)
  def _Left[A, B]: Prism[Either[A, B], A] = Prism[Either[A, B], A](_.fold(Some(_), _ => None))(Left(_))
  def _Right[A, B]: Prism[Either[A, B], B] = Prism[Either[A, B], B](_.fold(_ => None, Some(_)))(Right(_))
  println(fold(List(Some(1), None, Some(4)))(focus(_Some[Int], sum[Int])))
  println(fold(List(Some(1), None, Some(4)))(focus(_None[Int], length[Unit])))
  println(fold(List[Either[Int, String]](Left(1), Right("x"), Left(10), Left(5), Right("y")))(focus(_Left[Int, String], sum[Int])))

  println(fold(List(Some((1, "x")), None, Some((10, "y"))))(focus(_Some.composeLens(_1[Int, String]), sum[Int])))
}

object CustomMonoids {
  import Ordering.Implicits._

  val unitMonoid: Monoid[Unit] = catsKernelStdAlgebraForUnit

  case class Max[A](v: A)
  def maxMonoid[A: Ordering](minValue: A): Monoid[Max[A]] = new Monoid[Max[A]] {
    override def empty = Max(minValue)
    override def combine(x: Max[A], y: Max[A]) = if (x.v < y.v) y else x
  }
  val maxIntMonoid = maxMonoid(Int.MinValue)

  def numProductMonoid[A: Numeric] = new Monoid[A] {
    override def empty = implicitly[Numeric[A]].one
    override def combine(l: A, r: A) = implicitly[Numeric[A]].times(l, r)
  }

  def firstMonoid[T] = new Monoid[Option[T]] {
    override def empty = None
    override def combine(l: Option[T], r: Option[T]) = l.orElse(r)
  }

  def lastMonoid[T] = new Monoid[Option[T]] {
    override def empty = None
    override def combine(l: Option[T], r: Option[T]) = r.orElse(l)
  }

  def andMonoid = new Monoid[Boolean] {
    override def empty = true
    override def combine(x: Boolean, y: Boolean) = x && y
  }

  def orMonoid = new Monoid[Boolean] {
    override def empty = false
    override def combine(x: Boolean, y: Boolean) = x || y
  }

  val intMonoid: Monoid[Int] = catsKernelStdGroupForInt
}