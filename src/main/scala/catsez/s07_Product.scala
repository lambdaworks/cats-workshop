package catsez

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object s07_Product extends App {

  class Balance
  class Statement
  class Statistics

  def getBalance: Future[Balance] = ???
  def getStatements: Future[List[Statement]] = ???
  def getStatistics: Future[Statistics] = ???

  case class Account2(balance: Balance, statements: List[Statement])
  case class Account3(balance: Balance, statements: List[Statement], statistics: Statistics)

  def getAccount: Future[Account3] =
    for {
      balance <- getBalance
      statements <- getStatements
      statistics <- getStatistics
    } yield Account3(balance, statements, statistics)

  // the above translates to:
  def getAccount2: Future[Account3] =
    getBalance.flatMap { balance =>
      getStatements.flatMap { statements =>
        getStatistics.map(statistics => Account3(balance, statements, statistics))
      }
    }

  // THE PROBLEM:
  // In the above example getStatements does not actually rely on getBalance, and getStatistics does not actually
  // rely on the previous two, so this sequential flatMap chain is unnecessarily powerful for the purpose of
  // combining these values into a tuple required by Account3


  // THE SOLUTION: a new type-class we'll call Product

  // the Map type-class as seen in section #5
  trait Map[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  implicit class MapOps[F[_], A](fa: F[A])(implicit tc: Map[F]) {
    def map[B](f: A => B): F[B] = tc.map(fa)(f)
  }
  object Map {
    implicit val mapFuture: Map[Future] = new Map[Future] {
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }
    implicit val mapTry: Map[Try] = new Map[Try] {
      override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
    }
  }

  // the new type-class Product for combining F[A] and F[B] into F[(A, B)]
  // the name "product" becomes clear when thinking about type cardinality -> |F[(A, B)]| == |F[A]| x |F[B]|
  trait Product[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Product {
    def apply[F[_]](implicit P: Product[F]) = P

    implicit val productFuture: Product[Future] = new Product[Future] {
      override def product[A, B](fa: Future[A], fb: Future[B]) = fa.flatMap(a => fb.map((a, _)))
    }
    implicit val productTry: Product[Try] = new Product[Try] {
      override def product[A, B](fa: Try[A], fb: Try[B]) = fa.flatMap(a => fb.map((a, _)))
    }
  }
  implicit class ProductOps[F[_], A](fa: F[A])(implicit tc: Product[F]) {
    def product[B](fb: F[B]) = tc.product(fa, fb)
  }

  def getAccountP =
    //getBalance.product(getStatements).map { case (balance, statements) => Account2(balance, statements) }
    //getBalance.product(getStatements).map{ case (balance, statements) => Account2.tupled((balance, statements)) }
    //getBalance.product(getStatements).map(t => Account2.tupled(t))
    getBalance.product(getStatements).map(Account2.tupled)


  // gets trickier with larger tuples because it gets nested: ((A, B), C)
  def getAccountSP: Future[Account3] =
    getBalance.product(getStatements).product(getStatistics).map {
      case ((balance, statements), statistics) => Account3(balance, statements, statistics)
    }

  // abstractions for tuples of a specific size
  implicit class ProductOpsForTuple3[F[_]: Product: Map, A, B, C](t: (F[A], F[B], F[C])) {
    def tupled: F[(A, B, C)] = t match {
      case (fa, fb, fc) => fa.product(fb).product(fc).map {
        case ((a, b), c) => (a, b, c)
      }
    }
    def mapN[D](f: (A, B, C) => D): F[D] = tupled.map(f.tupled)
  }
  implicit class ProductOpsForTuple2[F[_]: Product: Map, A, B](t: (F[A], F[B])) {
    def tupled: F[(A, B)] = t match {
      case (fa, fb) => fa.product(fb)
    }
    def mapN[D](f: (A, B) => D): F[D] = tupled.map(f.tupled)
  }

  val t: Future[(Balance, List[Statement], Statistics)] = (getBalance, getStatements, getStatistics).tupled
  def getAccountT: Future[Account3] =
    t.map(Account3.tupled)

  def getAccountMap: Future[Account3] =
    (getBalance, getStatements, getStatistics).mapN(Account3)

  // Tips for finding all this in Cats:
  // The Product type-class implemented here aims to demonstrate `product` for which (in Cats) one would most likely use
  // the Applicative type-class, which also defines `pure` and extends Functor and Apply, where Functor defines `map`,
  // while Apply defines `ap` and implements `product` (inherited from Semigroupal) through `ap`.
  // For simplicity, here we focus only on `product` which in Cats is defined by the Semigroupal type-class.

  // In Cats, type-classes are structured hierarchically:
  // Monad <: Applicative <: Functor
  // This does not mean instances of Monad need to define `map` and `product` as well, given that these can be defined
  // through `flatMap` and `pure`. To demonstrate:


  trait Applicative[F[_]] {
    def pure[A](a: A): F[A]
    def map[A, B](a: F[A])(f: A => B): F[B]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    override def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => pure(f(a)))
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))
  }

}
