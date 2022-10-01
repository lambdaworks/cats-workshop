package catsez

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._

import java.util.UUID
import scala.util.Try

object s08_Traverse {


  // THE PROBLEM

  val strings: List[String] = ???

  val oh_no: List[Try[UUID]] = strings.map(s => Try(UUID.fromString(s)))




  // TRAVERSE

  trait Traverse[F[_]] {
    def traverse[A, B, G[_]: Applicative](fa: F[A])(f: A => G[B]): G[F[B]]
  }
  implicit class TraverseOps[F[_], A](fa: F[A])(implicit T: Traverse[F]) {
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = T.traverse(fa)(f)
  }

  implicit val traverseForList: Traverse[List] = new Traverse[List] {
    override def traverse[A, B, G[_]: Applicative](list: List[A])(f: A => G[B]): G[List[B]] =
      list.foldRight(List.empty[B].pure[G]) { case (a, aggr) =>
        (aggr, f(a)).mapN { case (l, fa) => fa :: l }
      }.map(_.reverse)
  }



  // PROBLEM SOLVED (just replace 'map' with 'traverse')

  val oh_yes: Try[List[UUID]] = strings.traverse(s => Try(UUID.fromString(s)))



  // SEQUENCE (added after the workshop)
  // `sequence` is useful when there is no function to apply because we already have an F[G[A]]
  // that we just want to turn into a G[F[A]]

  implicit class SequenceOps[F[_]: Traverse, G[_]: Applicative, A](fa: F[G[A]]) {
    def sequence: G[F[A]] = fa.traverse(identity)
  }

  val before: List[Try[String]] = ???

  val after: Try[List[String]] = before.sequence

  // Note:
  // In this example `sequence` is defined in a separate implicit class relying on `Traverse`.
  // In Cats, `sequence` is just another method of the `Traverse` type-class.


}
