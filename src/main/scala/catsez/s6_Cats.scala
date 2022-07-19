package catsez

import cats.data.{EitherT, OptionT}
import cats.syntax.applicative._
import cats.syntax.bifunctor._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.{Monad, Monoid}

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object s6_Cats {

  /************************************************************************
   * Show        *   show
   * Eq          *   === aka. 'eq'
   ************************************************************************
   * Semigroup   *   |+| aka. combine(A, A): A
   * Monoid      *   |+|, empty
   ************************************************************************
   * Functor     *   map
   * Applicative *   map, pure                                             <- 'product' will be covered in workshop #3
   * Monad       *   map, pure, flatMap
   * Bifunctor   *   leftMap, bimap, leftWiden
   ************************************************************************/


  // SUM (Semigroup & Monoid)

  val summedStrings: String = "a" |+| "b"

  def sumListGeneric[A: Monoid](list: List[A]): A =
    list.fold(Monoid[A].empty)(_ |+| _)

  // MONAD & MT

  case class UserId(value: UUID)
  case class AccountId(accountId: UUID)

  case class User(id: UserId)
  case class Account(id: AccountId)
  class Statement
  class Balance

  case class Report(user: User, account: Account, statements: List[Statement], balance: Balance)

  sealed abstract class Error
  case object UserNotFound extends Error
  case object NoAccount extends Error
  case object WrongAccountType extends Error // <- has no balance

  trait UserClientF[F[_]] {
    def getUser(id: UserId): F[Option[User]]
  }
  class UserClientFuture extends UserClientF[Future] {
    override def getUser(id: UserId): Future[Option[User]] = ???
  }
  class UserClientTry extends UserClientF[Try] {
    override def getUser(id: UserId): Try[Option[User]] = ???
  }

  trait AccountClientF[F[_]] {
    def getAccount(userId: UserId): F[Option[Account]]
  }
  class AccountClientFuture extends AccountClientF[Future] {
    override def getAccount(userId: UserId): Future[Option[Account]] = ???
  }
  class AccountClientTry extends AccountClientF[Try] {
    override def getAccount(userId: UserId): Try[Option[Account]] = ???
  }

  trait StatementClientF[F[_]] {
    def getStatements(accountId: AccountId): F[List[Statement]]
  }
  class StatementClientFuture extends StatementClientF[Future] {
    override def getStatements(accountId: AccountId): Future[List[Statement]] = ???
  }
  class StatementClientTry extends StatementClientF[Try] {
    override def getStatements(accountId: AccountId): Try[List[Statement]] = ???
  }

  trait BalanceClientF[F[_]] {
    def getBalance(accountId: AccountId): F[Option[Balance]]
  }
  class BalanceClientFuture extends BalanceClientF[Future] {
    override def getBalance(accountId: AccountId): Future[Option[Balance]] = ???
  }
  class BalanceClientTry extends BalanceClientF[Try] {
    override def getBalance(accountId: AccountId): Try[Option[Balance]] = ???
  }

  class ReportServiceOT[F[_]: Monad](
    userClient: UserClientF[F],
    accountClient: AccountClientF[F],
    statementClient: StatementClientF[F],
    balanceClient: BalanceClientF[F]
  ) {

    def getReportOT(userId: UserId): F[Option[Report]] =
      {
        for {
          user       <- OptionT(userClient.getUser(userId))
          account    <- OptionT(accountClient.getAccount(user.id))
          statements <- OptionT.liftF(statementClient.getStatements(account.id))
          balance    <- OptionT(balanceClient.getBalance(account.id))
        } yield Report(user, account, statements, balance)
      }.value

  }

  class ReportServiceET[F[_]: Monad](
    userClient: UserClientF[F],
    accountClient: AccountClientF[F],
    statementClient: StatementClientF[F],
    balanceClient: BalanceClientF[F]
  ) {
    def getReportE(userId: UserId): F[Either[Error, Report]] =
      {
        for {
          user       <- EitherT(userClient.getUser(userId).map(_.toRight(UserNotFound)))
          account    <- EitherT(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
          statements <- EitherT.liftF(statementClient.getStatements(account.id))
          balance    <- EitherT(balanceClient.getBalance(account.id).map(_.toRight(WrongAccountType))).leftWiden[Error]
        } yield Report(user, account, statements, balance)
      }.value
  }


  // BONUS CATS TIPS

  def curriedFunction[A](a1: A)(a2: A): A = ???

  object OptionConstructors {

    //val doesNotCompile1: Option[Int] = curriedFunction(None)(Some(1))
    //val doesNotCompile2: Option[Int] = curriedFunction(Some(1))(None)

    val compiles1: Option[Int] = curriedFunction(none[Int])(1.some)
    val compiles2: Option[Int] = curriedFunction(1.some)(none[Int])

  }

  object EitherConstructors {

    //val doesNotCompile1: Either[String, Int] = curriedFunction(Left(""))(Right(1))
    //val doesNotCompile2: Either[String, Int] = curriedFunction(Right(1))(Left(""))

    val compiles1: Either[String, Int] = curriedFunction("".asLeft[Int])(1.asRight)
    val compiles2: Either[String, Int] = curriedFunction(1.asRight[String])("".asLeft)

  }

  object LessParentheses {
    val manyParentheses: Future[Either[String, Option[Int]]] = Future.successful(Right(Some(1)))
    val noParentheses: Future[Either[String, Option[Int]]] = 1.some.asRight.pure[Future]
  }

  object FunctorSyntax {
    val insteadOfThis: Future[String] = Future(1).map(_ => "independent-value")
    val canDoThis: Future[String] = Future(1).as("independent-value")

    val mapToUnit: Future[Unit] = Future(1).map(_ => ())
    val void: Future[Unit] = Future(1).void
  }

  object FlatMapSyntax {
    def notify(a: Int) = Future(println(a))

    val insteadOfThis =
      for {
        _ <- notify(1)
        _ <- notify(2)
        _ <- notify(3)
      } yield ()

    val canDoThis = notify(1) >> notify(2) >> notify(3).void
  }

}
