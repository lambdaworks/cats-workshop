package catsez

import cats.Functor
import cats.data.EitherT
import cats.instances.future._
import cats.syntax.bifunctor._

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object s09_Nothing {

  //// MODEL

  case class UserId(value: UUID)
  case class AccountId(accountId: UUID)
  val someUserId: UserId = UserId(UUID.randomUUID())

  case class User(id: UserId)
  case class Account(id: AccountId)
  class Statement
  class Balance

  case class Report(user: User, account: Account, statements: List[Statement])

  sealed abstract class Error
  case object UserNotFound extends Error
  case object NoAccount extends Error

  class UserClient {
    def getUser(id: UserId): Future[Option[User]] = ???
  }

  class AccountClient {
    def getAccount(userId: UserId): Future[Option[Account]] = ???
  }

  class StatementClient {
    def getStatements(accountId: AccountId): Future[List[Statement]] = ???
  }

  // PROBLEM 1: The following in which liftF is used at the end of the flatMap chain does not compile because Nothing
  // is not inferred, so leftWiden's type constraint cannot be verified
  /*class ReportServiceP1(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient
  ) {
    def getReport(userId: UserId): Future[Either[Error, Report]] = {
        for {
          user       <- EitherT(userClient.getUser(userId).map(_.toRight(UserNotFound)))
          account    <- EitherT(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
          statements <- EitherT.liftF(statementClient.getStatements(account.id)).leftWiden[Error] // <- does not compile
        } yield Report(user, account, statements)
      }.value
  }*/

  // PROBLEM 2: even if we set the type explicitly to Nothing, the code still doesn't compile because resolving
  // implicit classes do not work well when Nothing is involved, so leftWiden cannot be applied
  /*class ReportServiceP2(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient
  ) {
    type Bottom <: Nothing
    def getReport(userId: UserId): Future[Either[Error, Report]] = {
      for {
        user       <- EitherT(userClient.getUser(userId).map(_.toRight(UserNotFound)))
        account    <- EitherT(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
        statements <- EitherT.liftF[Future, Nothing, List[Statement]](statementClient.getStatements(account.id)).leftWiden[Error]
      } yield Report(user, account, statements)
    }.value
  }*/

  // SOLUTION 1: fully explicit types
  class ReportServiceS1(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient
  ) {
    def getReport(userId: UserId): Future[Either[Error, Report]] = {
        for {
          user       <- EitherT(userClient.getUser(userId).map(_.toRight(UserNotFound)))
          account    <- EitherT(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
          statements <- EitherT.liftF[Future, Error, List[Statement]](statementClient.getStatements(account.id))
        } yield Report(user, account, statements) 
      }.value
  }

  // SOLUTION 2: create a leftF alternative using a custom subtype of Nothing for which leftWiden would work
  class ReportServiceS2(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient
  ) {

    implicit class EitherTConstructors(et: EitherT.type) {
      type Bottom <: Nothing
      def liftFN[F[_]: Functor, A](fa: F[A]): EitherT[F, Bottom, A] = et.liftF(fa)
    }

    def getReport(userId: UserId): Future[Either[Error, Report]] = {
        for {
          user       <- EitherT(userClient.getUser(userId).map(_.toRight(UserNotFound)))
          account    <- EitherT(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
          statements <- EitherT.liftFN(statementClient.getStatements(account.id)).leftWiden[Error]
        } yield Report(user, account, statements)
      }.value

  }

  // SOLUTION 3: create a leftF alternative accepting only the error type
  class ReportServiceS3(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient
  ) {

    implicit class EitherTConstructors(et: EitherT.type) {
      class LiftFE[E] {
        def apply[F[_] : Functor, A](fa: F[A]) = et.liftF[F, E, A](fa)
      }
      def liftFE[E] = new LiftFE[E]
    }

    def getReport(userId: UserId): Future[Either[Error, Report]] = {
        for {
          user       <- EitherT(userClient.getUser(userId).map(_.toRight(UserNotFound)))
          account    <- EitherT(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
          statements <- EitherT.liftFE[Error](statementClient.getStatements(account.id))
        } yield Report(user, account, statements)
      }.value

  }

}
