package catsez

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object s04_Transformers {

  // NESTED MAPS AND FLAT-MAPS



  //// MODEL

  case class UserId(value: UUID)
  case class AccountId(accountId: UUID)

  case class User(id: UserId)
  case class Account(id: AccountId)
  class Statement
  class Balance

  case class Report(user: User, account: Account, statements: List[Statement], balance: Balance)


  //// DEPENDENCIES

  class UserClient {
    def getUser(id: UserId): Future[Option[User]] = ???
  }

  class AccountClient {
    def getAccount(userId: UserId): Future[Option[Account]] = ???
  }

  class StatementClient {
    def getStatements(accountId: AccountId): Future[List[Statement]] = ???
  }

  class BalanceClient {
    def getBalance(account: AccountId): Future[Option[Balance]] = ???
  }


  //// NAIVE APPROACH

  class ReportService(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReport(userId: UserId): Future[Option[Report]] =
      userClient.getUser(userId).flatMap {
        case None =>
          Future.successful(None)
        case Some(user) =>
          accountClient.getAccount(user.id).flatMap {
            case None =>
              Future.successful(None)
            case Some(account) =>
              statementClient.getStatements(account.id).flatMap { statements =>
                balanceClient.getBalance(account.id).map {
                  _.map { balance =>
                    Report(user, account, statements, balance)
                  }
                }
              }
          }
      }
  }



  //// OPTION-FUTURE

  case class OptionFuture[A](value: Future[Option[A]]) {
    def map[B](f: A => B): OptionFuture[B] = OptionFuture(value.map(_.map(f)))

    def flatMap[B](f: A => OptionFuture[B]): OptionFuture[B] = OptionFuture(value.flatMap {
      case None => Future.successful(None)
      case Some(a) => f(a).value
    })
  }

  object OptionFuture {
    def liftFuture[A](future: Future[A]): OptionFuture[A] = OptionFuture(future.map(Some(_)))
  }



  //// USING OPTION-FUTURE

  class ReportService2(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReport2(userId: UserId): Future[Option[Report]] =
      OptionFuture(userClient.getUser(userId)).flatMap { user =>
        OptionFuture(accountClient.getAccount(user.id)).flatMap { account =>
          OptionFuture.liftFuture(statementClient.getStatements(account.id)).flatMap { statements =>
            OptionFuture(balanceClient.getBalance(account.id)).map { balance =>
              Report(user, account, statements, balance)
            }
          }
        }
      }.value
  }



  //// OPTION-FUTURE FOR-COMP

  class ReportService3(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReport3(userId: UserId): Future[Option[Report]] =
      {
        for {
          user       <- OptionFuture(userClient.getUser(userId))
          account    <- OptionFuture(accountClient.getAccount(user.id))
          statements <- OptionFuture.liftFuture(statementClient.getStatements(account.id))
          balance    <- OptionFuture(balanceClient.getBalance(account.id))
        } yield Report(user, account, statements, balance)
      }.value



    //// COLUMNAR FORMATTING

    def getReport3C(userId: UserId): Future[Option[Report]] =
      {
        for {
          user       <- OptionFuture(                 userClient.getUser(userId)                     )
          account    <- OptionFuture(                 accountClient.getAccount(user.id)              )
          statements <- OptionFuture.liftFuture(      statementClient.getStatements(account.id)      )
          balance    <- OptionFuture(                 balanceClient.getBalance(account.id)           )
        } yield Report(user, account, statements, balance)
      }.value
  }



  //// TYPED ERRORS

  sealed abstract class Error

  case object UserNotFound extends Error
  case object NoAccount extends Error
  case object WrongAccountType extends Error // <- has no balance

  //// NAIVE APPROACH

  class ReportServiceE(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReportE(userId: UserId): Future[Either[Error, Report]] =
      userClient.getUser(userId).flatMap {
        case None =>
          Future.successful(Left(UserNotFound))
        case Some(user) =>
          accountClient.getAccount(user.id).flatMap {
            case None =>
              Future.successful(Left(NoAccount))
            case Some(account) =>
              statementClient.getStatements(account.id).flatMap { statements =>
                balanceClient.getBalance(account.id).map {
                  case None =>
                    Left(WrongAccountType)
                  case Some(balance) =>
                    Right(Report(user, account, statements, balance))
                }
              }
          }
      }
  }

  //// NAIVE APPROACH REFACTORED (to isolate boilerplate)

  class ReportServiceE2(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReportE2(userId: UserId): Future[Either[Error, Report]] =
      userClient.getUser(userId).map(_.toRight(UserNotFound)).flatMap {
        case Left(e) =>
          Future.successful(Left(e))
        case Right(user) =>
          accountClient.getAccount(user.id).map(_.toRight(NoAccount)).flatMap {
            case Left(e) =>
              Future.successful(Left(e))
            case Right(account) =>
              statementClient.getStatements(account.id).flatMap { statements =>
                balanceClient.getBalance(account.id).map(_.toRight(WrongAccountType)).map {
                  _.map { balance =>
                    Report(user, account, statements, balance)
                  }
                }
              }
          }
      }
  }



  //// EITHER-FUTURE

  case class EitherFuture[+E, A](value: Future[Either[E, A]]) {
    def map[B](f: A => B): EitherFuture[E, B] = EitherFuture(value.map(_.map(f)))

    def flatMap[E1 >: E, B](f: A => EitherFuture[E1, B]): EitherFuture[E1, B] = EitherFuture(value.flatMap {
      case Left(e) => Future.successful(Left(e))
      case Right(a) => f(a).value
    })
  }

  object EitherFuture {
    def liftFuture[E, A](future: Future[A]): EitherFuture[E, A] = EitherFuture(future.map(Right[E, A]))
  }

  //// USING EITHER-FUTURE

  class ReportServiceE3(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReportE3(userId: UserId): Future[Either[Error, Report]] =
      EitherFuture(userClient.getUser(userId).map(_.toRight(UserNotFound))).flatMap { user =>
        EitherFuture(accountClient.getAccount(user.id).map(_.toRight(NoAccount))).flatMap { account =>
          EitherFuture.liftFuture(statementClient.getStatements(account.id)).flatMap { statements =>
            EitherFuture(balanceClient.getBalance(account.id).map(_.toRight(WrongAccountType))).map { balance =>
              Report(user, account, statements, balance)
            }
          }
        }
      }.value
  }

  //// EITHER-FUTURE FOR-COMP

  class ReportServiceE4(
    userClient: UserClient,
    accountClient: AccountClient,
    statementClient: StatementClient,
    balanceClient: BalanceClient
  ) {
    def getReportE4(userId: UserId): Future[Either[Error, Report]] = {
      for {
        user       <- EitherFuture(userClient.getUser(userId).map(_.toRight(UserNotFound)))
        account    <- EitherFuture(accountClient.getAccount(user.id).map(_.toRight(NoAccount)))
        statements <- EitherFuture.liftFuture(statementClient.getStatements(account.id))
        balance    <- EitherFuture(balanceClient.getBalance(account.id).map(_.toRight(WrongAccountType)))
      } yield Report(user, account, statements, balance)
    }.value

    // COLUMNAR FORMATTING

    def getReportE4C(userId: UserId): Future[Either[Error, Report]] = {
      for {
        user       <- EitherFuture(            userClient.getUser(userId).map(_.toRight(UserNotFound))                )
        account    <- EitherFuture(            accountClient.getAccount(user.id).map(_.toRight(NoAccount))            )
        statements <- EitherFuture.liftFuture( statementClient.getStatements(account.id)                              )
        balance    <- EitherFuture(            balanceClient.getBalance(account.id).map(_.toRight(WrongAccountType))  )
      } yield Report(user, account, statements, balance)
    }.value
  }

}
