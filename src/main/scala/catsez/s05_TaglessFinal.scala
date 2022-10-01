package catsez

import cats.syntax.either._
import cats.syntax.option._

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}

object s05_TaglessFinal {

  // TAGLESS-FINAL PATTERN

  //// MODEL

  case class UserId(value: UUID)
  case class AccountId(accountId: UUID)
  val someUserId: UserId = UserId(UUID.randomUUID())

  case class User(id: UserId)
  case class Account(id: AccountId)
  class Statement
  class Balance

  case class Report(user: User, account: Account, statements: List[Statement], balance: Balance)

  sealed abstract class Error
  case object UserNotFound extends Error
  case object NoAccount extends Error
  case object WrongAccountType extends Error // <- has no balance

  //// TRY INSTEAD OF FUTURE

  object ExampleWithTry {

    class UserClientTry {
      def getUser(id: UserId): Try[Option[User]] = ???
    }

    class AccountClientTry {
      def getAccount(userId: UserId): Try[Option[Account]] = ???
    }

    class StatementClientTry {
      def getStatements(accountId: AccountId): Try[List[Statement]] = ???
    }

    class BalanceClientTry {
      def getBalance(accountId: AccountId): Try[Option[Balance]] = ???
    }

    class ReportServiceTry(
      userClient: UserClientTry,
      accountClient: AccountClientTry,
      statementClient: StatementClientTry,
      balanceClient: BalanceClientTry
    ) {

      def getReport(userId: UserId): Try[Option[Report]] =
        userClient.getUser(userId).flatMap {
          case None =>
            Success(None)
          case Some(user) =>
            accountClient.getAccount(user.id).flatMap {
              case None =>
                Success(None)
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

  }

  // The Try-based service is almost identical to the Future-based one, so let's create a generic service instead

  //// POLYMORPHIC CLIENTS

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

  //// SOME MORE TYPE-CLASSES

  trait Map[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  implicit class MapOps[F[_], A](fa: F[A])(implicit tc: Map[F]) {
    def map[B](f: A => B): F[B] = tc.map(fa)(f)
  }
  object Map {
    implicit val mapTry: Map[Try] = new Map[Try] {
      override def map[A, B](`try`: Try[A])(f: A => B): Try[B] = `try`.map(f)
    }
    implicit val mapFuture: Map[Future] = new Map[Future] {
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }
  }

  trait FlatMap[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  implicit class FlatMapOps[F[_], A](fa: F[A])(implicit tc: FlatMap[F]) {
    def flatMap[B](f: A => F[B]): F[B] = tc.flatMap(fa)(f)
  }
  object FlatMap {
    implicit val flatMapTry: FlatMap[Try] = new FlatMap[Try] {
      override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    }
    implicit val flatMapFuture: FlatMap[Future] = new FlatMap[Future] {
      override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
    }
  }

  trait Pure[F[_]] {
    def pure[A](a: A): F[A]
  }
  implicit class PureOps[A](a: A) {
    def pure[F[_]](implicit tc: Pure[F]): F[A] = tc.pure(a)
  }
  object Pure {
    implicit val pureTry: Pure[Try] = new Pure[Try] {
      override def pure[A](a: A): Try[A] = Success(a)
    }
    implicit val pureFuture: Pure[Future] = new Pure[Future] {
      override def pure[A](a: A): Future[A] = Future.successful(a)
    }
  }


  //// REFACTORED SERVICE RELYING ON ABSTRACTIONS

  class ReportServiceF[F[_] : FlatMap : Pure : Map](
    userClient: UserClientF[F],
    accountClient: AccountClientF[F],
    statementClient: StatementClientF[F],
    balanceClient: BalanceClientF[F]
  ) {

    def getReport(userId: UserId): F[Option[Report]] =
      userClient.getUser(userId).flatMap {
        case None =>
          none[Report].pure
        case Some(user) =>
          accountClient.getAccount(user.id).flatMap {
            case None =>
              none[Report].pure
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

  //// USE THE SAME SERVICE WITH FUTURE OR TRY

  val reportFuture: Future[Option[Report]] =
    new ReportServiceF[Future](
      new UserClientFuture,
      new AccountClientFuture,
      new StatementClientFuture,
      new BalanceClientFuture
    ).getReport(someUserId)

  val reportTry: Try[Option[Report]] =
    new ReportServiceF[Try](
      new UserClientTry,
      new AccountClientTry,
      new StatementClientTry,
      new BalanceClientTry
    ).getReport(someUserId)



  //// OptionFuture, OptionTry -> OptionT

  case class OptionT[F[_] : Map : FlatMap : Pure, A](value: F[Option[A]]) {
    def map[B](f: A => B): OptionT[F, B] = OptionT(value.map(_.map(f)))

    def flatMap[B](f: A => OptionT[F, B]): OptionT[F, B] = OptionT(value.flatMap {
      case None => none[B].pure
      case Some(a) => f(a).value
    })
  }

  object OptionT {
    def liftF[F[_] : Map : FlatMap : Pure, A](fa: F[A]): OptionT[F, A] = OptionT(fa.map(Some(_)))
  }

  class ReportServiceOT[F[_] : FlatMap : Pure : Map](
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



  //// EitherFuture, EitherTry -> EitherT

  case class EitherT[F[_] : Map : FlatMap : Pure, E, A](value: F[Either[E, A]]) {
    def map[B](f: A => B): EitherT[F, E, B] = EitherT(value.map(_.map(f)))

    def flatMap[E1 >: E, B](f: A => EitherT[F, E1, B]): EitherT[F, E1, B] = EitherT(value.flatMap {
      case Left(e) => e.asInstanceOf[E1].asLeft[B].pure[F]
      case Right(a) => f(a).value
    })
  }

  object EitherT {
    def liftF[F[_] : Map : FlatMap : Pure, A](fa: F[A]): EitherT[F, Nothing, A] = EitherT[F, Nothing, A](fa.map(Right(_)))
  }

  class ReportServiceET[F[_] : FlatMap : Pure : Map](
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









  //// BIMAP

  trait BiMap[F[_, _]] {
    def rightMap[A, B, B1](fa: F[A, B])(f: B => B1): F[A, B1]
    def leftMap[A, A1, B](fa: F[A, B])(f: A => A1): F[A1, B]

    def bimap[A, A1, B, B1](fa: F[A, B])(lf: A => A1, rf: B => B1): F[A1, B1]
  }

  implicit class BiMapOps[F[_, _], A, B](fab: F[A, B])(implicit tc: BiMap[F]) {
    def leftMap[A1](f: A => A1): F[A1, B] = tc.leftMap[A, A1, B](fab)(f)
    def rightMap[B1](f: B => B1): F[A, B1] = tc.rightMap[A, B, B1](fab)(f)

    def bimap[A1, B1](fa: F[A, B])(lf: A => A1, rf: B => B1): F[A1, B1] = tc.bimap(fa)(lf, rf)

    def leftWiden[A1 >: A]: F[A1, B] = fab.asInstanceOf[F[A1, B]]
  }



  implicit val eitherBiMap: BiMap[Either] = new BiMap[Either] {
    override def rightMap[A, B, B1](either: Either[A, B])(f: B => B1): Either[A, B1] = either.map(f)

    override def leftMap[A, A1, B](either: Either[A, B])(f: A => A1): Either[A1, B] = either.left.map(f)

    override def bimap[A, A1, B, B1](either: Either[A, B])(lf: A => A1, rf: B => B1): Either[A1, B1] =
      either match {
        case Left(e) => Left(lf(e))
        case Right(a) => Right(rf(a))
      }
  }





  //implicit val eitherTBiMap: BiMap[EitherT] = ??? // <- does not compile because EitherT has 3 type parameters











  //// TYPE-LAMBDA

  // implicit def eitherTBiMap[F[_]]: BiMap[({type P[A, B] = EitherT[F, A, B]})#P] = ???


  // List[String], Future[Int    <- types
  // List, Future                <- type-constructors
  // [_], [_,_], [+_], [-_,+_]   <- kinds

  // we can use the kind-projector plugin to avoid writing type-lambdas explicitly

  //// KIND-PROJECTOR SYNTAX: [F, *, *]

  implicit def eitherTBiMap[F[_] : Map : FlatMap : Pure]: BiMap[EitherT[F, *, *]] =
    new BiMap[EitherT[F, *, *]] {
      override def rightMap[A, B, B1](fa: EitherT[F, A, B])(f: B => B1): EitherT[F, A, B1] = fa.map(f)

      override def leftMap[A, A1, B](fa: EitherT[F, A, B])(f: A => A1): EitherT[F, A1, B] =
        EitherT(fa.value.map(_.left.map(f)))

      override def bimap[A, A1, B, B1](fa: EitherT[F, A, B])(lf: A => A1, rf: B => B1): EitherT[F, A1, B1] =
        EitherT(fa.value.map {
          case Left(e) => Left(lf(e))
          case Right(a) => Right(rf(a))
        })
    }







  // EXAMPLE LIBRARIES BUILT IN THIS STYLE

  //// HTTP4S <- HTTP for Scala
  /*
    HttpRoutes.of[Task] {
      case GET -> Root / IntVar(id) =>
        getUser(id)
    }
   */

  //// FS2 <- functional streams for Scala
  /*
    val s: Stream[Task, String] = Stream.repeatEval(Task(readLine))
   */

  //// DOOBIE - SQL DB library
  /*
    query.transact[Task](transactor: Transactor[Task]): Task[_]
   */

}
