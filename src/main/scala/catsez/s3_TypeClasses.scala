package catsez

import java.util.UUID

object s3_TypeClasses {

  // TYPE-CLASS PATTERN

  case class UserId(value: UUID)



  //// SHOW (type class example 1)

  def usingToString(userId: UserId): String =
    s"""{ "userId": "${userId.toString}"}"""


  trait Show[-A] {
    def show(a: A): String
  }
  implicit class ShowOps[A](a: A)(implicit showA: Show[A]) {
    def show: String = showA.show(a)
  }

  implicit val showUUID: Show[UUID] = new Show[UUID] {
    def show(uuid: UUID): String = uuid.toString
  }
  implicit def showUserId: Show[UserId] = new Show[UserId] {
    def show(userId: UserId): String = userId.value.show
  }

  def usingShow(userId: UserId): String =
    s"""{ "userId": "${userId.show}"}"""




  //// EQUALITY (type class example 2)

  def stdEquals(a: UUID, b: UUID): Boolean = a == b

  trait Eq[A] {
    def eq(a1: A, a2: A): Boolean
  }
  implicit class EqOps[A: Eq](a1: A) { // <- context-bound syntax [A: Eq]
    def ===(a2: A): Boolean = Eq[A].eq(a1, a2)
  }
  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq // <- type-class summoner

    implicit val eqForUUID: Eq[UUID] = _ == _
    implicit val eqForUserId: Eq[UserId] = _ == _
  }

  def smartEquals(a: UserId, b: UserId): Boolean = a === b



  //// SUM |+| (dependent type class instances)

  trait Sum[A] {
    def sum(a1: A, a2: A): A

    def empty: A
  }
  implicit class SumOps[A](a1: A)(implicit S: Sum[A]) {
    def |+|(a2: A): A = S.sum(a1, a2)
  }
  object Sum {
    def apply[A](implicit s: Sum[A]): Sum[A] = s

    implicit val sumForString: Sum[String] = new Sum[String] {
      override def sum(a1: String, a2: String): String = a1 + a2

      override def empty: String = ""
    }

    implicit def sumForOption[A](implicit SA: Sum[A]): Sum[Option[A]] = new Sum[Option[A]] {
      override def sum(a1: Option[A], a2: Option[A]): Option[A] =
        (a1, a2) match {
          case (None, None) => None
          case (None, Some(a)) => Some(a)
          case (Some(a), None) => Some(a)
          case (Some(a1), Some(a2)) => Some(a1 |+| a2)
        }

      override def empty: Option[A] = None
    }
  }

  val summedStrings: String = "a" |+| "b"

  val option1: Option[String] = Some("a")
  val option2: Option[String] = Some("b")

  val summedOptions: Option[String] = option1 |+| option2







  def sumList(list: List[Option[String]]): Option[String] =
    list.fold(None)(_ |+| _)






  def sumListGeneric[A: Sum](list: List[A]): A =
    list.fold(Sum[A].empty)(_ |+| _)


}
