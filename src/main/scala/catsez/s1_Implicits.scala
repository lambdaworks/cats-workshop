package catsez

import scala.language.implicitConversions

object s1_Implicits {

  // IMPLICIT RESOLUTION


  trait I

  object I {
    implicit val i: I = ???
  }


  val a: I = implicitly[I]



  //// IMPLICIT PARAMETERS

  def m(arg: Int)(implicit i: I): Int = ???

  m(1) // equivalent to -> m(1)(implicitly[I])


  //// DEPENDENT IMPLICITS

  trait DI
  object DI {
    implicit def di(implicit i: I): DI = ???
  }

  implicitly[DI]


  //// AMBIGUOUS IMPLICITS

  trait AI

  object AI {
    implicit val ai1: AI = ???
    implicit def ai2(implicit i: I): AI = ???
  }

  //implicitly[AI]


  //// IMPLICIT PRIORITIES

  trait P

  trait LowPriorityPValues {
    implicit val p2: P = ???
  }
  object P extends LowPriorityPValues {
    implicit def p1(implicit i: I): P = ???
  }

  implicitly[P]


  //// IMPLICIT CONVERSIONS

  trait From
  trait To
  implicit def convert(a: From): To = ???

  def m2(arg: To): Unit = ???

  m2(new From {}) // <- implicitly converts the argument of type From to the expected type To

  // implicit conversions not recommended, explicit import required to enable this feature


  //// IMPLICIT CLASSES

  implicit class StringListOps(list: List[String]) {
    def findMyCat: Int = list.indexOf("Mr. Fluffers")
  }


  val list = List("other-cat", "Mr. Fluffers", "yet-another-cat")

  val myCatIndex: Int = list.findMyCat // <- implicitly applies the constructor of StringListOps to make .findMyCat possible

}
