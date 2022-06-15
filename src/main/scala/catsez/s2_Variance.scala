package catsez

object s2_Variance {

  // TYPE-VARIANCE

  trait Animal
  trait Cat extends Animal


  //// COVARIANCE

  /*
       T[+A]

       A <: B ===> T[A] <: T[B]
   */

  val listOfAnimals: List[Animal] = List[Cat]()



  //// CONTRAVARIANCE

  /*
       T[-A]

       A <: B ===> T[B] <: T[A]
   */

  class Painter[-A] {
    def paint(a: A): Unit = ???
  }

  val catPainter: Painter[Cat] = new Painter[Animal]()



  //// CO- VS CONTRA- VS IN-VARIANT

  class MyType[A] {
    def get: A = ???             // <- this line would not compile for class MyType[-A]
    def put(a: A): Unit = ???    // <- this line would not compile for class MyType[+A]
  }

  //val my1: MyType[Animal] = new MyType[Cat]
  //val my2: MyType[Cat] = new MyType[Animal]

  //val animal: Animal = my1.get
  //my2.put(new Cat {})


  /*
    Different type arguments can have different variance:   Function1[-A, +B]
   */
}
