package com.xammel.lectures

object AdvancedPatternMatching {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => s"only elem is $head"
    case _           => "empty"
  }

  /*
  - constants
  - wildcards
  - case classes
  - tuples
  - some special magic like above
   */

  class Person(val name: String, val age: Int)
  object Person {
    // normal
    def unapply(arg: Person): Option[(String, Int)] = Some(arg.name, arg.age)
    // can overload
    def unapply(age: Int): Option[String] = Some(if (age < 18) "minor" else "major")
  }

  val person = new Person("me", 50)
  val normalMatch = person match {
    case Person(n, a) => s"here it is"
  }

  val overloadedMatch = person.age match {
    case Person(status) => status // "major"
  }

  /*
  Ex
   */
  val n: Int = 46
  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "an even number"
    case _ => "no property"
  }

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }
  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10
  }

  val customMatchingSolution = n match {
    case even()        => "string for even"
    case singleDigit() => "string for single digit"
  }

  // infix patterns
  case class Or[A, B](a: A, b: B)
  val either: Or[Int, String] = Or(2, "two")

  val humanDesc = either match {
    case Or(number, string) => s"$number is written as $string"
    case number Or string   => s"same as above...."
  }
  // only works when you have 2 things in the pattern

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A         = ???
    def tail: MyList[A] = ???
  }
  case object Empty                                                       extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1, 2"
    case MyList(1, 2, 3)  => "contains with 1, 2, 3"
  }

  // custom return types for unapply
  // doesn't need to be Option
  // just needs to be a type that has the following two methods...
  // isEmpty: Boolean
  // get: something
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = person.name
    }
  }

  val customTypeMatching = person match {
    case PersonWrapper(name: String) => name
  }

  def main(args: Array[String]): Unit = {
    println(customMatchingSolution)

    println(decomposed)
  }
}
