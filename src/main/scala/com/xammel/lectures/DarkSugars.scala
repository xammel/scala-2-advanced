package com.xammel.lectures

import scala.util.Try

object DarkSugars {

  /** syntax sugar #1: methods with single param
    */
  def singleArgMethod(arg: Int): String = s"$arg things"

  // can call with curly braces
  val description = singleArgMethod {
    // write some code here
    1
  }

  // like try
  val aTryInstance = Try {
    // stuff
  }

  // map is also an example of this
  val incremented_v1 = List(1, 2, 3).map { x => x + 1 }
  val incremented_v2 = List(1, 2, 3).map(f = (x: Int) => x + 1)

  /** syntax sugar #2: single abstract method
    */
  trait Action {
    def act(x: Int): Int
  }

  // classic
  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }
  // magic way
  val anInstance_v2: Action = (x: Int) => x + 1

  // example of Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hi there")
  })
  val aNicerThread = new Thread(() => println("sweet"))

  /** syntax sugar #3: the :: and #:: methods
    */

  val prependedList = 2 :: List(3, 4)
  // rewritten as List(3,4).::(2)
  // associatiity is determined by the operators last char
  // if ends in a :, it's left associative

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
    def :<--(value: T): MyStream[T] = this
  }

  val stream = new MyStream[Int]
  val woo    = 1 -->: stream
  val foo    = stream :<-- 2

  /** syntax sugar #4: multi-word method naming...
    */

  class TeenGirl(name: String) {
    def `and then said`(goss: String) = println(s"$name said $goss")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "woohoo" // compiles

  /** syntax sugar #5: infix types
    */

  class Composite[A, B]
  val composite: Composite[Int, String] = new Composite[Int, String]
  val infixed: Int Composite String     = new Composite[Int, String] // compiles!

  /** syntax sugar #6 : update method is special, like apply
    */

  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // rewritten to anArray.update(2,7)
  // used in mutable collections

  /** syntax sugar #7: setters for mutable containers
    */

  class Mutable {
    private var internalMember: Int = 0

    def member                     = internalMember // "getter"
    def member_=(value: Int): Unit = {
      internalMember = value // setter
    }
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // rewritten as aMutableContainer.member_=(42)

  def main(args: Array[String]): Unit = {}

}
