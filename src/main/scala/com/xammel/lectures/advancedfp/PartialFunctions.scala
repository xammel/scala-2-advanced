package com.xammel.lectures.advancedfp

object PartialFunctions {

  class FunctionNotApplicableException extends RuntimeException

  val aFunction = (x: Int) => x + 1
  // nasty way to restrict valid inputs
  val aFussyFunc = (x: Int) => if (x == 1) 42 else if (x == 2) 44 else throw new FunctionNotApplicableException
// nicer
  val aNicerFussyFunc = (x: Int) =>
    x match {
      case 1 => 42
      case 2 => 44
      case _ => throw new FunctionNotApplicableException
    }
  // We are defining a partial function ... {1,2} => Int
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 44
  }

  // PF utils
  val isIt: Boolean = aPartialFunction.isDefinedAt(6) // false

  // lift
  val lifted: Int => Option[Int] = aPartialFunction.lift // total function from int to option

  val pfChain = aPartialFunction.orElse[Int, Int] { case 5 =>
    67
  }

  // PFs extend normal functions
  val aTotalFunction: Int => Int = { case 1 =>
    99
  }

  // Higher Order Functionss accept PFs as well, becuase PF is a subtype of a total function
  val aMappedList = List(1, 2).map(aPartialFunction)

  /*
  Note: PFs can only have 1 parameter type
   */


  def main(args: Array[String]): Unit = {
    println(aPartialFunction(2))
    // println(aPartialFunction(5)) // MatchError
    println(lifted(5))  // None
    println(pfChain(5)) // 67

    println(aMappedList)
  }
}
