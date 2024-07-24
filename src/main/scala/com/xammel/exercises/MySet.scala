package com.xammel.exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  override def apply(elem: A): Boolean = contains(elem)
  /*
  Implement a functional set
   */
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(set: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
  def -(elem: A): MySet[A]               // remove
  def --(anotherSet: MySet[A]): MySet[A] // difference
  def &(anotherSet: MySet[A]): MySet[A]  // intersection
  def unary_! : MySet[A]
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = valSeq match {
      case Seq() => acc
      case seq   => buildSet(seq.tail, acc + seq.head)
    }
    buildSet(values, new EmptySet[A])
  }
}

// all elements of type A which satisfy a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {

  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)

  override def ++(set: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || set(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def foreach(f: A => Unit): Unit = politelyFail

  override def -(elem: A): MySet[A] = filter(_ != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole")
}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(set: MySet[A]): MySet[A] = set

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] = {
    if (this.contains(elem)) this else new NonEmptySet[A](elem, this)
  }

  override def ++(set: MySet[A]): MySet[A] = tail ++ set + head

  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = {
    if (head == elem) tail
    else tail - elem + head
  }

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MyTest {

  val s: MySet[Int] = MySet(1, 2, 3, 4)
  def main(args: Array[String]): Unit = {
    s.foreach(println)
    (s + 5).foreach(println)
    (s ++ s + 5).foreach(println)
    s.map(x => x * 10).foreach(println)
    s.flatMap(x => MySet(x * 10)).foreach(println)

    println("-" * 30)
    val neg = !s // s.unary_!
    println(neg(2), neg(5))
  }
}
