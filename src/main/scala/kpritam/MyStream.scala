package kpritam

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](elem: B): MyStream[B]
  def ++[B >: A](anotherStream: ⇒ MyStream[B]): MyStream[B]

  def foreach(f: A ⇒ Unit): Unit
  def map[B](f: A ⇒ B): MyStream[B]
  def flatMap[B](f: A ⇒ MyStream[B]): MyStream[B]
  def filter(predicate: A ⇒ Boolean): MyStream[A]

  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if(isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

object MyStream {
  def from[A](start: A)(generator: A ⇒ A): MyStream[A] =
    new Cons(start, from(generator(start))(generator))
}

object EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](elem: B): MyStream[B] = new Cons(elem, this)
  override def ++[B >: Nothing](anotherStream: ⇒ MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing ⇒ Unit): Unit = Unit
  override def map[B](f: Nothing ⇒ B): MyStream[B] = this
  override def flatMap[B](f: Nothing ⇒ MyStream[B]): MyStream[B] = this
  override def filter(predicate: Nothing ⇒ Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = throw new NoSuchElementException
  override def takeAsList(n: Int): List[Nothing] = throw new NoSuchElementException
}

class Cons[A](h: A, t: ⇒ MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = false
  override val head: A = h
  override lazy val tail: MyStream[A] = t

  override def #::[B >: A](elem: B): MyStream[B] = new Cons(elem, this)
  override def ++[B >: A](anotherStream: ⇒ MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  override def foreach(f: A ⇒ Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A ⇒ B): MyStream[B] = new Cons(f(head), tail.map(f))
  override def flatMap[B](f: A ⇒ MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A ⇒ Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  override def take(n: Int): MyStream[A] =
    if(n < 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))

}

object MyStreamExample extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.#::(0)
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  // map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())
  println(startFrom0.filter(_ < 10).take(10).take(20).toList())

}