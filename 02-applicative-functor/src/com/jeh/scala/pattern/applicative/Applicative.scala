package com.jeh.scala.pattern.applicative

trait Functor[T[_], A] {
  def fmap[B](f: A => B): T[B]
  val identity: A
}

trait ApplicativeFunctor[T[_], A] extends Functor[T, A] {
  def apply(tf: T[A => A]): T[A]
}

abstract class ListApplicativeFunctor[A](list: List[A]) extends ApplicativeFunctor[List, A] {
  def fmap[B](f: A => B): List[B] = list.map(f)
  def apply(tf: List[A => A]): List[A] = {
    var l: List[A] = list
    tf.foreach(f => l = l.map(f))
    return l
  }
}

case class IntListAppFunctor(list: List[Int], identity: Int = 0)
                                                     extends ListApplicativeFunctor[Int](list)

object IntListAppFunctor {
  implicit def pure(a: Int) = IntListAppFunctor(List[Int](a))
}

object Test3 extends App {

  val af = IntListAppFunctor(List(1, 2))
  val increase = (x: Int) => x + 1
  val double = (x: Int) => x * 2

  println(af.apply(List(increase)))
  println(af.apply(List(increase, double)))

  println(IntListAppFunctor.pure(23))

}
