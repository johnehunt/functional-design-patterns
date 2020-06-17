package com.jeh.scala.pattern.functor

trait Functor[T[_]] {
  def fmap[A, B](list: List[A])(f: A => B): List[B]
}

object ListFunctor extends Functor[List] {
  def fmap[A, B](list: List[A])(f: A => B): List[B] = list map f
}

object Test extends App {
  val l1 = List(1, 2)
  val result = ListFunctor.fmap(l1)((i: Int) => i + 1)
  println(result)
}

