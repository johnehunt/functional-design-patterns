package com.jeh.scala.pattern.functor

trait Functor2[T[A], A] {
  def fmap[B](f: A => B): T[B]
  val identity: A
}

case class ListFunctor2[A](identity: A, list: List[A]) extends Functor2[List,A]{
  def fmap[B](f: A => B): List[B] = list.map(f)
}

object Test2 extends App {
  val lf = ListFunctor2[Int](0, List(1,2))
  println(lf.fmap(_ + 1))
  println(lf.fmap(i => i + 1))
}

