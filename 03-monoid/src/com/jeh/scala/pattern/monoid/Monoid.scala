package com.jeh.scala.pattern.monoid

trait Monoid[T] {
  def append(m1: T, m2: T): T
  val identity: T
}

class StringMonoid extends Monoid[String] {
  def append(s1: String, s2: String) = s1 + s2
  val identity = ""
}

// Another example of a concrete Monoid
object IntMonoid extends Monoid[Int] {
  def append(x: Int, y: Int) = x + y
  val identity = 0
}

object Test4 extends App {
  val stringMonoid = new StringMonoid()
  println(stringMonoid.append(stringMonoid.identity, "John"))
  println(stringMonoid.append("John", "Hunt"))

  println("--------------------")
  
  println(IntMonoid.append(IntMonoid.identity, 1))
  println(IntMonoid.append(1, 2))
  println(IntMonoid.append(2, 1))

}


