package com.jeh.scala.pattern.arrow

trait Arrow[A[-_, +_]] {
  def arrow[B, C](f: B => C): A[B, C]

  def compose[B, C, D](a1: A[B, C], a2: A[C, D]): A[B, D]

  def first[B, C, D](a: A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: A[B, C]): A[(D, B), (D, C)]
}

object Arrow {
  val Function1Arrow = new Arrow[Function1] {
    def arrow[B, C](f: B => C) = f

    def compose[B, C, D](a1: B => C, a2: C => D) =
      a2 compose a1

    def first[B, C, D](a: B => C) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: B => C) =
      (db: (D, B)) => (db._1, a(db._2))
  }
}

object test extends App {
  import Arrow._
  val f1 = (x: Int) => x / 2
  val a1 = Function1Arrow.arrow(f1)
  println(a1)
  println(a1(4))

  val f2 = (x: Int) => x * 3 + 1
  val a2 = Function1Arrow.arrow(f2)
  println(a2)
  println(a2(4))

  val a3 = Function1Arrow.compose(a1, a2)
  println(a3)
  println(a3(4))

}


