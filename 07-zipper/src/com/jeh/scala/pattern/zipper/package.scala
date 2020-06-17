package com.jeh.scala.pattern

package object zipper {
   def zipper[A](ls: Seq[A], a: A, rs: Seq[A]): Zipper[A] = new Zipper[A] {
    val focus = a
    val left = ls
    val right = rs
  }
}

