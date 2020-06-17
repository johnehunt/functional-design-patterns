package com.jeh.scala.pattern.zipper

trait Zipper[A] {
  val focus: A
  val left: Seq[A]
  val right: Seq[A]

  def next: Option[Zipper[A]] = right match {
    case Nil => None
    case _ => Some(zipper(left :+ focus, right.head, right.tail))
  }
  def prev: Option[Zipper[A]] = left match {
    case Nil => None
    case _ => Some(zipper(left.init, left.last, focus +: right))
  }
  def insert(a: A): Zipper[A] = zipper(left :+ focus, a, right)
  def delete: Option[Zipper[A]] = right match {
    case Nil => None
    case _ => Some(left match {
      case Nil => zipper(Nil, right.head, right.tail)
      case _ => zipper(left.init, left.last, right)
    })
  }

  def atStart: Boolean = left.isEmpty
  def atEnd: Boolean = right.isEmpty
  
  override def toString = "Zipper: " + focus + " (left: " + left + ") (right: " + right + ")" 
}

object Test extends App {
  val data = Seq("John", "Denise", "Phoebe", "Adam")
  var zip = zipper[String](Nil, data.head, data.tail)
  println(zip)
  println(zip.atStart)
  zip = zip.next.get
  println(zip)
  println(zip.atStart)
  zip = zip.prev.get
  println(zip)
  println(zip.atStart)
}


