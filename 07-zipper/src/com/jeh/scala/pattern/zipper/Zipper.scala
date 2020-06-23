package com.jeh.scala.pattern.zipper

/**
 * Zippers support the traversal and possible mutation
 * of immutable data structures such as trees
 *
 * @tparam A
 */
trait Zipper[A] {

  // Import methods in Zipper object
  import Zipper._

  val focus: A // a particular node in a structure
  val left: Seq[A] // Sequence of elements to the left of the focus point
  val right: Seq[A] // Sequence of elements to the right of the focus point

  /**
   * Move the focus point to the next element
   * The exact semantics of this differs depending upon the underlying
   * data structure - e.g. a list versus a tree
   *
   * @return
   */
  def next: Option[Zipper[A]] = right match {
    case Nil => None
    case _ => Some(zipper(left :+ focus, right.head, right.tail))
  }

  /**
   * Move the focus point to the next element
   */
  def prev: Option[Zipper[A]] = left match {
    case Nil => None
    case _ => Some(zipper(left.init, left.last, focus +: right))
  }

  /**
   * Insert a new element at the focus point.
   * This Zipper is immutable and this a new Zipper is created.
   *
   * @param a element to be inserted
   * @return a New Zipper
   */
  def insert(a: A): Zipper[A] = zipper(left :+ focus, a, right)

  /**
   * Delete an element form the Zipper - return a new Zipper instance
   *
   * @return
   */
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

object Zipper {
  // Convenience method used to create a Zipper instance
  def zipper[A](l: Seq[A], f: A, r: Seq[A]): Zipper[A] = new Zipper[A] {
    val focus = f
    val left = l
    val right = r
  }

}

object Test extends App {
  import Zipper._

  val data = Seq("John", "Denise", "Phoebe", "Adam")
  val zip = zipper[String](l = Nil, f = data.head, r = data.tail)
  println(zip)
  println(zip.atStart)
  val zip2 = zip.next.get
  println(zip)
  println(zip2)
  println(zip2.atStart)
}


