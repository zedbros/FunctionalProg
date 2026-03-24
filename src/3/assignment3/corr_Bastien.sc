import scala.annotation.tailrec

abstract class IntSet() {
  infix def add(x: Int): IntSet
  infix def contains(x: Int): Boolean

  // Exercise 1
  infix def foreach(f:Int=>Unit):Unit
  infix def foreachSorted(f:Int=>Unit):Unit

  // Exercise 2
  infix def union(other:IntSet):IntSet
  infix def intersection(other:IntSet):IntSet

  // Exercise 2
  infix def excl(x: Int): IntSet
  infix def +(x:Int) = add(x)
  infix def -(x:Int) = excl(x)
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet() {
  def add(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left add x, right)
    else if (x > elem) new NonEmpty(elem, left, right add x)
    else this
  }
  def contains(x: Int): Boolean = {
    if x < elem then left contains x
    else if x > elem then right contains x
    else true
  }

  // Exercise 1
  override def toString: String = s"(${left.toString}|$elem|${right.toString})"
  def foreach(f: Int => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  def foreachSorted(f: Int => Unit): Unit = {
    left.foreachSorted(f)
    f(elem)
    right.foreachSorted(f)
  }

  // Exercise 2
  def union(other:IntSet):IntSet = left.union(right.union(other.add(elem)))
  def intersection(other:IntSet):IntSet = {
    val base = left.intersection(other) union right.intersection(other)
    if other.contains(elem) then base.add(elem) else base
  }

  // Exercise 3
  def excl(x: Int): IntSet = if elem==x then left union right else left.excl(x) union right.excl(x) add elem
}

object Empty extends IntSet() {
  def contains(x: Int): Boolean = false
  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString: String = "-"

  // Exercise 1
  def foreach(f:Int=>Unit):Unit = ()
  def foreachSorted(f: Int => Unit): Unit = ()

  // Exercise 2
  def union(other:IntSet):IntSet = other
  def intersection(other:IntSet):IntSet = Empty

  // Exercise 3
  def excl(x: Int): IntSet = Empty
}

// Exercise 1
// 1.1
println(Empty)               // prints -
println(Empty.add(3))        // prints (-|3|-)
println(Empty.add(3).add(2)) // prints ((-|2|-)|3|-)
// 1.3
Empty.add(3).add(2).add(6).add(1) foreach (x => println(s"${x+1},"))
// 1.4
Empty.add(3).add(2).add(6).add(1) foreachSorted (x => println(s"${x+1},"))

// Exercise 2
val a = Empty.add(5).add(3).add(7)
val b = Empty.add(4).add(2).add(3).add(1).add(6).add(7).add(5)
// 2.1
a union b
// 2.2
a intersection b

// Exercise 3
// 3.1
a.excl(3)
b.excl(2).excl(6)
//3.2
def o1 = Empty + 3 + 4 + 12 + 5
val o2 = o1 - 3 - 4