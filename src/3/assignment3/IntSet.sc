abstract class IntSet() {
  infix def add(x: Int): IntSet
  infix def contains(x: Int): Boolean
  infix def foreach(f: Int => Unit): Unit
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet() {
  def add(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left add x, right)
    else if (x > elem) new NonEmpty(elem, left, right add x)
    else this
  }

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def toString: String = s"($left | $elem | $right)"

  def foreach(f: Int => Unit): Unit = {
    f(left)
    f(elem)

  }
}

object Empty extends IntSet() {
  def contains(x: Int): Boolean = false
  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString: String = "-"

  def foreach(f: Int => Unit): Unit = {
    f
  }
}

println(Empty)
println(Empty.add(3))
println(Empty.add(3).add(2))

def s1 = Empty
def s2 = Empty.add(3)
def s3 = Empty.add(3).add(2)

s1.foreach(println)
s2.foreach(println)
s3.foreach(println)
