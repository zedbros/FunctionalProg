abstract class IntSet() {
  infix def add(x:Int):IntSet
  infix def contains(x:Int):Boolean

  infix def foreach(f:Int=>Unit):Unit

  infix def union(other:IntSet):IntSet
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
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def union(other: IntSet): IntSet = {
    foreach(other)
  }
}

object Empty extends IntSet() {
  def contains(x: Int): Boolean = false

  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString: String = "-"

  def foreach(f: Int => Unit): Unit = {}
  def union(other: IntSet): IntSet = Empty
}


// EX 1
println(Empty)
println(Empty.add(3))
println(Empty.add(3).add(2))

def s1 = Empty
def s2 = Empty.add(3)
def s3 = Empty.add(3).add(2)

s1.foreach(println)
s2.foreach(println)
s3.foreach(println)

(Empty.add(3).add(2).add(6).add(1)) foreach (x => print(x+1 + ", "))
// Gives 4, 3, 2, 7

// Because it will first iterate on the leftmost element's that's given to it,
// so therefor by starting with 3, will add 1, go to two and add, then 6,
// but 7 is on the right, so skips, goes to 1 and adds 1.. later comes back
// to six and adds.
// I tried switching right left lines foreach and yes does the exact opposite,
// goes for the right or bigger numbers first.

// EX2
def s4 = Empty.add(5)
def s5 = Empty.add(6).add(7)

s4.union(s5)