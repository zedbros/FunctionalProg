import scala.collection.immutable.ArraySeq
//trait Nat extends Addable[Nat]
//object Zero extends Nat
//case class Succ(n: Nat) extends Nat
//
//class Complex(a: Double, b: Double) extends Addable[Complex]:
////  println("Yes")
//  def plus() = ???
//
//trait Addable[A]:
//  def plus(other: A): A
//  def double(): A = this.plus(this)


// Ex 7.1
case class Dalton(x: Int) extends Ordered[Dalton]:
  def compare(that: Dalton): Int = {
    this.x - that.x
  }
val a = Dalton(2)
val b = Dalton(3)
val c = Dalton(2)
a < b
a > b
a <= b
a <= c
a <= c


class Set[E <: Ordered[E]](private val contents: ArraySeq[E] = ArraySeq[E]()):
   def including(x: Int): ArraySeq[E] =
     val i = partitioningIndex(x)
     ArraySeq(contents.prefix(i) ++ x ++ contents.drop(i))

   def contains(x: Int): Boolean =
     val i = partitioningIndex(x)
     (i < contents.length) && (contents(i) == x)

   def partitioningIndex(x: Int): Int =
     def proceed(lo: Int, hi: Int): Int =
       if lo == hi then
         lo
       else
         val m = (hi-lo) / 2
         if x < contents(m) then proceed(lo, m) else proceed(m, hi)
     proceed(0, contents.length - 1)

val l = List(1,2,3,4)

