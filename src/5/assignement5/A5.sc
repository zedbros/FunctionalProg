import scala.annotation.tailrec

// EX1
// EX 1.1
def lengthString(l: List[String]): List[Int] = {
  val how_many = (x: String) => x.length
  val yis = l.map(how_many)
  yis
}
// Expected: List(3,4,3,3))
lengthString(List("How","long","are","we?"))

// EX 1.2
// Old
//def dup(l: Any, x: Int): List[Any] = {
//  val do_it = (el: Any) => el
//  val res = l map do_it
//  List(1,2,3)
//}
// corr
def dup[T](e : T, n : Int): List[T] = {
  List.range(1, n+1).map(x => e)
}
def dup2[T](e : T, n : Int): List[T] = {
  @tailrec
  def add(n: Int, l: List[Any]) : List[Any] = {
    if (n <= 0)
      l
    else
      add(n-1, 0::l)
  }
  add(n, List.empty).map(x => e)
}
// THIS ONE I REALLY LIKE
def nice_dup[T](e : T, n : Int): List[T] = {
  if (n <= 0)
    Nil
  else
    e :: dup(e, n-1)
}
// Expected: List("foo", "foo", "foo", "foo", "foo")
dup("foo", 5)
// Expected: List(List(1,2,3), List(1,2,3))
dup(List(1,2,3), 2)

// EX 1.3
def dot(l1: List[Int], l2: List[Int]): List[Int] = {
  require(l1.length == l2.length)
  val l = l1 zip l2
  val add = (el: (Int, Int))=> el(0) * el(1)
  l.map(add)
}
// Expected: List(2,8,9)
dot(List(1,2,3), List(2,4,3))





// EX2
// EX 2.1
def areTrue(l: List[Boolean]): Boolean = {
  l.foldRight(true)((x, y) => x && y)
}
// Expected false
areTrue(List(true, true, false))
// Expected true
areTrue(List(true, true, true))

// EX 2.2
def lString(l: List[String]): Int = {
  l.foldLeft("")((a, b) => a + b).foldLeft(0)((x, y) => x+1)
}
// Expected 12
lString(List("Folding", "is", "fun"))

// EX 2.3
def longest(l: List[String]): Int = {
  l.foldLeft(0)((a,b)=> if(a > lString(List(b))) a else lString(List(b)))
}
// Expected 8
longest(List("What", "is", "the", "longest?"))

// EX 2.4
def isPresent(l: List[Int], x: Int): Boolean = {
  l.foldLeft(false)((bool, nbr) => bool || (nbr==x))
}
def better_isPresent[T](l: List[T], e: T): Boolean = {
  l.foldLeft(false)((bool, v) => bool || (v==e))
}
//Expected false
isPresent(List(1,2,3,4), 5)
// Expected true
isPresent(List(1,2,3,4), 3)
//Expected false
better_isPresent(List(1,2,3,4), 5)
// Expected true
better_isPresent(List(1,2,3,4), 3)

// EX 2.5
// old
//def flattenList(l: List[Any]) = {
//  l.foldLeft(None)((a, b) => a)
//}
// corr
def flattenList(l: List[Any]): List[Any] = {
  l.foldLeft(List.empty)((acc, x) =>
    x match
      case xs: List[Any] => acc ::: flattenList(xs)
      case _ => acc :+ x
  )
}
flattenList(List(1,1))
// Expected List(1,1,2,3,5,8)
flattenList(List(List(1,1), 2, List(3, List(5, 8))))



//var a: Any = "bla"
//var b: String = "youpi"
//b = a.asInstanceOf[String]
//b