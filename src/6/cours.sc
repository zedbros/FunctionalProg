import scala.annotation.tailrec

def merge(xs: List[Int], ys: List[Int]): List[Int] = {
  (xs, ys) match {
    case (a, Nil) => a
    case (Nil, b) => b
    case (a, b) if b.head > a.head => a.head :: merge(a.tail, b)
    case (a, b) => b.head :: merge(a, b.tail)
  }
}

// 6.1
def filter[E](f: E => Boolean, xs: List[E]): List[E] = {
  xs match {
    case Nil => Nil
    case h :: t if f(h) => h :: filter(f, t)
    case _ :: t => filter(f, t)
  }
}

def partition[E](f: E => Boolean, xs: List[E]): (List[E], List[E]) = {
  // Can one line it:
 //  (filter(f, xs), filter((x) => !f(x), xs))
  @tailrec
  def proceed(xs: List[E], ys: List[E], zs: List[E]): (List[E], List[E]) = {
    xs match {
      case h :: t => if(f(h)) proceed(t, h::ys, zs) else proceed(t, ys, h::zs)
      case _ => (ys.reverse, zs.reverse)
    }
  }
  proceed(xs, List.empty, List.empty)
}

//def for[E](xs: List[E], f: E => Boolean)(body: E => Unit): Unit = {
//  xs match{
//    case Nil = ()
//    case h::t =>
//      if(f(h)) body(h)
//      for(t, f)(body)
//  }
//}

@main def M = {
  val a = List(1, 3, 5, 7, 9)
  val b = List(2, 3, 4, 5, 6)
  println("Merge:     " + merge(a, b))
  println("Filter:    " + filter[Int](x => x >= 5, a))
  println("Partition: " + partition[Int](_ > 3, a))

  //  for(List(1,2))
  
  // EX 6.2
  val ps = for (
    x: Char <- 'a' to 'h';
    y: Int <- 1 to 8
  ) yield (x.toString, y.toString)
  println(ps)

  val ps_v2 = for (
    row <- 1 to 8;
    col <- 'a' to 'h'
  ) yield (col, row)
  println(ps_v2)
}

M
