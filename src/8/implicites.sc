
trait Ordering[A]:
  def lessThan(lhs: A, rhs: A): Boolean

given f[A](using w: Ordering[A]): Ordering[List[A]] = new Ordering[List[A]] {
  def lessThan(lhs: List[Int], rhs: List[Int]): Boolean =
    (lhs, rhs) match
      case (x :: xs, y :: ys) =>
        if w.lessTHan(x, y) then
}

def min[A](a: A, b: A)(using w: Ordering[A]): A =
  if w.lessThan(b, a) then b else a

@main def Main =
  given w: Ordering[Int] = new Ordering[Int] {
    def lessThan(lhs: Int, rhs: Int): Boolean = lhs < rhs
  }
  println(min(1,2))

  // Using "using" makes it become implicit
