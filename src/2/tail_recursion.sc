import scala.annotation.tailrec

def fact(x: Int): Int = {
  @tailrec
  def factTail(n: Int, acc: Int): Int =
    if (n == 1) acc else factTail(n - 1, acc * n)
  factTail(x, 1)
}
fact(4)