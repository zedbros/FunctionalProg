import scala.annotation.tailrec

def fib(x: Int): Int = {
  if (x==0 || x==1) 1 else fib(x-1) + fib(x-2)
}

//@tailrec
//def tailFib(x: Int, acc: Int): Int =
//  if (x == 0 || x==1) return acc
//  tailFib(x-1, acc)


fib(6)
//tailFib(6, 0)