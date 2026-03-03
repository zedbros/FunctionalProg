import scala.annotation.tailrec
// EX1
def v1(): Double = 8.0
def fib(x: Double): Double = {
  if (x == 0) return 0.0
  if (x == 1) return 1.0
  fib(x-1) + fib(x-2)
}
@tailrec
def tail_fib(x: Double, x1: Double, x2: Double): Double = {
  if (x == 1) return x2
  tail_fib(x-1, x2, x1+x2)
}

fib(v1())
tail_fib(v1(), 0, 1)


// EX2
def v2(): Int = 9
def sum(a: Int, b: Int, f: Int => Double): Double = {
  require(a <= b)
  @tailrec
  def iter(a: Int, acc: Double): Double = {
    if (a == b+1) return acc
    iter(a + 1, acc + f(a))
  }
  iter(a, 0)
}

sum(0, v2(), x=>x)


// EX3
// 3.1
def prod(a: Double)(b: Double)(f: Double => Double): Double = {
  require(a <= b)
  @tailrec
  def proceed(a: Double, acc: Double): Double = {
    if(a==b+1) return acc
    proceed(a+1, acc*f(a))
  }
  proceed(a, 1)
}
prod(v1())(v2())(x=>x)

// 3.2
def fact(n: Int): Int = { // damn one shot dat shit
  prod(1)(n)(identity).toInt
}

fact(v2())

// 3.3
def mapReduce(a: Double, b: Double)(loop: (Double, Double) => Double)(f: Double => Double): Double = {
  require(a <= b)
  @tailrec
  def proceed(a: Double, acc: Double): Double = {
    if(a==b+1) return acc
    proceed(a+1, loop(f(a), acc))
  }
  proceed(a, if (loop(0,1)==0) 1 else 0)
}

def new_sum(a: Double, b: Double): Double = {
  mapReduce(a, b)((x1, x2) => x1+x2)(identity)
}
def new_mult(a: Double, b: Double): Double = {
  mapReduce(a, b)((x1, x2) => x1*x2)(identity)
}

new_sum(3, 4)
new_mult(3, 4)

mapReduce(1, 5)((x1, x2) => x1+x2)(x => x * 2.0 + 1.0)