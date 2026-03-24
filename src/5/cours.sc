val l = List(4, 1, 12, 7)

def max(x: Int, y: Int) = {
  val theMax = if(x>y) x else y
  println(s"We compared $x to $y and the max was $theMax")
  theMax
}

5 max 45

l.reduceLeft(max)


val l1 = List("abc", "def", "ghi")

l1.foldRight("")((a,b) => a + ";" + b)
// you can do a bunch of shit with the fold functions.
// Like here we did a concaténation of strings


// Ex5.1
List(1,2,3).foldRight(2)((x, y) => x - y)

