class Rational(n:Int, d:Int) {
  require(d > 0)
  require(n >= 0)

  private def gcd(x: Int, y:Int): Int = {
    if(y==0) x else gcd(y, x%y)
  }
  private val g = gcd(n, d)

  def num = n
  def denum = d

  infix def +(that: Rational): Rational = {
    new Rational(num * that.denum + denum * that.num, denum * that.denum)
  }
  def ==(that: Rational): Boolean = {
    num == that.num && denum == that.denum
  }

  override
  def toString: String = s"${num} / ${denum}"


  // Ex 3.1
  // No neg because we decided to only work in positive Rationals
  def neg() = new Rational(-num, denum)

  def <(that: Rational): Boolean = {
    if (num*that.denum < denum / that.num) return true
    false
  }

  def max(x1: Rational, a: Array[Rational]) = {
    def proceed(m: Rational, i: Int): Rational = {
      if (i == a.length) if (x1 < a(i)) return a(i)
      if (m < a(i)) proceed(a(i), i + 1)
      else proceed(m, i + 1)
    }
  }
}

val r1 = new Rational(2,3)
r1.num
val r2 = new Rational(4,3)
r2.denum

//val r3 = r1.add(r2)
//print(r3)

r1 + r2
r1 < r2
r1 == r2

