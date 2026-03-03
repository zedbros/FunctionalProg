// Ex1
def val1() = 5.0
def val2() = 3.0

def square(x: Double) = x*x

square(val1())
square(val2())

square(val1()*val2())

def quad(x: Double) = square(square(x))

quad(val2())

def bar(x: Int, y: Boolean) = "Hello"

// Ex2
//Task1
def x0() = 2 // initial estimate AKA: x0
def thresh(): Float = 0.001 // when to stop

def f_sqrt(x: Double, wantedNumber: Double): Double = square(x) - wantedNumber
def f_prime_sqrt(x: Double): Double = 2*x
def abs(x: Double): Double = if(x >= 0) x else -x
def isGoodEnough(x: Double, wantedNbr: Double, thresh: Double): Boolean = if (abs(abs(wantedNbr) - square(x)) < thresh) true else false

def proceed(x: Double, wantedNbr: Double, thresh: Double): Double = {
  if(isGoodEnough(x, wantedNbr, thresh)) return x
  proceed(x - (f_sqrt(x, wantedNbr)/f_prime_sqrt(x)), wantedNbr, thresh)
}

def nbr() = 612

def sqrt(x: Double): Double = proceed(x0(), nbr(), thresh())

sqrt(nbr())
