def square(x: Double) = x * x
def twice(x: Double) = 2.0 * x

def deriv(f: Double => Double, x: Double, dx: Double) =
  (f(x + dx) - f(x)) / dx

val dx = 0.00000000001

deriv(square, 3, dx)
deriv(twice, 3, dx)