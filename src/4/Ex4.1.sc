def patFoo(x: Any): Boolean = {
  x match {
    case a: Int if (a % 4) == 0 => true
    case b: Char if (b == b.toUpper) => true
    case c: Boolean => true
    case _ => false
  }
}

patFoo(4)
patFoo('c')
patFoo('C')
patFoo(true)

