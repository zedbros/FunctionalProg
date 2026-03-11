abstract class Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}
class Number(n: Int) extends Expr {
  def isNumber = true
  def isSum = false
  def numValue: Int = n
  def leftOp: Expr = sys.error("Number, not a leftOp")
  def rightOp: Expr = sys.error("Number, not a rightOp")
}
class Sum(e1: Expr, e2: Expr) extends Expr {
  def isNumber = false
  def isSum = true
  def numValue: Int = sys.error("Calling num on a sum")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
}