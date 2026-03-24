sealed abstract class Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

case class Product(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Product(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): Unit = e match {
  case Number(n) => print(n)
  case Sum(e1, e2) => print(eval(e1) + "+" + eval(e2))
  case Product(e1, e2) => if (e1.isInstanceOf[Sum] && e2.isInstanceOf[Sum]) print("(" + eval(e1) + ") * (" + eval(e2) + ")")
                          else if(e1.isInstanceOf[Sum]) print("(" + eval(e1) + ") * " + eval(e2))
                          else if(e2.isInstanceOf[Sum]) print(eval(e1) + " * (" + eval(e2) + ")")
                          else print(eval(e1) + "*" + eval(e2))
}

// Ex 1.3
val expr0 = Sum(Product(Number(2), Number(3)), Number(4))
println("Expr0: " + show(expr0))
assert(eval(expr0) == 10)

val expr1 = Product(Number(4), Number(12))
println("Expr1: " + show(expr1))
assert(eval(expr1) == 48)

val expr2 = Product(Sum(Number(2), Number(3)), Number(4))
println("Expr2: " + show(expr2))
assert(eval(expr2) == 20)

val expr3 = Product(Number(2), Sum(Number(3), Number(4)))
println("Expr3: " + show(expr3))
assert(eval(expr3) == 14)


// EX2
sealed abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

//def tree_eval(e: Expr): Int = e match {
//  case Leaf(l) => l
//  case Node(left, right) => tree_eval(left)
//}

def leafSum(b: BinaryTree): Int = {
  
}


leafSum(Node(Node(Leaf(3), Leaf(8)), Leaf(5))) // should give 16