// CC Blanc 2025
// EX3
// 3.1
def findTrues(l: List[Boolean]): Int = {
  l.foldLeft(0)((a,b) => if(b) a+1 else a)
}
findTrues(List(true, false, false, true, false))
// 3.2
def remDup(l: List[Any]): List[Any] = {
  l.foldLeft(List.empty)((a, b) => if(a.contains(b)) a else b::a)
}
// Expected List(5,3,2,4)
remDup(List(5,3,2,4,3,2,3,3))
// Expected List("hello", "youpi")
remDup(List("hello", "youpi", "hello", "hello"))