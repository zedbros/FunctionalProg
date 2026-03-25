import scala.annotation.tailrec
// EX 1.1
// Foo!
// x1=4
// x2=4

// call by name form the => so corr:
// Foo!
// x1=4
// Foo!
// x2=4

// EX 1.2
def toUpper(s: String): String = {
  @tailrec
  def proceed(upperCaseString: String, pos: Int): String = {
    if(upperCaseString.length == s.length) return upperCaseString
    proceed(upperCaseString + s.charAt(pos).toUpper, pos + 1)
  }
  proceed("", 0)
}
toUpper("hello ma man")



// EX 2
def balanceMatch(chars: List[Char]): Boolean = {
// I tried to do leftPos rightPos but I know that doesn't work
    def proceed(pos: Int, parenth_count: Int): Boolean = {
        if(pos == chars.length) 
          if (parenth_count == 0)
            return true
          else 
          return false  
        chars(pos) match {
          case '(' => proceed(pos+1, parenth_count+1)
          case ')' => if(parenth_count <= 0) false else proceed(pos+1, parenth_count-1)
          case _ => proceed(pos+1, parenth_count)
        }
    }
    proceed(0, 0)
}
balanceMatch(List(')', 'a', '('))
balanceMatch(List('(', 'a', ')'))
balanceMatch(List('(', '(', 'a', ')'))
