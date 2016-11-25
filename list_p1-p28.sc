// P01
def last(l: List[Int]): Int = {
  if (l.tail.isEmpty) l.head
  else last(l.tail)
}
last(List(1, 1, 2, 3, 5, 8))


// P02
def penultimate(l: List[Int]): Int = {
  if (l.tail.tail.isEmpty) l.head
  else penultimate(l.tail)
}
penultimate(List(1, 1, 2, 3, 5, 8))


// P03
def nth(n: Int, l: List[Int]): Int = {
  if (n == 0) l.head
  else nth(n-1, l.tail)
}
nth(2, List(1, 1, 2, 3, 5, 8))


// P04
def length(l: List[Int]): Int = {
  def recur(n: Int, l: List[Int]): Int = {
    if (l.isEmpty) n
    else recur(n+1, l.tail)
  }
  recur(0, l)
}
length(List(1, 1, 2, 8))


// P05
def reverse(l: List[Int]): List[Int] = {
  def recur(newL: List[Int], oldL: List[Int]): List[Int] = {
    if (oldL.isEmpty) newL
    else recur(oldL.head::newL, oldL.tail)
  }
  recur(List(), l)
}
reverse(List(1, 1, 2, 3, 5, 8))


// P06
def isPalindrome(l: List[Int]): Boolean = reverse(l) == l
isPalindrome(List(1, 2, 3, 4, 5))
isPalindrome(List(1, 2, 3, 2, 1))


// P07
def flatten(l: List[Any]): List[Any] = {
  def recur(l: List[Any],x: List[Any]): List[Any] = {
    if (l.isEmpty) x
    else if (l.head.isInstanceOf[List[Any]]) recur(l.tail, x:::recur(l.head.asInstanceOf[List[Any]], List()))
    else recur(l.tail, x:+l.head)
  }
  recur(l, List()).asInstanceOf[List[Int]]
}
flatten(List(List(1, 1), 2, List(3, List(5, 8))))
flatten(List(1,2,3,List(1,2,3,List(1),List(5,6,7,List(8,List(9,List(10)))))))



// P08
def compress(l: List[Symbol]): List[Symbol] = {
  def hasChar(l: List[Symbol], ch: Symbol): Boolean = {
    if (l.isEmpty) false
    else if (l.head == ch) true
    else hasChar(l.tail, ch)
  }
  def recur(l: List[Symbol], cl: List[Symbol]): List[Symbol] = {
    if (l.isEmpty) cl
    else if (hasChar(cl, l.head)) recur(l.tail, cl)
    else recur(l.tail, cl :+ l.head)
  }
  recur(l, List())
}
compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))


// P09
type LS = List[Symbol]
type LLS = List[LS]
def pack(l: LS): LLS = {
  def recur(l: LS,lastSym: Symbol, buffer: LS, packedL: LLS): LLS = {
    if (l.isEmpty) packedL:+buffer
    else if (l.head == lastSym) recur(l.tail, l.head, buffer :+ l.head, packedL)
    else recur(l.tail, l.head, List(l.head), packedL:+buffer)
  }
  recur(l, l.head, List(), List())
}
pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))


// P10
def encode(l: List[Symbol]): List[List[Any]] = pack(l).map(x => List(x.length,x.head))
encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))


// P11
def encodeModified(l: List[Symbol]): List[Any] = encode(l).map({
  case List(1,a) => a
  case b => b
})
encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))


// P12
def decode(l: List[(Int,Symbol)]): List[Any] = {
  l.flatMap(x => List.tabulate(x._1)(_ => x._2))
}
decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))


// P14
def duplicate(l: List[Symbol]): List[Symbol] = l.flatMap(x => List(x,x))
duplicate(List('a, 'b, 'c, 'c, 'd))


// P15
def duplicateN(n: Int, l: List[Symbol]): List[Symbol] = {
  l.flatMap(x => List.fill(n)(x))
}
duplicateN(10, List('a, 'b, 'c, 'c, 'd))


// http://aperiodic.net/phil/scala/s-99/
