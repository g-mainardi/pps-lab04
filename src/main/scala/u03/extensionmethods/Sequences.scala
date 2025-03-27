package u03.extensionmethods

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:
    def cons[E](head: E, tail: Sequence[E]): Sequence[E] = Cons(head, tail)
    def nil[E](): Sequence[E] = Nil()

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
        case Nil()      => Nil()

      def concat(other: Sequence[A]): Sequence[A] = l match
        case Cons(h, t) => Cons(h, t.concat(other))
        case Nil()      => other

      def contains(elem: A): Boolean = l match
        case Cons(h, t) => elem == h | t.contains(elem)
        case _ => false

      def distinct(): Sequence[A] =
        def _distinct(s: Sequence[A])(acc: Sequence[A]): Sequence[A] = s match
          case Cons(h, t) if !acc.contains(h) => Cons(h, _distinct(t)(Cons(h, acc)))
          case Cons(_, t) => _distinct(t)(acc)
          case _ => Nil()
        _distinct(l)(Nil())

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))

@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  
