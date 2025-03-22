package u03.extensionmethods

object Streams:

  import Sequences.*
  
  enum Stream[A]:
    case Empty()
    case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    extension [A](stream: Stream[A])  
      def toList: Sequence[A] = stream match
        case Cons(h, t) => Sequence.Cons(h(), t().toList)
        case _ => Sequence.Nil()

    extension [A](stream: Stream[A])  
      def map[B](f: A => B): Stream[B] = stream match
        case Cons(head, tail) => cons(f(head()), tail().map(f))
        case _ => Empty()

    extension [A](stream: Stream[A])  
      def filter(pred: A => Boolean): Stream[A] = stream match
        case Cons(head, tail) if (pred(head())) => cons(head(), tail().filter(pred))
        case Cons(head, tail) => tail().filter(pred)
        case _ => Empty()

    extension [A](stream: Stream[A])  
      def take(n: Int): Stream[A] = (stream, n) match
        case (Cons(head, tail), n) if n > 0 => cons(head(), tail().take(n - 1))
        case _ => Empty()

    extension [A](stream: Stream[A])
      def flatMap[B](f: A => Stream[B]): Stream[B] = stream match
        case Cons(head, tail) => f(head()) concat tail().flatMap(f)
        case _ => Empty()

    extension [A](stream: Stream[A])
      def concat(other: => Stream[A]): Stream[A] = stream match
        case Cons(head, tail) => cons(head(), tail().concat(other))
        case _ => other

    /**
     * Cartesian product should be as follow:
     * Stream(1, 2, 3).cartesianProduct(Stream('a', 'b')) == Stream((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b'))
     */
    def cartesianProductWithoutFlatmap[A, B](stream1: Stream[A], stream2: Stream[B]): Stream[(A, B)] = (stream1, stream2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons((h1(), h2()), cartesianProductWithoutFlatmap(stream1, t2()) concat cartesianProductWithoutFlatmap(t1(), stream2))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def generate[A](supplier: () => A): Stream[A] =
      cons(supplier(), generate(supplier))  

    def range(start: Int, end: Int): Stream[Int] =
      iterate(start)(_ + 1).take(end - start)

  end Stream

@main def streamExample =
  import Streams.*
  import Stream.* 
  
  generate(() => "a").take(10).toList
  
  generate(() => Math.random())
      .map(x => (x*10).toInt)
      .filter(x => x < 5)
      .take(10)
      .toList

  //generate(() => scala.io.StdIn.readLine).map(s => {println(s);s}).foldleft("")(_ + _)    
