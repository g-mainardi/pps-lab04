package u04.monads
import Monads.*, Monad.*

object Optionals:

  // data structure for optionals
  enum Optional[A]:
    case Just(a: A)
    case Empty()

  // minimal set of algorithms
  object Optional:
    extension [A](m: Optional[A])
      def filter(p: A => Boolean): Optional[A] = m match
        case Just(a) if (p(a)) => m
        case _                 => Empty()
      def getOrElse(default: A): A = m match
        case Just(a) => a
        case _ => default

  // extending Optional to be a Monad!
  given Monad[Optional] with
    import Optional.{Just, Empty}  
    
    // unit: just boxing the value
    def unit[A](a: A): Optional[A] = Just(a)

    // flatMap: opens the box if possible, gives the new box
    extension [A](m: Optional[A])
      def flatMap[B](f: A => Optional[B]): Optional[B] =
        m match
          case Just(a) => f(a)
          case Empty() => Empty()

@main def tryOptionals =
  import Optionals.{*, given} // importing also given terms
  import Optional.*           // importing Optional algorithms

  def optionalRandom(): Optional[Double] =
    Just(java.lang.Math.random()).filter(_ < 0.9)

  // for comprehension: <- just unboxes the Optional if possible
  val m: Optional[Double] = for
    x <- optionalRandom()
    y <- optionalRandom()
    z <- optionalRandom()
  yield x + y + z
  println(m)

  // equivalent formulation by flatMap/map
  val m2: Optional[Double] =
    optionalRandom().flatMap(x =>
      optionalRandom().flatMap(y =>
        optionalRandom().map(z =>
          x + y + z)))
  println(m2)

  // use of some monadic operator
  println:
    map2(Just("rand: "), optionalRandom())(_ + _) // Some("rand: 0.223.."), or Empty() 
