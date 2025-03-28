package u04.monads

import States.*

trait CounterState:
  type Counter
  def initialCounter(): Counter
  def inc(): State[Counter, Unit]
  def dec(): State[Counter, Unit]
  def reset(): State[Counter, Unit]
  def set(newValue: Int): State[Counter, Unit]
  def get(): State[Counter, Int]
  def nop(): State[Counter, Unit]

object CounterStateImpl extends CounterState:
  opaque type Counter = Int
  
  def initialCounter(): Counter = 0

  // giving (new_counter, result)
  def inc(): State[Counter, Unit] = State(i => (i + 1, ()));
  def dec(): State[Counter, Unit] = State(i => (i - 1, ()));
  def reset(): State[Counter, Unit] = State(i => (0, ()));
  def set(newValue: Int): State[Counter, Unit] = State(i => (newValue, ()));
  def get(): State[Counter, Int] = State(i => (i, i));
  def nop(): State[Counter, Unit] = State(i => (i, ()));

@main def tryCounterState =
  import Monads.*, Monad.*, States.{*, given}, State.*
  val counterState: CounterState = CounterStateImpl
  import counterState.*  // or directly, import CounterStateImpl.*

  println:
    inc().run(initialCounter()) // (1,())
  println:
    seq(inc(), inc()).run(initialCounter()) // (2,())
  println:
    seq(reset(), seq(inc(), get())).run(initialCounter()) // (1,1)

  def increment(n: Int): State[Counter, Unit] =
    if (n == 0)
    then nop()
    else
      for
        _ <- increment(n - 1)
        _ <- inc()
      yield ()  

  val session: State[Counter, Int] =
    for 
      _ <- inc()
      _ <- reset()
      _ <- increment(5)
      v <- get()
      _ <- reset()
    yield v

  println:
    session.run(initialCounter())  // (5, 0)  

  val session2: State[Counter, Int] =
    for
      _ <- set(8)
      v <- get()
      _ <- reset()
    yield v

  println:
    session2.run(initialCounter()) // (8, 0)