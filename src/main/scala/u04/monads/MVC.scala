package u04.monads

import Optionals.Optional

@main def runMVC =
  import Monads.*, Monad.*, States.*, State.*, CounterStateImpl.*, WindowStateImpl.*
  import u03.extensionmethods.Streams.*
  
  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] = 
    State: (sm, sv) => 
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for 
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addTextField(text = "write here...", name = "SetterField", 6)
    _ <- addButton(text = "set", name = "SetButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- show()
    events <- eventStream()
  yield events
  
  def parse(text: String): State[Window, Optional[Int]] =
    try
      val number = text.toInt
      State(w => (w, Optional.Just(number)))
    catch
      case _: NumberFormatException => State(w => (w, Optional.Empty()))

  def getParseAndSet(field: String, label: String): State[(Counter, Window), Unit] = for
    n <- mv(nop(), _ => for
      t <- getText(field)
      n <- parse(t)
    yield n)
    f <- mv(seq(set(n.getOrElse(0)), get()), i => toLabel(i.toString, label))
  yield f

  val controller = for
    events <- mv(seq(reset(), get()), i => windowCreation(i.toString))
    _ <- seqN(events.map(_ match
        case "IncButton" => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
        case "DecButton" => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
        case "ResetButton" => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
        case "SetButton" => getParseAndSet("SetterField", "Label1")
        case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
  yield ()

  controller.run((initialCounter(), initialWindow))