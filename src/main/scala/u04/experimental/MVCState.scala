package u04.experimental

import Events.{CanvasClick, ComponentClick}
import u04.monads.Monads.Monad.{seq, seqN}
import u04.monads.States.State

@main def runMVCState =
  import FractalStateImpl.*
  import WindowStateWithCanvasImpl.*
  import u03.extensionmethods.Streams.*

  val width = 300
  val height = 300

  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] = 
    State: (sm, sv) => 
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)
  def convertColorToIteration(iteration: Int, maxIterations: Int): java.awt.Color = {
    if (iteration >= maxIterations) java.awt.Color.BLACK
    else {
      val hue = 0.7f + 10 * iteration.toFloat / maxIterations
      java.awt.Color.getHSBColor(hue % 1, 0.8f, 1f)
    }
  }

  def redraw(f: (Int, Int) => Int): State[Window, Unit] = for
    _ <- drawPixels((x, y) => convertColorToIteration(f(x, y), 1000), "Mandlebrot")
  yield ()

  def windowCreation(drawer: (Int, Int) => Int): State[Window, Stream[Events]] = for
    _ <- setSize(width, height)
    _ <- addCanvas(height, width, "Mandlebrot")
    _ <- show()
    _ <- drawPixels((x, y) => convertColorToIteration(drawer(x, y), 1000), "Mandlebrot")
    _ <- addButton("Reset", "reset")
    events <- eventStream()
  yield (events)

  val controller = for
    events <- mv(get(width, height), f => windowCreation(f))
    _ <- seqN(events.map(_ match
      case CanvasClick(x, y) =>
        mv(seq(centerIn(x, y, width, height), seq(zoomIn(),get(width, height))), f => redraw(f))
      case ComponentClick("reset") => mv(seq(reset(), get(width, height)), f => redraw(f)))
    )
  yield ()

  controller.run((initialFractal(), initialWindow))