package u04.experimental

import Events.{CanvasClick, ComponentClick}
import u04.monads.Monads.Monad.{seq, seqN}
import u04.monads.States.State

@main def runMVCState =
  import FractalStateImpl.*
  import WindowStateWithCanvasImpl.*
  import u03.extensionmethods.Streams.*

  val width = 500
  val height = 500
  val span = 75
  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] = 
    State: (sm, sv) => 
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  val baseHue = 0.7f // Starting point in the color spectrum (blue-violet)
  val hueRange = 10f // How much the color cycles through the spectrum
  val saturation = 0.8f // Color intensity
  val brightness = 1.0f // Color brightness (max)

  def convertColorToIteration(iteration: Int, maxIterations: Int): java.awt.Color = {
    if (iteration >= maxIterations) java.awt.Color.BLACK
    else {
      val hue = baseHue + (iteration.toFloat / maxIterations) * hueRange
      java.awt.Color.getHSBColor(hue % 1, saturation, brightness)
    }
  }

  def redraw(data: Drawer): State[Window, Unit] = data match
    case (f, iterations) =>
      for
        _ <- drawPixels((x, y) => convertColorToIteration(f(x, y), iterations), "Mandlebrot")
      yield ()

  def windowCreation(data: Drawer): State[Window, Stream[Events]] = data match
    case (drawer, iterations) =>
      for
      _ <- setSize(width + span, height + span)
      _ <- addCanvas(height, width, "Mandlebrot")
      _ <- addButton("Reset", "reset")
      _ <- show()
      _ <- drawPixels((x, y) => convertColorToIteration(drawer(x, y), iterations), "Mandlebrot")
      events <- eventStream()
    yield (events)

  val controller = for
    events <- mv(get(width, height), f => windowCreation(f._1, f._2))
    _ <- seqN(events.map {
      case CanvasClick(x, y) =>
        mv(seq(centerIn(x, y, width, height), seq(zoomIn(), get(width, height))), f => redraw(f))
      case ComponentClick("reset") => mv(seq(reset(), get(width, height)), f => redraw(f))
    })
  yield ()

  controller.run((initialFractal(), initialWindow))