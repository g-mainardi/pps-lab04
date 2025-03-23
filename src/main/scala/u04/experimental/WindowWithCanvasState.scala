package u04.experimental

import u03.extensionmethods.Streams.*
import CanvasFunctionalFacade.EventFacade
import u04.monads.Monads.Monad.seqN
import u04.monads.States.State

enum Events:
  case ComponentClick(name: String)
  case CanvasClick(x: Int, y: Int)
trait WindowStateWithCanvas:
  type Window
  def initialWindow: Window
  def setSize(width: Int, height: Int): State[Window, Unit]
  def addButton(text: String, name: String): State[Window, Unit]
  def addLabel(text: String, name: String): State[Window, Unit]
  def toLabel(text: String, name: String): State[Window, Unit]
  def show(): State[Window, Unit]
  def exec(cmd: =>Unit): State[Window, Unit]
  def eventStream(): State[Window, Stream[Events]]
  def addCanvas(width: Int, height: Int, name: String): State[Window, Unit]
  def drawPixel(x: Int, y: Int, color: java.awt.Color, name: String): State[Window, Unit]
  def drawPixels(function: (Int, Int) => java.awt.Color, name: String): State[Window, Unit]
  def clearCanvas(name: String): State[Window, Unit]

object WindowStateWithCanvasImpl extends WindowStateWithCanvas:
  import CanvasFunctionalFacade.*

  type Window = Frame

  def initialWindow: Window = createFrame

  def setSize(width: Int, height: Int): State[Window, Unit] =
    State(w => ((w.setSize(width, height)), {}))

  def addButton(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addButton(text, name)), {}))

  def addLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addLabel(text, name)), {}))

  def toLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.showToLabel(text, name)), {}))

  def show(): State[Window, Unit] =
    State(w => (w.show, {}))

  def exec(cmd: =>Unit): State[Window, Unit] =
    State(w => (w, cmd))

  def eventStream(): State[Window, Stream[Events]] =
    State(w => (w, Stream.generate(() => mapEvent(w.events().get))))

  private def mapEvent(e: EventFacade): Events = e match
    case ev: ComponentClick => Events.ComponentClick(ev.getComponentName)
    case ev: ClickOnCanvasEvent=> Events.CanvasClick(ev.getPoint.x, ev.getPoint.y)

  def addCanvas(width: Int, height: Int, name: String): State[Window, Unit] =
    State(w => ((w.addCanvas(width, height, name)), {}))

  def drawPixel(x: Int, y: Int, color: java.awt.Color, name: String): State[Window, Unit] =
    State(w =>
      ((w.drawPixel(x, y, color, name)), {}
      ))

  def drawPixels(function: (Int, Int) => java.awt.Color, name: String): State[Window, Unit] =
    State(w => ((w.drawAllPixelsWith(p => function(p.x, p.y), name)), {}))

  def clearCanvas(name: String): State[Window, Unit] =
    State(w => ((w.clearCanvas(name)), {}))

@main def canvasExample =
  import WindowStateWithCanvasImpl.*
  val translate = 4
  val scale = 8

  def distanceToHeart(x: Double, y: Double): Double = {
    // Scale and translate coordinates to center the heart
    val nx = x * scale - translate
    val ny = (y * scale - translate) * -1  // Flip y to match screen coordinates

    // Heart shape formula based on distance
    val q = nx * nx + ny * ny - 1
    Math.pow(q, 3) - nx * nx * ny * ny * ny
  }

  def heartColor(x: Int, y: Int, width: Int, height: Int): java.awt.Color = {
    // Normalize coordinates between 0 and 1
    val nx = x.toDouble / width
    val ny = y.toDouble / height

    val distance = distanceToHeart(nx, ny)

    // If distance ≤ 0 we're inside the heart
    if (distance <= 0) {
      java.awt.Color.RED
    } else {
      java.awt.Color.WHITE
    }
  }

  val canvasWidth = 500
  val canvasHeight = 500

  def view = for
    _ <- setSize(canvasWidth + 100, canvasHeight + 100)
    _ <- addCanvas(canvasWidth, canvasHeight, "HeartCanvas")
    _ <- show()
    _ <- drawPixels(heartColor(_, _, canvasWidth, canvasHeight), "HeartCanvas")
  yield ()

  view.run(initialWindow)
