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

  def computeMandelbrot(cReal: Double, cImaginary: Double, maxIterations: Int): Int =
    var real, imaginary = 0.0
    var iteration = 0
    while (real * real + imaginary * imaginary <= 4.0 && iteration < maxIterations) do
      val newReal = real * real - imaginary * imaginary + cReal
      val newImaginary = 2.0 * real * imaginary + cImaginary
      real = newReal
      imaginary = newImaginary
      iteration += 1
    iteration

  def colorFromIterations(iterations: Int, maxIterations: Int): java.awt.Color =
    if iterations == maxIterations then java.awt.Color.BLACK
    else new java.awt.Color(iterations * 8 % 256, iterations * 4 % 256, iterations * 2 % 256)

  val canvasWidth = 500 // Width of the canvas in pixels
  val canvasHeight = 500 // Height of the canvas in pixels
  val maxIterations = 100 // Maximum iterations for Mandelbrot computation
  val centerX = -0.7 // Center X-coordinate for Mandelbrot zoom
  val centerY = 0.0 // Center Y-coordinate for Mandelbrot zoom
  var zoomLevel = 3.0 // Zoom level defining the visible range of the fractal

  def drawer(x: Int, y: Int, centerX: Double, centerY: Double, zoomLevel: Double): java.awt.Color =
    val scaleX = zoomLevel / canvasWidth
    val scaleY = zoomLevel / canvasHeight
    val cReal = (x - canvasWidth / 2) * scaleX + centerX
    val cImaginary = (y - canvasHeight / 2) * scaleY + centerY
    val iterations = computeMandelbrot(cReal, cImaginary, maxIterations)
    colorFromIterations(iterations, maxIterations)

  def stateMandelbrot(): State[(Double, Double, Double), Unit] = State(s => ((centerX, centerY, zoomLevel), {}))

  def view(render: (Int, Int) => java.awt.Color) = for
    _ <- setSize(canvasWidth + 100, canvasHeight + 100) // Adjust window size
    _ <- addCanvas(canvasWidth, canvasHeight, "MandelbrotCanvas")
    _ <- show()
    _ <- drawPixels(drawer(_, _, centerX, centerY, zoomLevel), "MandelbrotCanvas")
    e <- eventStream()
    _ <- seqN(e.map(e => exec {
      zoomLevel = zoomLevel * 2
      println(zoomLevel)
    }))
  yield ()

  view(drawer(_, _, centerX, centerY, zoomLevel)).run(initialWindow)
