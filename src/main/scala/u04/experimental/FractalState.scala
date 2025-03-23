package u04.experimental

import u04.monads.States.State

trait FractalState:
  type Fractal
  type Drawer = ((Int, Int) => Int, Int)
  def initialFractal(): Fractal
  def zoomIn(): State[Fractal, Unit]
  def centerIn(xScreen: Int, yScreen: Int, width:Int, height: Int): State[Fractal, Unit]
  def get(canvasWidth: Int, canvasHeight: Int): State[Fractal, Drawer]
  def reset(): State[Fractal, Unit]

object FractalStateImpl extends FractalState:
  case class InternalFractalState(centerX: Double, centerY: Double, zoomLevel: Double, maxIterations: Int)

  extension (state: InternalFractalState)
    def zoomIn(): InternalFractalState = state match
      case InternalFractalState(centerX, centerY, zoomLevel, maxIterations) => InternalFractalState(centerX, centerY, zoomLevel / 2, maxIterations)

    def updateCenter(x: Double, y: Double): InternalFractalState = state match
      case InternalFractalState(centerX, centerY, zoomLevel, maxIterations) => InternalFractalState(x, y, zoomLevel, maxIterations)

  opaque type Fractal = InternalFractalState

  def initialFractal(): Fractal = InternalFractalState(0.0, 0.0, 4.0, 1000)

  def zoomIn(): State[Fractal, Unit] = State(f => (f.zoomIn(), ()))

  def get(canvasWidth: Int, canvasHeight: Int): State[Fractal, ((Int, Int) => Int, Int)] = State(f => (f, (drawer(_, _, canvasWidth, canvasHeight, f), f._4)))

  override def centerIn(xScreen: Int, yScreen: Int, width: Int, height: Int): State[Fractal, Unit] = State(f => {
    val InternalFractalState(centerX, centerY, zoomLevel, maxIterations) = f
    val (cReal, cImaginary) = pixelToComplex(xScreen, yScreen, width, height, f)
    (f.updateCenter(cReal, cImaginary), ())
  })

  def reset(): State[Fractal, Unit] = State(f => (initialFractal(), ()))

   private def computeMandelbrot(cReal: Double, cImaginary: Double, maxIterations: Int): Int = {
     @annotation.tailrec
     def iterate(real: Double, imaginary: Double, iteration: Int): Int =
       if (real * real + imaginary * imaginary > 4.0 || iteration >= maxIterations) iteration
       else {
         val newReal = real * real - imaginary * imaginary + cReal
         val newImaginary = 2.0 * real * imaginary + cImaginary
         iterate(newReal, newImaginary, iteration + 1)
       }
     iterate(0.0, 0.0, 0)
   }

  private def drawer(x: Int, y: Int, canvasWidth: Int, canvasHeight: Int, fractalState: Fractal): Int =
    val InternalFractalState(centerX, centerY, zoomLevel, maxIterations) = fractalState
    val (cReal, cImaginary) = pixelToComplex(x, y, canvasWidth, canvasHeight, fractalState)
    computeMandelbrot(cReal, cImaginary, maxIterations)


  private def pixelToComplex(x: Int, y: Int, w: Int, h: Int, fractalState: Fractal): (Double, Double) =
    val InternalFractalState(centerX, centerY, zoomLevel, _) = fractalState
    val scaleX = zoomLevel / w
    val scaleY = zoomLevel / h
    ((x - w / 2) * scaleX + centerX, (y - h / 2) * scaleY + centerY)