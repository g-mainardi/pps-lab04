package u04.experimental

import u04.monads.States.State

trait FractalState:
  type Fractal
  type Drawer = ((Int, Int) => Int, Int)
  def initialFractal(maxIterations: Int): Fractal
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

    def iterations: Int = state match
      case InternalFractalState(_, _, _, maxIterations) => maxIterations
  opaque type Fractal = InternalFractalState

  def initialFractal(maxIterations: Int): Fractal = InternalFractalState(0.0, 0.0, 4.0, maxIterations)

  def zoomIn(): State[Fractal, Unit] = State(f => (f.zoomIn(), ()))

  def get(canvasWidth: Int, canvasHeight: Int): State[Fractal, ((Int, Int) => Int, Int)] =
    State(f =>
      (f, (fractalInArea(_, _, canvasWidth, canvasHeight, f), f.iterations)))

  override def centerIn(xScreen: Int, yScreen: Int, width: Int, height: Int): State[Fractal, Unit] = State(f => {
    val InternalFractalState(centerX, centerY, zoomLevel, maxIterations) = f
    val (cReal, cImaginary) = pixelToComplex(xScreen, yScreen, width, height, f)
    (f.updateCenter(cReal, cImaginary), ())
  })

  def reset(): State[Fractal, Unit] = State(f => (initialFractal(f.iterations), ()))

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

  private def fractalInArea(x: Int, y: Int, canvasWidth: Int, canvasHeight: Int, fractalState: Fractal): Int =
    val InternalFractalState(centerX, centerY, zoomLevel, maxIterations) = fractalState
    val (cReal, cImaginary) = pixelToComplex(x, y, canvasWidth, canvasHeight, fractalState)
    Fractals.computeMandelbrot(cReal, cImaginary, maxIterations)

  private def pixelToComplex(x: Int, y: Int, w: Int, h: Int, fractalState: Fractal): (Double, Double) =
    val InternalFractalState(centerX, centerY, zoomLevel, _) = fractalState
    val scaleX = zoomLevel / w
    val scaleY = zoomLevel / h
    ((x - w / 2) * scaleX + centerX, (y - h / 2) * scaleY + centerY)

object Fractals:
  def computeMandelbrot(cReal: Double, cImaginary: Double, maxIterations: Int): Int = {
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

  def computeJulia(x: Double, y: Double, maxIterations: Int): Int = {
    // Standard Julia set uses a fixed complex parameter c
    val cReal = -0.7
    val cImaginary = 0.27

    @annotation.tailrec
    def iterate(real: Double, imaginary: Double, iteration: Int): Int =
      if (real * real + imaginary * imaginary > 4.0 || iteration >= maxIterations) iteration
      else {
        val newReal = real * real - imaginary * imaginary + cReal
        val newImaginary = 2.0 * real * imaginary + cImaginary
        iterate(newReal, newImaginary, iteration + 1)
      }
    iterate(x, y, 0) // Starting with the coordinates in the complex plane
  }

  def computeBurningShip(x: Double, y: Double, maxIterations: Int): Int = {
    @annotation.tailrec
    def iterate(real: Double, imaginary: Double, iteration: Int): Int =
      if (real * real + imaginary * imaginary > 4.0 || iteration >= maxIterations) iteration
      else {
        val newReal = real * real - imaginary * imaginary + x
        val newImaginary = 2.0 * Math.abs(real * imaginary) + y
        iterate(newReal, newImaginary, iteration + 1)
      }
    iterate(0.0, 0.0, 0)
  }