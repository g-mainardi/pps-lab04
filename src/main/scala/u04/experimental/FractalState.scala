package u04.experimental

import u04.monads.States.State

trait FractalState:
  type Fractal
  def initialFractal(): Fractal
  def zoomIn(): State[Fractal, Unit]
  def centerIn(xScreen: Int, yScreen: Int, width:Int, height: Int): State[Fractal, Unit]
  def get(canvasWidth: Int, canvasHeight: Int): State[Fractal, (Int, Int) => Int]
  def reset(): State[Fractal, Unit]

object FractalStateImpl extends FractalState:
  opaque type Fractal = (Double, Double, Double, Int)

  def initialFractal(): Fractal = (0.0, 0.0, 4.0, 100)

  def zoomIn(): State[Fractal, Unit] = State(f => (f.copy(_3 = f._3 / 2), ()))

  def get(canvasWidth: Int, canvasHeight: Int): State[Fractal, (Int, Int) => Int] = State(f => (f, drawer(_, _, canvasWidth, canvasHeight, f)))

  override def centerIn(xScreen: Int, yScreen: Int, width: Int, height: Int): State[Fractal, Unit] = State(f => {
    val (centerX, centerY, zoomLevel, maxIterations) = f
    val scaleX = zoomLevel / width
    val scaleY = zoomLevel / height
    val cReal = (xScreen - width / 2) * scaleX + centerX
    val cImaginary = (yScreen - height / 2) * scaleY + centerY
    (f.copy(_1 = cReal, _2 = cImaginary), ())
  })

  def reset(): State[Fractal, Unit] = State(f => (initialFractal(), ()))

  private def computeMandelbrot(cReal: Double, cImaginary: Double, maxIterations: Int): Int =
    var real, imaginary = 0.0
    var iteration = 0
    while (real * real + imaginary * imaginary <= 4.0 && iteration < maxIterations) do
      val newReal = real * real - imaginary * imaginary + cReal
      val newImaginary = 2.0 * real * imaginary + cImaginary
      real = newReal
      imaginary = newImaginary
      iteration += 1
    iteration

  private def drawer(x: Int, y: Int, canvasWidth: Int, canvasHeight: Int, fractalState: Fractal): Int =
    val (centerX, centerY, zoomLevel, maxIterations) = fractalState
    val scaleX = zoomLevel / canvasWidth
    val scaleY = zoomLevel / canvasHeight
    val cReal = (x - canvasWidth / 2) * scaleX + centerX
    val cImaginary = (y - canvasHeight / 2) * scaleY + centerY
    computeMandelbrot(cReal, cImaginary, maxIterations)




