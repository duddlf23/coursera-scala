package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      val bVal = b()
      bVal * bVal- 4 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val deltaVal = delta()
      if deltaVal == 0 then Set(- b() / (2 * a()) )
      else if deltaVal < 0 then Set()
      else
        Set(
          (- b() + math.sqrt(deltaVal)) / (2 * a()),
          (- b() - math.sqrt(deltaVal)) / (2 * a())
        )
    }
