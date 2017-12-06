package sketch.scope.pdf

import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot

/**
  * Licensed by Probe Technology, Inc.
  */
trait PlottedDist[A] extends SampledDist[A] {

  def densityPlot: DensityPlot

}

trait PlottedDistPropOps[D[_]<:PlottedDist[_]] extends SampleDistPropOps[D] {

  def densityPlot(dist: D[_]): Option[DensityPlot] = Some(dist.densityPlot)

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  case class PlottedDistImpl[A](measure: Measure[A], densityPlot: DensityPlot) extends PlottedDist[A]

  def apply[A](measure: Measure[A], densityPlot: DensityPlot): PlottedDist[A] = PlottedDistImpl(measure, densityPlot)

  def probability[A](dist: PlottedDist[A], start: A, end: A): Option[Double] = ???

  def sample[A](dist: PlottedDist[A]): (PlottedDist[A], A) = ???

}