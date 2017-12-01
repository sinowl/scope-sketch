package sketch.scope.pdf

import sketch.scope.range._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Dirac Delta Function.
  */
case class DeltaDist[A](measure: A => Prim, center: Prim) extends SmoothDist[A]

trait DeltaDistOps extends SmoothDistPropOps[DeltaDist] {

  def probability[A](dist: DeltaDist[A], from: A, to: A): Option[Double] = {
    if(Range(dist.measure(from), dist.measure(to)).contains(dist.center)) Some(1) else Some(0)
  }

}

object DeltaDist extends DeltaDistOps