package sketch.scope.benchmark

import sketch.scope.pdf.{NormalDist, Sketch, Structure, Count}
import sketch.scope.measure._
import sketch.scope.rand._

/**
  * Licensed by Probe Technology, Inc.
  */
object SketchBenchOps {

  val defaultSketch: Sketch[Double] = Sketch.empty(doubleMeasure, 5, 1000, 1, 2000)
  // CDim : CaSize = 1000

  val defaultSignals: Stream[Double] = Stream.iterate(0.0)(_ + 0.1)

  val normalD = NormalDist(doubleMeasure, 10.0, 1.0, IRng(100))

  val normalD1 = NormalDist(doubleMeasure, 11.0, 1.0, IRng(100))
  val normalD3 = NormalDist(doubleMeasure, 12.0, 1.0, IRng(100))
  val sampleD1 = normalD1.samples(100)._2
  val sampleD3 = normalD3.samples(100)._2

  def updateBench(sketch: Sketch[Double], signals: Stream[Double], n: Int): Option[Sketch[Double]] = {
      signals
      .take(n)
      .foldLeft(Option(sketch)){ case (sketchO, signal) =>
        sketchO.flatMap(sketch => sketch.update(signal))
      }
  }

  def rearrangeBench[A](sketch: Sketch[A]): Option[Sketch[A]] = sketch.rearrange

  def updateNormalD(sketch: Sketch[Double], n: Int): Option[Sketch[Double]] = {
    val samplesD = normalD.samples(n)._2
    samplesD.foldLeft(Option(sketch)){ case (sketchO, sample) =>
      sketchO.flatMap(sketch => sketch.update(sample))
    }
  }

  def narrowupdateNormalD(sketch: Sketch[Double], n: Int): Option[Sketch[Double]] = {
    val count:Count = 1
    val samplesD = normalD.samples(n)._2
    Sketch.narrowUpdate(sketch, samplesD.map(x => (x,count)))
  }

  def deepupdateNormalD(sketch: Sketch[Double], n: Int): Option[(Sketch[Double],Structure)] = {
    val count:Count = 1
    val samplesD = normalD.samples(n)._2
    Sketch.deepUpdate(sketch, samplesD.map(x => (x,count)))
  }

  def sketchMap(sketch: Sketch[Double]): Sketch[Double] = {
    sketch.map(i => -1 * i)
  }

  def sketchFlatMpa(sketch: Sketch[Double]): Sketch[Double] = {
    sketch.flatMap(i => NormalDist[Double](doubleMeasure, i, 1))
  }

  def KL(sketch: Sketch[Double]): Double = {
    sampleD1.flatMap( a => sampleD3.map( b => a*Math.log(a/b) )).foldLeft(0.0)((m:Double, n:Double)=>m+n)
    // same ~= 0
  }

}
