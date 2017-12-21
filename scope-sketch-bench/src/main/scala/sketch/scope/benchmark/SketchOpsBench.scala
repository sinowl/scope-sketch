package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import SketchBenchOps._
import sketch.scope.pdf.{Count, Sketch}
import sketch.scope.measure._

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SketchOpsBench {

  @Param(Array("5"))
  var caDepth: Int = _

  @Param(Array("5000"))
  var caSize: Int = _

  @Param(Array("1", "2", "5"))
  var coDepth: Int = _

//  @Param(Array("1000", "100000"))
  @Param(Array("1000", "2000", "3000"))
  var coSize: Int = _

  val sketch = Sketch.empty(doubleMeasure, caDepth, caSize, coDepth, coSize)
  val count:Count = 1
  val samplesD = normalD.samples(1000)._2
  Sketch.deepUpdate(sketch, samplesD.map(x => (x,count)))

  @Benchmark
  def construct = {
    Sketch.empty(doubleMeasure, caDepth, caSize, coDepth, coSize)
  }

  @Benchmark
  def update = {
    updateBench(sketch, defaultSignals, 1)
  }

  @Benchmark
  def rearrange = {
    rearrangeBench(sketch)
  }

  @Benchmark
  def updatenormalDist = {
    updateNormalD(sketch, 1000)
  }

  @Benchmark
  def narrowupdatenormalDist = {
    narrowupdateNormalD(sketch, 1000)
  }

  @Benchmark
  def deepupdatenormalDist = {
    deepupdateNormalD(sketch, 1000)
  }

  @Benchmark
  def sketchmap = {
    sketchMap(sketch)
  }

  @Benchmark
  def sketchbind = {
    sketchFlatMpa(sketch)
  }

}
