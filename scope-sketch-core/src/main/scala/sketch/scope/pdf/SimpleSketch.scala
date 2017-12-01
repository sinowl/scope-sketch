package sketch.scope.pdf

/**
  * Licensed by Probe Technology, Inc.
  */
case class SimpleSketch[A](measure: A => Prim, structures: Structures) extends Sketch[A]

trait SimpleSketchOps extends SketchPrimPropOps[SimpleSketch] {

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch(sketch.measure, sketch.structures)

}

object SimpleSketch extends SimpleSketchOps {

  def modifyStructure[A](sketch: SimpleSketch[A], f: Structures => Option[Structures]): Option[SimpleSketch[A]] =
    f(sketch.structures).map(structure => SimpleSketch(sketch.measure, structure))

}