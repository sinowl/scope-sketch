package sketch.scope.pdf.syntax

import sketch.scope.pdf.monad.{DistBind, DistFunctor, DistMonad}
import sketch.scope.pdf.{Dist, SampleDist, Sketch}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistSyntax extends DistPropSyntax with DistMonadSyntax

trait DistPropSyntax {

  implicit class DistPropSyntaxImpl[A](dist: Dist[A]) {
    def probability(from: A, to: A): Option[Double] = Dist.probability(dist, from, to)
  }

}

trait DistBindAux[InD[_]<:Dist[_], OutD[_]<:Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistMonadSyntax extends DistMonadSyntax1 {

  implicit class DistMonadSyntaxImpl0[A](dist: Dist[A]) {
    def map[B](f: A => B)(implicit functor: DistFunctor[Dist]): Dist[B] = functor.map(dist, f)
    def flatMap[B, D1[_]<:Dist[_], D2[_]<:Dist[_]](f: A => D1[B])
                                                  (implicit
                                                   aux: DistBindAux[D1, D2],
                                                   monad: DistMonad[Dist, D1, D2]): aux.Out[B] = monad.bind(dist, f)
  }

}

trait DistMonadSyntax1 extends DistMonadSyntax2 {

  implicit def bindAux1: DistBindAux[Sketch, Sketch] = new DistBindAux[Sketch, Sketch] {}
  implicit def distMonad1: DistMonad[Dist, Sketch, Sketch] = DistMonad.sketch

}

trait DistMonadSyntax2 extends DistMonadSyntax3 {

  implicit def bindAux2: DistBindAux[SampleDist, SampleDist] = new DistBindAux[SampleDist, SampleDist] {}
  implicit def distMonad2: DistMonad[Dist, SampleDist, SampleDist] = DistMonad.sampleDist

}

trait DistMonadSyntax3 {

  implicit def bindAux3: DistBindAux[Dist, Dist] = new DistBindAux[Dist, Dist] {}
  implicit def distMonad3: DistMonad[Dist, Dist, Dist] = DistMonad.dist

}