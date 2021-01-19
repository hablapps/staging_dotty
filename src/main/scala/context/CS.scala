package context

import scala.language.implicitConversions

sealed abstract class CS[T]
case class CSInt(i: Either[Int, Double]) extends CS[Int]
case class CSBool(b: Boolean) extends CS[Boolean]

object CS{

  type CS_Repr[T] = Boolean => CS[T]

  implicit object CSArith extends Arith[CS_Repr]{
      def lit(i: Int): CS_Repr[Int] =
        if (_) CSInt(Right(i)) else CSInt(Left(i))
      def add(i: CS_Repr[Int], j: CS_Repr[Int]): CS_Repr[Int] =
        ???
      def times(i: CS_Repr[Int], j: CS_Repr[Int]): CS_Repr[Int] =
        ???
      def iF[T](cond: CS_Repr[Boolean], _then: CS_Repr[T], _else: CS_Repr[T]): CS_Repr[T] =
        ???
  }
}
