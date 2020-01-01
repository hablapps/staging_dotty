import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.staging.{run, withQuoteContext, Toolbox}

import org.junit.Test
import org.junit.Assert._

class TestCH {

  // Needed to run or show quotes
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  sealed abstract class Derivation[A]

  case class Var[A](x: Int) extends Derivation[A]

  case class ImplI[A, B](
      f: Derivation[A] => Derivation[B]
  ) extends Derivation[A => B]

  case class ImplE[A, B](
      d1: Derivation[A => B],
      d2: Derivation[A]
  ) extends Derivation[B]

  case class AndI[A, B, E](
      d1: Derivation[A],
      d2: Derivation[B]
  ) extends Derivation[(A, B)]

  case class AndE1[A, B, E](
      d1: Derivation[(A, B)]
  ) extends Derivation[A]

  case class AndE2[A, B, E](
      d1: Derivation[(A, B)]
  ) extends Derivation[B]

  object Derivation {

    def show[A](d: Derivation[A], i: Int = 0): String = d match {
      case Var(x)       => s"x$x"
      case ImplI(f)     => s"ImplI(x$i, " + show(f(Var(i)), i + 1) + ")"
      case ImplE(f, a)  => s"ImplE(${show(f, i)}, ${show(a, i)})"
      case AndI(d1, d2) => s"AndI(${show(d1, i)}, ${show(d2, i)})"
      case AndE1(d)     => s"AndE1(${show(d, i)})"
      case AndE2(d)     => s"AndE2(${show(d, i)})"
    }
  }


}
