import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.staging.{run, withQuoteContext, Toolbox}

import org.junit.Test
import org.junit.Assert._

object TestMacrosDef {

  // Needed to run or show quotes
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def map[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])(given t: Type[T], q: QuoteContext): Expr[Unit] = '{
    var i: Int = 0
    while (i < ($arr).length) {
      val element: $t = ($arr)(i)
      ${f('element)}
      i += 1
    }
  }

  def sum(arr: Expr[Array[Int]])(given q: QuoteContext): Expr[Int] = '{
    var sum = 0
    ${ map(arr, x => '{sum += $x}) }
    sum
  }

  inline def sum_m(arr: Array[Int]): Int = ${sum('arr)}

  inline def ands(b: => Boolean): Int =
    ${andsImpl('b)}

  def andsImpl(b: Expr[Boolean])(given QuoteContext): Expr[Int] = {
    println(b.show)
    b match {
      case '{($i: Boolean) && ($j: Boolean)} =>
        '{1 + ${andsImpl(i)} + ${andsImpl(j)}}
      // case '{false} => Expr(-1)
      // case '{true} => Expr(-2)
      case _ => Expr(0)
    }}

}
