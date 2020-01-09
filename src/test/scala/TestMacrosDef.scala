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

  inline def curryHoward(f: [A, B] => (A => B) => A => B): Int =
    ${curryHowardImpl('f)}

  def curryHowardImpl(
    f: Expr[[A, B] => (A => B) => A => B])(given QuoteContext): Expr[Int] = {
    println(s"code: ${f.show}")
    f match {
      case '{[$tA, $tB] => (f: $tC) => ($body: $tF)} =>
      // if tA == tC && tA == tE && tB == tD =>
        Expr(0)
    }
  }

  def curryHowardImpl1[A: Type, B: Type](
    f: Expr[A => B])(given QuoteContext): Expr[Int] = {
    println(s"code: ${f.show}")
    f match {
      // case '{(f: A) => $body: B} =>
      // case '{(f: $tA => $tB) => (a: $tC) => ($body: $tF)} =>
      // if tA == tC && tA == tE && tB == tD =>
      case '{($f1: $tA => $tB)} =>
        Expr(0)
      case _ =>
        Expr(-1)
    }
  }

}
