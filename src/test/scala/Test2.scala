import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.staging.{run, withQuoteContext, Toolbox}

import org.junit.Test
import org.junit.Assert._

class Test2 {

  // Needed to run or show quotes
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  private def code(given QuoteContext) = '{ identity("foo") }

  @Test def basics(): Unit = {
    withQuoteContext{
      val e: Expr[Int] = '{1+2}
      val e2: Expr[Int] = '{$e + $e}
      println(e.show)
      println(e2.show)
      // warning: assertEquals('{$e}.show, e.show)
      // error: ${e}
    }
  }

  @Test def avoidCapture(): Unit = {
    def rec(n: Int, z: Expr[Int])(given QuoteContext): Expr[Int] =
      if (n == 0) z
      else '{((x: Int) => ${rec(n-1, '{x + $z})})(${Expr(n)})}

    def recAux(n: Int, z: Int): Int =
      run(rec(n, Expr(z)))

    withQuoteContext{
      println(rec(3, Expr(0)).show)
    }
    assertEquals(recAux(4,0), 10)
  }

  @Test def powerTest(): Unit = {

    def unStagePower(x: Int, n: Int): Int =
      if (n == 0) 1
      else x * unStagePower(x, n-1)

    def stagePower(x: Expr[Int], n: Int)(given QuoteContext): Expr[Int] =
      if (n == 0) Expr(1)
      else '{$x * ${stagePower(x, n-1)}}

    withQuoteContext{println(stagePower(Expr(3), 4).show)}

    def power(x: Int, n: Int): Int =
      run(stagePower(Expr(x),n))

    assertEquals(power(3, 3), 27)

  }

  @Test def ADTInterpreter(): Unit = {
    enum Arith[A]{
      case IntE(i: Int) extends Arith[Int]
      case Add(i: Arith[Int], j: Arith[Int]) extends Arith[Int]
      case Bool(b: Boolean) extends Arith[Boolean]
      case If[A](b: Arith[Boolean], _then: Arith[A], _else: Arith[A]) extends Arith[A]
    }

    import Arith._

    val e: Arith[Int] = If(Bool(false), IntE(1), Add(IntE(4), IntE(0)))

    def eval[A](e: Arith[A]): A =
      e match {
        case IntE(i) => i
        case Add(i, j) => eval(i) + eval(j)
        case Bool(b) => b
        case If(b, t, e) => if (eval(b)) eval(t) else eval(e)
      }

    assertEquals(eval(e), 4)

    def compile[A: Type](e: Arith[A])(given QuoteContext): Expr[A] =
      e match {
        case IntE(i) => Expr(i)
        case Bool(b) => Expr(b)
        case Add(i, j) => '{ ${compile(i)} + ${compile(j)} }
        case If(b, t, e) => '{ if ${compile(b)} then ${compile(t)} else ${compile(e)}}
      }

    withQuoteContext{
      assertEquals(compile(e).show, '{if false then 1 else 4+0}.show)
    }

    assertEquals(run(compile(e)), 4)
  }

  @Test def TaglessFinalInterpreter(): Unit = {

    trait Arith[P[_]] with
      def IntE(i: Int): P[Int]
      def Add(i: P[Int], j: P[Int]): P[Int]
      def Bool(b: Boolean): P[Boolean]
      def If[A](b: P[Boolean], _then: P[A], _else: P[A]): P[A]

    def e[P[_]](given A: Arith[P]): P[Int] =
      A.If(A.Bool(false), A.IntE(1), A.Add(A.IntE(4), A.IntE(0)))

    type Id[A] = A

    given Eval: Arith[Id] with
      def IntE(i: Int) = i
      def Add(i: Int, j: Int) = i + j
      def Bool(b: Boolean) = b
      def If[A](b: Boolean, t: A, e: A) = if b then t else e

    assertEquals(e(given Eval), 4)
    assertEquals(e[Id], 4)

    given Compile(given QuoteContext): Arith[[T] =>> (given Type[T]) => Expr[T]]{
      def IntE(i: Int): (given Type[Int]) => Expr[Int] =
        Expr(i)
      def Bool(b: Boolean): (given Type[Boolean]) => Expr[Boolean] =
        Expr(b)
      def Add(i: (given Type[Int]) => Expr[Int], j: (given Type[Int]) => Expr[Int]): (given Type[Int]) => Expr[Int] =
        '{ $i + $j }
      def If[A](b: (given Type[Boolean]) => Expr[Boolean], t: (given Type[A]) => Expr[A],
          e: (given Type[A]) => Expr[A]): (given Type[A]) => Expr[A] =
        '{ if $b then $t else $e }
    }

    type TypedExpr[T] = (given Type[T]) => Expr[T]

    def Compile1(given QuoteContext) = new Arith[TypedExpr]{
      def IntE(i: Int): TypedExpr[Int] =
        Expr(i)
      def Bool(b: Boolean): TypedExpr[Boolean] =
        Expr(b)
      def Add(i: TypedExpr[Int], j: TypedExpr[Int]): TypedExpr[Int] =
        '{ $i + $j }
      def If[A](b: TypedExpr[Boolean], t: TypedExpr[A], e: TypedExpr[A]): TypedExpr[A] =
        '{ if $b then $t else $e }
    }

    withQuoteContext{
      assertEquals(e[[T] =>> (given Type[T]) => Expr[T]].show, '{if false then 1 else 4+0}.show)
      assertEquals(e(given Compile).show, '{if false then 1 else 4+0}.show)
    }

    // assertEquals(run(e(given Compile)), 4)
    assertEquals(run(e[[T] =>> (given Type[T]) => Expr[T]]), 4)
  }


}
