package context

trait Arith[Repr[_]]{
  def lit(i: Int): Repr[Int]
  def add(i: Repr[Int], j: Repr[Int]): Repr[Int]
  def times(i: Repr[Int], j: Repr[Int]): Repr[Int]
  def iF[T](cond: Repr[Boolean], _then: Repr[T], _else: Repr[T]): Repr[T]
}
