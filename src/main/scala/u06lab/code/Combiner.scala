package u06lab.code

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A
  
object Combiner:
  object SumCombiner extends Combiner[Double]:
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b

  object ConcatCombiner extends Combiner[String]:
    override def unit: String = ""
    override def combine(a: String, b: String): String = a ++ b

  object MaxCombiner extends Combiner[Int]:
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = if a > b then a else b

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combine[A](a: Iterable[A])(using combine: Combiner[A]): A

object FunctionsImpl extends Functions:
  import Combiner.*
  override def sum(a: List[Double]): Double = combine(a)(using SumCombiner)
  override def concat(a: Seq[String]): String = combine(a)(using ConcatCombiner)
  override def max(a: List[Int]): Int = combine(a)(using MaxCombiner)
  override def combine[A](a: Iterable[A])(using combiner: Combiner[A]): A = a.foldLeft(combiner.unit)(combiner.combine)
