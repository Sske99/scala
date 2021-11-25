
import scala.{+:, :+}
import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
 *
 * Seq(1, 2) match {
 *   case head +: tail => ???
 *   case Nil          => ???
 *   case s            => ???
 * }
 *
 * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
 */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = Some(seq.drop(seq.length-1).head)

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop[A](a: Seq[A], b: Seq[A], c: Seq[(A, A)]): Seq[(A, A)] = a match {
      case ahead +: atail => b match {
        case blast +: Nil => c :+ (ahead, blast)
        case bhead +: btail => loop(atail,btail, c:+ (ahead,bhead))
      }
      case Nil => c
    }
    loop(a,b,Nil)
  }

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop[A](seq: Seq[A])(cond: A=> Boolean): Boolean = seq match{
      case head +: tail => if (cond(head)) loop(tail)(cond) else false
      case Nil => true
    }
    loop(seq)(cond)
  }

  /* d) Проверьте, является ли Seq палиндромом
   *
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop[A](a: Seq[A], b: Seq[A]): Boolean = a match {
      case head +: tail => loop(tail, b = b :+ head)
      case Nil => seq.equals(b)
    }
    loop(seq, Nil)
  }

  /* e) Реализуйте flatMap используя foldLeft.
   *
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((a, b) =>
   a ++: f(b))

  def main(args: Array[String]): Unit = {
    val a = Seq(1,2,3,4)
    val b = Seq(3,3,3,3)
    println("a) "+testLastElement(a))
    println("b) "+testZip(a, b))
    println("c) "+testForAll(b)((x: Int) => x % 2==0))
    println("d) "+testPalindrom(b))
    println("e) "+testFlatMap(a)((x: Int) => Seq(x*x)))
  }
}
