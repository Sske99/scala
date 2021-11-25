import scala.annotation.tailrec

/** Напишите вашу реализацию в тестовые функции.
 *
 * https://docs.scala-lang.org/overviews/collections/maps.html
 */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    def func(seq:Seq[Int]):Int = seq.sum/seq.length
    users.groupBy(_.name).map{
      input => (input._1, func(input._2.map(_.age)))
    }
  }
  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = {
    @tailrec
    def loop(list:List[(String, User)], out: Int):Int = list.length match {
      case 0 => out
      case _ => {
        if(list.head._1.contains("Adam") || list.head._2.name.contains("Adam")){
          if(list.head._1.contains("Adam") && list.head._2.name.contains("Adam"))
            loop(list.tail, out+2)
          else
            loop(list.tail, out+1)}
            else loop(list.tail, out)

        }
      }
    loop(map.toList, 0)
    }


  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
    map.filter(v => v._2.age > 35)
  }

  def main(args: Array[String]): Unit = {
    val user1 = User("Kolya", 10)
    val user2 = User("Petya", 20)
    val user3 = User("Sveta", 40)
    val users:Seq[User] = Seq(user1, user2, user3)
    var map =Map("Oleg" -> User("Adama", 25), "Adam" -> User("Adam", 60))
    println("a) "+testGroupUsers(users))
    println("b) "+testNumberFrodos(map))
    println("c) "+testUnderaged(map))

  }
}
