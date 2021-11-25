"crazy" * 3
//3
10 max 2
//4
BigInt(2).pow(1024)
//5
import scala.util.Random
import scala.math.BigInt.probablePrime
probablePrime(100, Random)
//6
probablePrime(100, Random) toString 36
//7
"Privet".head
"Privet".last
//8
/*take(n:Int) return Строка, содержащая первые n символов этой строки. Получить строку от первого до n-го символа.
drop (n:Int) Остальная часть строки без первых n символов. Возвращает строку без первых n символов.
takeRight(n: Int) Строка, содержащая последние n символов этой строки.
dropRight(n: Int) Остальная часть строки без последних n символов. Вернуть строку без последних n символов.*/
"Privet drug!" take 7
 "Privet drug!" drop 7
 "Privet drug!" takeRight 6
 "Privet drug!" dropRight 6
//9
def signum(n:Int) = if (n>0) 1 else if (n<0) -1 else 0
signum(9)
signum(-3)
signum(0)
//10
/* */
val block = {}
//11
for(i<-10 to 0 by -1) println(i)
  //12
  def countdown(n: Int) = for(i<- n to 0 by -1) println(i)
countdown(7)
  //13
  (for(i<- "Hello") yield i.toLong).product
  //14
  def product(s:String) = s.map(i => i.toLong).product
product("Hello")
  //16
  def product(s:String, p:Long):Long = s.isEmpty match {
  case false => product(s.tail, p * s.head.toLong)
       case true => p
       }
  def prod(s:String):Long = s.isEmpty match {
   case false => product(s, 1)
    case true => 0
    }
  prod("Hello");
//17
def pow(x:Int, n:Int) = n match {
 case n if n > 0 && (n%2 == 0) => scala.math.pow(x, n/2)
 case n if n > 0 && (n%2 == 1) => x * scala.math.pow(x, n-1)
 case 0 => 1
 case n if n < 0 => 1/scala.math.pow(x, -n)
 }
  pow(2, 3)
  pow(2, 4)

  //18

}
//21
def func21(n:List[Any], k:Int):Unit={
var res:List[Any]=List()
for(i<-n) {
  for (j <-1 to k) {
  res = res + i}
}
println(res)
}

