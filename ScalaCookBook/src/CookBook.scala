/**
 * Created by Korhan on 15/05/2015.
 */
object CookBook {

  def main (args: Array[String]) {

    val numbers = List(1,2,3,4,5,6)

    val trimmedList = numbers drop 2 take 3 reverse

    val filtered = numbers ::: trimmedList filter(a => a > 3)
    println(filtered)
  }

}
