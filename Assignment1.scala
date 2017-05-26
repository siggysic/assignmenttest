import scala.io.Source

object Assignment1 extends App {

  val TextFile: String = "test.txt"

  // Assignment 2-2
  // def check(str: String): String = {
  //   val results = for {
  //     x <- 0 to str.length
  //     y <- 0 to str.length
  //     if y > x && str.substring(x, y) == str.substring(x, y).reverse
  //   } yield str.substring(x, y)
  //   results.toList.sortWith(_.length > _.length).headOption.getOrElse("")
  // }

  // Assignment 2-3
  // def brackets(str: String) = {
  //   val results = str.startsWith("(") :: str.endsWith(")") :: (str.filter(_ == '(').length == str.filter(_ == ')').length) :: Nil
  //   results.forall(b => b)
  // }

  def group(lists: List[Graph], x: Int = 0, y: Int = 0): List[List[Graph]] = {
    if(lists.isDefinedAt(y)) {
      if(lists(x).vertex2 == lists(y).vertex1 && lists(y).vertex1 < lists(y).vertex2 && lists(x).vertex1 < lists(x).vertex2)
        List(lists(x) :: lists(y) :: Nil) ::: group(lists, x, y+1)
      else group(lists, x, y+1)
    }else if(x < lists.length-1) group(lists, x+1)
    else Nil
  }

  def result(lists: List[List[Graph]], x: Int = 0, y: Int = 0): List[Tuple2[String, String]] = {
    if(x < lists.length) 
      (
        (lists(x)(y).length.toInt + lists(x)(y+1).length.toInt).toString,
        (s"${lists(x)(y).vertex1} -> ${lists(x)(y).vertex2} -> ${lists(x)(y+1).vertex2}")
      ) :: result(lists, x+1)
    else Nil
  }

 val sourceFile: String = Source.fromFile(TextFile).mkString("")
  val formatData: List[Graph] = sourceFile.split("\n").toList.map{sourceLine =>
    val listLine = sourceLine.trim.split(" ").toList
    Graph(listLine(0), listLine(1), listLine(2))
  }
  // println(Console.CYAN + sourceFile + Console.RESET)
  // println(Console.BLUE + group(formatData) + Console.RESET)
  val result: List[Tuple2[Int, String]] = result(group(formatData)).map(res => (res._1.toInt, res._2)).sortWith(_._1 > _._1)
  println(Console.RED + s"Greatest Path : ${result.head._2}" + Console.RESET)
  println(Console.RED + s"Shortest Path : ${result.last._2}" + Console.RESET)
  println(Console.RED + s"Result : ${result.head._1} ${result.last._1}" + Console.RESET)
}

case class Graph(vertex1: String, vertex2: String, length: String)