object Main extends Solution {
  override def solve(): Any = {
    println(numDecodings(readLine))
  }
  def numDecodings(s: String): Int = {
    val t = s.map(c => c - '0');
    lazy val f: Int => Int = memoize {
      case -1 => 1
      case 0  => (if (t(0) != 0) f(-1) else 0)
      case x =>
        (if (9 < t(x - 1) * 10 + t(x) && t(x - 1) * 10 + t(x) <= 26) f(x - 2)
         else 0) + (if (t(x) != 0) f(x - 1) else 0)
    }
    f(s.length - 1)
  }
}

trait Solution {
  import java.io._

  val cases = 4
  val isLocal: Boolean = sys.props.get("CLown1331").exists(_.toBoolean)
  val isFileInput: Boolean = isLocal
  val isFileOutput: Boolean = false

  val input = "res/in.txt"
  val output = "res/out.txt"

  val bf = new BufferedReader(
    if (isFileInput) new FileReader(input) else new InputStreamReader(System.in)
  )
  val in = new StreamTokenizer(bf)
  val out = new PrintWriter(
    if (isFileOutput) new FileWriter(output)
    else new OutputStreamWriter(System.out)
  )

  def debug(x: => Any): Unit = if (isLocal) println(x)

  def solve(): Any

  def main(args: Array[String]): Unit = {
    if (isLocal)
      (1 to cases).foreach(_ => solve())
    else
      solve()
    flush()
  }
  def memoize[I, O](f: I => O): I => O =
    new scala.collection.mutable.HashMap[I, O]() {
      override def apply(key: I): O = getOrElseUpdate(key, f(key))
    }
  def next: String = {
    in.ordinaryChars('0', '9')
    in.wordChars('0', '9')
    in.nextToken()
    in.sval
  }

  def nextInt: Int = {
    in.nextToken()
    in.nval.toInt
  }

  def nextLong: Long = {
    in.nextToken()
    in.nval.toLong
  }

  def nextDouble: Double = {
    in.nextToken()
    in.nval
  }

  def readLine: String = bf.readLine()

  def println(o: Any): Unit = out.println(o)

  def print(o: Any): Unit = out.print(o)

  def printf(s: String, o: Any*): PrintWriter = out.printf(s, o)

  def flush(): Unit = out.flush()
}
