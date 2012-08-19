/**
 * @lincese mit
 * Copyright © 2012 Masanori Matsumoto
 */
import scala.sys.process._
import scala.collection._
import java.io.File


object PlovrBuild {
  def usage = """
Usage: java -jar xxx.jar [options]

  -c JS dir. ex) etc/plovr
     The directory must have plovr/release directory in it.
     uses src/main/webapp/static/js by default.

  -j The number of processes to execute.
     Plovr may consumes 0.5 to 2GB memory so take care with the option.
     It uses the half of the number of processor by default.

  -s The column separated pathes to enable strict mode.
     If the warning occurs, it returns 1.
"""

  val BUILD_FAILED = 1
  val WRONG_ARGUMENT = 2
  val NOTHING_TO_BUILD = 3

  /**
   * Makes compiler context from console parameters.
   *
   * @param arglist The console paraters
   * @return The context.
   */
  def makeContext(arglist: List[String]): Context = {
    type OptionMap = Map[Symbol, Any]
    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "-p" :: value :: tail =>
          nextOption(map ++ Map('plovr -> value), tail)
        case "-j" :: value :: tail =>
          nextOption(map ++ Map('jobs -> value.toInt), tail)
        case "-c" :: value :: tail =>
          nextOption(map ++ Map('configdir -> value), tail)
        case "-s" :: value :: tail =>
          nextOption(map ++ Map('strict -> value.split(",").toList), tail)
        case string :: Nil =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case option :: tail =>
          println("Unknown option " + option)
          System.exit(WRONG_ARGUMENT)
          map // for type
      }
    }

    def defaultParallelism: Int = {
      import scala.collection.parallel._
      math.max(availableProcessors/2, 1)
    }

    val options = nextOption(Map(), arglist)

    Context(
      options.getOrElse('plovr, "plovr.jar").asInstanceOf[String],
      options.getOrElse('configdir, ".").asInstanceOf[String],
      options.getOrElse('jobs, defaultParallelism).asInstanceOf[Int],
      options.getOrElse('strict, List()).asInstanceOf[List[String]])
  }

  /**
   * Main method.
   *
   * @param args The command arguments.
   */
  def main(args: Array[String]) {
    if (args.length == 0) {
      println(usage)
      System.exit(WRONG_ARGUMENT)
    }

    val ctx = makeContext(args.toList)
    scala.collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(ctx.parallelism)

    val started = new java.util.Date().getTime
    val (results, noWarnings) = buildMain(ctx)
    val end = new java.util.Date().getTime

    println("It took %d sec." format (end - started) / 1000)

    if (results.isEmpty) {
      println("Nothing to build")
      System.exit(NOTHING_TO_BUILD)
    } else if (noWarnings && !results.forall(_.code == 0))
      println(Console.GREEN + "[Success] JS compile" + Console.RESET)
    else {
      println(Console.RED + "[Fail] JS compile" + Console.RESET)
      System.exit(BUILD_FAILED)
    }
  }

  /**
   * Builds javascripts.
   *
   * @param ctx The build context.
   * @return
   */
  def buildMain(ctx: Context): (parallel.ParSeq[ProcResponse], Boolean) = {

    // build js files.
    val results = buildByGroup(ctx)

    if (ctx.isStrict) {
      val warnings =
        for {
          res <- results
          fnmap = convertFileNameMap(parsePlovrMessages(res.err))
          entry <- fnmap
          path = entry._1
          if ctx.isStrictPath(path)
        } yield entry

      val noWarnings = warnings.isEmpty
      if (!noWarnings) {
        printWarnings(warnings)
      }

      (results, noWarnings)
    } else
      (results, true)
  }

  /**
   * Print out aggregated warnings.
   * @param
   */
  def printWarnings(warnings: parallel.ParSeq[(String, mutable.LinkedHashSet[PlovrMessage])]) {
      println()
      println(Console.RED + "*" * 80 + Console.RESET)
      println(Console.RED + "  ERROR: Warnings detected" + Console.RESET)
      println(Console.RED + "*" * 80 + Console.RESET)

      warnings foreach (entry => {
        println("=" * 80)
        println(Console.RED + "  " + entry._1 + Console.RESET)
        println("-" * 80)
        entry._2.foreach(println)
      })
  }

  /**
   * The context of plovr compiler.
   * @param plovr The plovr.jar path.
   * @param configDir The directory where config files exists.
   * @param The list of pathes in which js fiels warnings treated as error.
   */
  case class Context(plovr: String, configDir: String, parallelism: Int, strict: List[String]) {
    private var settingsCache: Option[Seq[File]] = None

    /**
     * Returns list of files within the config directory
     *
     * @return The config files.
     */
    def settings(): Seq[File] = {
      def findFiles = {
        var dir = new File(configDir)
        if (dir.exists)
          dir listFiles (new java.io.FileFilter {
            def accept(pathname: File) = {
              pathname.getName.endsWith(".json")
            }
          }) toSeq
        else
          Seq()
      }

      settingsCache match {
        case Some(files) => files
        case None => {
          val files = findFiles
          settingsCache = Option(files)
          files
        }
      }
    }

    /**
     * The size of jobs each plovr compiles
     *
     * @return The size.
     */
    def groupSize = math.ceil(settings.length.toDouble / parallelism).toInt

    /**
     * Strict mode.
     *
     * @return True if any strict pathes exists, False otherwise
     */
    def isStrict() = !strict.isEmpty;

    /**
     * Checks if the given path is strict or not.
     *
     * @return The strict pthes.
     */
    def isStrictPath(path: String) =
      strict.contains((s:String) => s.contains(path))

  }

  /**
   * The plovr process response.
   *
   * @param file The files processed.
   * @param code The response code.
   * @param std The standard outputs.
   * @param err The error outputs.
   */
  case class ProcResponse(file: Seq[File], code: Int,
                          std: List[String], err: List[String])

  /**
   * The plovr process warning output.
   *
   * @param typ The type of warning.
   * @param desc The warning description
   * @param found The found expression.
   * @param required The required expression.
   * @param path The file path.
   * @param line The line number.
   * @param pos The position number.
   */
  case class PlovrWarning(typ: String, desc: String,
                          found: Option[String], required: Option[String],
                          path: String, line: Int, pos: Int)

  /**
   * The plovr process output
   *
   * @param warning The list of strings
   */
  case class PlovrMessage(warning: List[String]) {
    private val r =
      """([\w_]+)\. ([^\n]*)(?:\n(found +: .*)\n(required: .*))?at ([\w./]+) line (\d+) : (\d+)""".r

    val detail = r findFirstIn warning.mkString("\n") match {
      case Some(r(typ, desc, found, required, file, line, pos)) =>
        Some(PlovrWarning(typ, desc, Option(found), Option(required),
                          file, line.toInt, pos.toInt))
      case None =>
        None
    }

    override def toString: String = detail match {
      case Some(PlovrWarning(typ, desc, found, required, path, line, pos)) => {
        val sb = new mutable.StringBuilder()
        sb ++= path ++ ":(" ++ line.toString ++ "," ++ pos.toString ++ "): "
        sb ++= typ ++ ": " ++ desc

        if (!found.isEmpty)
          sb ++= "\n " ++ found.get
        if (!required.isEmpty)
          sb ++= "\n " ++ required.get
        sb ++= "\n"

        sb.toString
      }
      case None => warning.mkString("\n")
    }
  }

  /**
   * Parses plovr outputs
   *
   * @param in the outputs.
   * @return the list of parsed messages.
   */
  def parsePlovrMessages(in: List[String]): List[PlovrMessage] = {
    @scala.annotation.tailrec
    def take(in: List[String], out: List[PlovrMessage]): List[PlovrMessage] =
      if (in.isEmpty) out
      else take(in.dropWhile(!_.trim.isEmpty()).dropWhile(_.trim.isEmpty),
                PlovrMessage(in.takeWhile(!_.trim.isEmpty()).map(_.trim())) :: out)
    take(in, Nil).reverse
  }

  /**
   * Converts fiel names to path to hash set.
   *
   * @param warns The plovr messages.
   */
  def convertFileNameMap(messages: List[PlovrMessage]): Map[String, mutable.LinkedHashSet[PlovrMessage]] = {
    type MessageSet = mutable.LinkedHashSet[PlovrMessage]
    val map = mutable.Map[String, MessageSet]()
    messages.foreach( w => {
      val key = if (w.detail.isEmpty) "|" else w.detail.get.path
      map.getOrElseUpdate(key, mutable.LinkedHashSet()) += w
    })
    map.toMap
  }

  /**
   * Creates process builder for plovr.
   *
   * @param ctx The context.
   * @param files The config files.
   */
  def buildProcess(ctx: Context, files: Seq[File]): ProcessBuilder =
    Process("java",
            Seq("-jar", ctx.plovr, "build") ++
            files.map(_.getAbsolutePath))


  /**
   * Builds javascripts by multiple process
   *
   * @param ctx The context.
   */
  def buildByGroup(ctx: Context): parallel.ParSeq[ProcResponse] = {
    if (ctx.groupSize == 0)
      parallel.ParSeq()
    else
      ctx.settings.grouped(ctx.groupSize).filter(!_.isEmpty).
        toSeq.par.map(build(ctx, _))
  }

  /**
   * Builds javascripts.
   *
   * @param ctx The context.
   * @param xs The list of setting files.
   */
  def build(ctx:Context, xs: Seq[File]): ProcResponse = {
    var stdouts = mutable.ListBuffer[String]()
    var stderrs = mutable.ListBuffer[String]()

    val processIO = new ProcessIO(_.close, _.close, err => {
      import java.io._
      val reader = new BufferedReader(new InputStreamReader(err))
      val obuffer = mutable.ListBuffer[String]()

      @scala.annotation.tailrec
      def readLine(i: Int): Unit = reader.readLine() match {
        case null => ()
        case line if line.startsWith("ATTENTION:") => {
          val headline =
            "\n" + "=" * 80 + "\n  " + xs(i).getPath() + "\n" + "-" * 80 + "\n"
          println(obuffer.mkString(headline, "\n", "\n" + line))
          stderrs ++= obuffer
          obuffer.clear()
          readLine(i + 1)
        }
        case line => {
          obuffer += line
          readLine(i)
        }
      }
      readLine(0)
    })

    val proc = buildProcess(ctx, xs).run(processIO)

    try {
      // wait process exit here.
      val exitValue = proc.exitValue

      ProcResponse(xs, exitValue, stdouts.toList, stderrs.toList)
    } finally {
      // make sure to destoroy child processes.
      proc.destroy()
    }
  }
}

