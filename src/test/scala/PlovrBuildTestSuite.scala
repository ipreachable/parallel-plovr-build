import org.scalatest.FunSuite

class PlovrBuildTestSuite extends FunSuite {

  test("default context") {
    val ctx = PlovrBuild.makeContext(List())
    assert("plovr.jar" == ctx.plovr)
    assert("." == ctx.configDir)
    assert(ctx.parallelism > 0)
    assert(List() == ctx.strict)
  }

  test("commandline parameters -j <The number of jobs>") {
    val ctx = PlovrBuild.makeContext(
      List("-j", "8", "-p"))
    println(ctx.parallelism)
    assert(8 == ctx.parallelism)
  }

  test("commandline parameters -p <plovr.jar>") {
    val ctx = PlovrBuild.makeContext(
      List("-j", "8", "-p", "bin/plovr.jar"))
    assert("bin/plovr.jar" == ctx.plovr)
  }

  test("commandline parameters -c <config dir>") {
    val ctx = PlovrBuild.makeContext(
      List("-j", "8", "-c", "src/main/resources/config/plovr"))
    assert("src/main/resources/config/plovr" == ctx.configDir)
  }

  test("commandline parameters -s <strict path>") { 
    val ctx = PlovrBuild.makeContext(
      List("-j", "8", "-s", "/strict/,/strict2/"))
    assert(List("/strict/", "/strict2/") == ctx.strict) 
  }


}
