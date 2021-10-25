package chapter7

import utils.BaseSpec
import Chapter7._
import java.util.concurrent.Executors

class Chapter7ExercisesSpec extends BaseSpec {

  ignore("Ex. 7.1 - Implement map2 which combines result of 2 parallel computation Par") {
    // Implemented method - map2
  }

  ignore("Ex. 7.2 - Create representation of Par") {
    // Implemented Par
  }

  ignore("Ex. 7.3 - fix map2 to use respect contract of timeouts on Future") {
    // Fixed map2 to use MapFuture2
  }

  test("Ex. 7.4 - Implement asyncF using lazyUnit") {
    def asyncIntToString = asyncF((x: Int) => x.toString())
    val executorService  = Executors.newFixedThreadPool(2)

    asyncIntToString(10).run(executorService).get() shouldBe "10"
  }

  ignore("Ex. 7.5 - Implement sequence method for Par") {
    // Implemented method - sequence
  }

  test("Ex. 7.6 - Implement parFilter method which filters elements in parallel") {
    val filterOp        = parFilter(List(1, 2, 3, 4, 5))(_ < 4)
    val executorService = Executors.newCachedThreadPool()
    val result          = (filterOp).run(executorService).get()

    result shouldBe List(1, 2, 3)
  }

}
