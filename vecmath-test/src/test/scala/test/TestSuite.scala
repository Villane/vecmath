package test

import org.villane.vecmath._
import org.villane.vecmath.Preamble._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class TestSuite extends FunSuite with ShouldMatchers {

  test("Test1 should give correct results") {
    import Test1._
    compute(5, 5) should be (Vector2(8, 8))
    compute2((5, 5), (6, 6)) should be (Vector2(-2, -2))
    computeNormal(1,3) should be (Vector2(-6, 2))
    compute3((7, 3),(6, 2)) should be (Vector2(-5, -1))
  }

}
