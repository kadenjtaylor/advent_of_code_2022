package dev.kaden

import cats.effect.IO
import weaver._

object HelloWorldSuite extends SimpleIOSuite {

  // Easy to test stuff with no side effects...
  pureTest("Ensure 1 + 1 = 2") {
    expect(1 + 1 == 2)
  }

}
