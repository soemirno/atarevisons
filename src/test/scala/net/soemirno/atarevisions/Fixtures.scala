package net.soemirno.atarevisions

import java.io.File

trait Fixtures {
//  val FIXTURES_FOLDER :File = new File("/tmp")
  val FIXTURES_FOLDER :File = new File(Thread.currentThread().getContextClassLoader().getResource("../test-classes/").getPath)
}
