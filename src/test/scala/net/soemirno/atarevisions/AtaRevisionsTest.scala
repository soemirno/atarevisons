package net.soemirno.atarevisions

import java.io.File
import collection.mutable.Map
import org.scalatest.junit.JUnit3Suite

class AtaRevisionsTest extends JUnit3Suite {
  val FIXTURES_FOLDER :File = new File(Thread.currentThread().getContextClassLoader().getResource("../test-classes/").getPath)
  val PREVIOUS_SOURCE = new File(FIXTURES_FOLDER, "previous.xml")
  val CURRENT_SOURCE = new File(FIXTURES_FOLDER, "current.xml")
  val RESULT_SOURCE = new File(FIXTURES_FOLDER, "result.xml")

  def testHasRevisedElements() = {

    val changes = AtaManual(CURRENT_SOURCE).diff(AtaManual(PREVIOUS_SOURCE), "20090601")
    val expectedList = AtaManual(RESULT_SOURCE).revisionIndicators

    println(AtaManualWriter.updateRevisionIndicators(changes(""), changes))
    for (expected <- expectedList.values) {
      if (!changes.contains(expected.key))
        fail("not found: " + expected.key + "," + expected.changeType)
    }

    for (change <- changes.values) {
      if (expectedList.contains(change.key) && change.changeType != expectedList(change.key).changeType)
        fail("expected: " + change.key + "," + expectedList(change.key).changeType)
    }
  }

}