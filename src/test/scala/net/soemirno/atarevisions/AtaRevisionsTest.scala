package net.soemirno.atarevisions

import collection.mutable.HashMap
import java.io.File
import org.scalatest.junit.JUnit3Suite

class AtaRevisionsTest extends JUnit3Suite with Fixtures {
  val PREVIOUS_SOURCE = new File(FIXTURES_FOLDER, "previous.xml")
  val CURRENT_SOURCE = new File(FIXTURES_FOLDER, "current.xml")
  val RESULT_SOURCE = new File(FIXTURES_FOLDER, "result.xml")

  def testHasRevisedElements() = {

    val changes = AtaElement(CURRENT_SOURCE).diff(AtaElement(PREVIOUS_SOURCE))
    val checks = AtaElement(RESULT_SOURCE).revisionIndicators

    for (check <- checks.values) {
      if (check.changeType != "U" && !changes.contains(check.key))
        Console.println("not found: " + check.key + "," + check.changeType)
    }

    Console.println("--- Changes ---")

    for (change: RevisionIndicator <- changes.values) {
      Console.println("detected: " + change.key + "," + change.changeType)
      if (checks.contains(change.key) &&change.changeType != checks(change.key).changeType){
        Console.println("expected: " + change.key + "," + checks(change.key).changeType)
        fail()
      }
    }
  }

}