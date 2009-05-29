package net.soemirno.atarevisions

import java.io.File
import org.scalatest.junit.JUnit3Suite

class AtaRevisionsTest extends JUnit3Suite with Fixtures {
  val PREVIOUS_SOURCE = new File(FIXTURES_FOLDER, "previous.xml")
  val CURRENT_SOURCE = new File(FIXTURES_FOLDER, "current.xml")

  def testCreateAtaDocumentWithNull() = {
    try {
      AtaDocument(null)
      fail("should throw exception")
    }
    catch {
      case iae: IllegalArgumentException =>
    }
  }

  def testCreateAtaDocumentWithInvalidFile() = {
    try {
      AtaDocument(new File("some_file_which_does_not_exist"))
      fail("should throw exception")
    } catch {
      case iae: IllegalArgumentException =>
    }

  }

  def testHasRevisedElements() = {
    val previous = AtaDocument(PREVIOUS_SOURCE)
    val current = AtaDocument(CURRENT_SOURCE)

    //    Console.println("--- Previous ---" )
    //    for (element <- previous.changeList)
    //      Console.println(element )
    //
    Console.println("--- Current ---")
    for (element <- current.changeList)
      Console.println(element)

    val changes = current.diff(previous, "20090201")

    Console.println("--- Changes ---")
    for (change <- changes)
      Console.println(change)
    assert(changes.keySet.contains("W0000019"))
    assert(changes("W0000019").changeType == "N")
    assert(changes("W0000019").date == "20090201")

    assert(changes("W0000002").changeType == "N")
    assert(changes("W0000002").date == "20090201")
  }
}