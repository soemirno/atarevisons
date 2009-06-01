package net.soemirno.atarevisions

import collection.mutable.HashMap
import java.io.File
import org.scalatest.junit.JUnit3Suite
import xml.{Text,NodeSeq, Group, Node, Elem}
class AtaRevisionsTest extends JUnit3Suite with Fixtures {
  val PREVIOUS_SOURCE = new File(FIXTURES_FOLDER, "previous.xml")
  val CURRENT_SOURCE = new File(FIXTURES_FOLDER, "current.xml")
  val RESULT_SOURCE = new File(FIXTURES_FOLDER, "result.xml")

  def testCreateAtaDocumentWithInvalidFile() = {
    try {
      AtaElement(new File("some_file_which_does_not_exist"))
      fail("should throw exception")
    } catch {
      case iae: IllegalArgumentException =>
    }

  }


  def testHasRevisedElements() = {

    val changes = AtaElement(CURRENT_SOURCE).diff(AtaElement(PREVIOUS_SOURCE), "20090201")
    val checks = AtaElement(RESULT_SOURCE).revisionIndicators

    for (check <- checks.values) {
      if (check.changeType != "U" && !changes.contains(check.key))
        Console.println("not found: " + check)
    }

    Console.println("--- Changes ---")
    for (change: RevisionIndicator <- changes.values) {
      Console.println("detected: " + change.key + "," + change.changeType)

      if (!checks.contains(change.key))
        Console.println("missing: " + change.key + "," + change.changeType)

      else if (change.changeType != checks(change.key).changeType)
        Console.println("expected: " + change.key + "," + checks(change.key).changeType)
    }
  }

  def equalsWithoutText(thisElem: Node, thatElem: Node): Boolean = {
    if (thisElem.child.length == 1 && thisElem.child(0).isInstanceOf[Text])
      return thisElem.text == thatElem.text 

    return ((thatElem.prefix == thisElem.prefix)
            && (thatElem.label == thisElem.label)
            && (thatElem.attributes.filter(a => a.key != "chg" && a.key != "revdate") == thisElem.attributes.filter(a => a.key != "chg" && a.key != "revdate") )
            && hasSameChildren(thatElem.child,thisElem.child)
            )
  }

  def hasSameChildren(a :NodeSeq, b :NodeSeq):Boolean = {
    val ita = a.filter(e => e.isInstanceOf[Elem]).elements
    val itb = b.filter(e => e.isInstanceOf[Elem]).elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (equalsWithoutText(ita.next, itb.next))
    }
    !ita.hasNext && !itb.hasNext && res
  }


  def testElemEquals() = {

    val thisElem = <root chg="U" revdate="20090512"><dd a='a' b='b'>hallo</dd>
<ee><ff/>
<ff/></ee></root>
    val thatElem = <root chg="x" revdate="TODOTODO"><dd b='b' a='a'>hallo</dd><ee><ff/><ff/></ee></root>

    for (e <- thisElem.elements)
      Console.println(e)

    assert(equalsWithoutText(thisElem, thatElem))

  }
}