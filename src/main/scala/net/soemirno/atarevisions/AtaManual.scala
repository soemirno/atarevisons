package net.soemirno.atarevisions

import collection.mutable.HashMap
import collection.mutable.Map
import java.io.File
import org.slf4j.LoggerFactory
import xml.Node

/**
 * factory methods for AtaManual
 */
object AtaManual {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  /**
   *  Construct manual from source file  
   */
  def apply(sourceFile: File, noRevdate : Boolean) = {
    
    logger.info("start loading " + sourceFile)
    val document = xml.XML loadFile (sourceFile)
    logger.info("finished loading " + sourceFile)
    
    logger.info("start mapping key to elements")    
    val map = mapElements(document, noRevdate)
    logger.info("finished mapping key to elements")
    
    new AtaManual(map)
  }
  
  private def mapElements(document : Node, noRevdate : Boolean) :Map[String, RevisionIndicator] = {
    val elementsContainingRevInd = (document \\ "_").filter(e => e \ "@chg" != "")
    val map = HashMap[String, RevisionIndicator]()

    //place all key elements in a hashmap
    for (element <- elementsContainingRevInd) {
      val rev = new RevisionIndicator(element, noRevdate)
      map += Pair(rev.key, rev)
    }
    return map
  }

}

/**
 * represents manual to be compared to or with
 */
class AtaManual(revIndicators: Map[String, RevisionIndicator]) {
  val logger = LoggerFactory.getLogger(this.getClass)

  def revisionIndicators(): Map[String, RevisionIndicator] = revIndicators

  /**
   * Compare manuals
   */
  def diff(previous: AtaManual, revDate :String): Map[String, RevisionIndicator] = {
    val result = new HashMap[String, RevisionIndicator]
    val previousIndicators = previous.revisionIndicators

    result ++ findChanges(previousIndicators, revDate)
    result ++ findDeleted(previousIndicators, revDate)

    return rolledUpResults(result, revDate)
  }

  /**
   *  make sure parent elements reflect changes in children
   */
  def rolledUpResults(result: Map[String, RevisionIndicator], revDate :String): Map[String, RevisionIndicator] = {
    logger.info("rollup results")
    var foundChange = true

    while (foundChange) {
      foundChange = false
      for (rev <- result.values if rev.changeType() == "U") {
        for (child <- rev.child if (child \ "@chg" != "")) {
          if (result((child \ "@key").text).changeType != "U") {
            logger.info("rolling up " + rev.key)
            foundChange = true
            result += Pair(rev.key, new RevisionIndicator(rev.key, "R", revDate, rev))
          }
        }
      }
    }

    return result
  }

  /**
   * Find elements removed in this revision
   */
  def findDeleted(prevChanges: Map[String, RevisionIndicator], revDate :String): Map[String, RevisionIndicator] = {
    val result = new HashMap[String, RevisionIndicator]
    logger.info("finding deleted elements")

    for (rev <- prevChanges.values if !revIndicators.contains(rev.key()))
      result += Pair(rev.key, new RevisionIndicator(rev.key, "D", revDate, rev))

    return result
  }

  /**
   * Find elements changed in this revision
   */
  def findChanges(prevChanges: Map[String, RevisionIndicator], revDate :String): Map[String, RevisionIndicator] = {
    val result = new HashMap[String, RevisionIndicator]
    logger.info("finding changed elements")

    for (rev <- revIndicators.values) {

      if (!prevChanges.contains(rev.key()) || prevChanges(rev.key()).changeType == "D")
        result += Pair(rev.key, new RevisionIndicator(rev.key, "N", revDate, rev))

      else if (rev isSameAs prevChanges(rev.key))
        result += Pair(rev.key, new RevisionIndicator(rev.key, "U", prevChanges(rev.key).revdate, rev))

      else
        result += Pair(rev.key, new RevisionIndicator(rev.key, "R", revDate, rev))
    }
    return result
  }

}