package michid.tree

import michid.tree.Transformation.{removeNode, moveNode, addNode}
import collection.mutable

object JsopDiff {
  private var counter = 0

  def createUniqueName(name: String): String = {
    counter += 1
    name + "-" + counter
  }

  /* Create a JSOP journal, which when applied to tree s will transform
     it to tree t. */
  def diffTrees(s0: Tree, t0: Tree): Iterable[Transformation] = {

    // List holding the JSOP journal after diffTree below returns
    val jsop = mutable.MutableList[Transformation]()

    // Create a location (trash) which will temporarily hold removed nodes.
    // This is necessary since these (or child nodes thereof) might still be
    // needed in move operations occurring only later in the differencing process.
    val op = addNode(s0.path :+ createUniqueName("trash"))
    jsop += op
    val trash = op(s0)

    /* Recursively create JSOP operations for the differences of the children
       of trees S and T. */
    def diffNodes(s: Tree, t: Tree) {
      val deleted = s.childNames.toSet -- t.childNames.toSet - trash.name
      val added   = t.childNames.toSet -- s.childNames.toSet

      // Need to take care of deleted nodes first in order to avoid
      // name clashes when adding new nodes later.
      for (d <- deleted map (s.child(_))) {
        // Deleted nodes are moved to trash.
        val op = moveNode(d.path, trash.path :+ createUniqueName(d.name))
        jsop += op
        op(s0)      // Transform s0 according to the single op
      }

      // Added nodes might have been moved here from their current source
      // or just added as new
      for (a <- added map (t.child(_))) {
        val op = a.source match {
          case Some(source) => moveNode(source.path, a.path)
          case None         => addNode(a.path)
        }

        jsop += op
        op(s0)      // Transform s0 according to the single op
      }

      for (c <- t.childNames) {
        diffNodes(s.child(c), t.child(c))
      }
    }

    // The main differencing process starts at the roots of the trees and
    // progresses recursively level by level.
    diffNodes(s0, t0)

    // Remove the trash location and all its content
    jsop += removeNode(trash.path)
  }

}
