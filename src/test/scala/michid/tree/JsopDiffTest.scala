package michid.tree

import org.scalatest.Assertions
import org.junit.Test

class JsopDiffTest extends Assertions {

  def diffAndCheck(s: Tree, t: Tree) {
    // Save original for latter comparison
    val original = Tree.copy(s)

    val jsop = JsopDiff.diffTrees(s, t)

    // Applying jsop to original tree should result in the target tree
    for (op <- jsop) {
      op(original)
    }
    assert(original === t)
  }

  @Test
  def diffEmptyTrees() {
    val s = Tree()

    val t = Tree.copy(s)

    diffAndCheck(s, t)
  }

  @Test
  def diffAddNode() {
    val s = Tree()

    val t = Tree.copy(s)
    t.addChild("a")

    diffAndCheck(s, t)
  }

  @Test
  def removeNode() {
    val s = Tree()
    s.addChild("a")

    val t = Tree.copy(s)
    t.removeChild("a")

    diffAndCheck(s, t)
  }

  @Test
  def moveNode() {
    val s = Tree()
    s.addChild("a").addChild("x")
    s.addChild("b")

    val t = Tree.copy(s)
    t.child("a").child("x").moveTo(t.child("b"), "y")

    diffAndCheck(s, t)
  }

  @Test
  def renameNode() {
    val s = Tree()
    s.addChild("a")

    val t = Tree.copy(s)
    t.child("a").moveTo(t, "b")

    diffAndCheck(s, t)
  }

  @Test
  def insertNode() {
    val s = Tree()
    s.addChild("a")
    val b = s.addChild("b")
    b.addChild("c")
    b.addChild("d")

    val t = Tree.copy(s)
    val x = t.addChild("x")
    t.child("b").moveTo(x, "b")

    diffAndCheck(s, t)
  }

  @Test
  def moveAndModify() {
    val s = Tree()
    s.addChild("a")
    val b = s.addChild("b")
    b.addChild("c").addChild("e")
    b.addChild("d")

    val t = Tree.copy(s)
    t.child("b").moveTo(t.child("a"), "b")
    t.child("a").child("b").child("c").removeChild("e")

    diffAndCheck(s, t)
  }

  @Test
  def modifyAndMove() {
    val s = Tree()
    s.addChild("a")
    val b = s.addChild("b")
    b.addChild("c").addChild("e")
    b.addChild("d")

    val t = Tree.copy(s)
    t.child("b").child("c").removeChild("e")
    t.child("b").moveTo(t.child("a"), "b")

    diffAndCheck(s, t)
  }

  @Test
  def revert() {
    val s = Tree()
    s.addChild("a").addChild("b")

    val t = Tree.copy(s)
    t.child("a").child("b").moveTo(t, "b")
    t.child("a").moveTo(t.child("b"), "a")

    diffAndCheck(s, t)
  }

  @Test
  def moveMoved() {
    val s = Tree()
    s.addChild("a")
    val b = s.addChild("b")
    b.addChild("c").addChild("e")
    b.addChild("d")

    val t = Tree.copy(s)
    t.child("b").moveTo(t.child("a"), "b")
    t.child("a").child("b").child("c").moveTo(t, "c")

    diffAndCheck(s, t)
  }

  @Test
  def movedMoves() {
    val s = Tree()
    s.addChild("a")
    val b = s.addChild("b")
    b.addChild("c").addChild("e")
    b.addChild("d")

    val t = Tree.copy(s)
    t.child("b").child("c").moveTo(t, "c")
    t.child("b").moveTo(t.child("a"), "b")

    diffAndCheck(s, t)
  }

}
