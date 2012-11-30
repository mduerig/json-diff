package michid.tree

import collection.mutable
import michid.tree.Tree.Path

sealed class Tree(var parent: Option[Tree], var name: String, val source: Option[Tree]) {
  private val children = mutable.Map[String, Tree]()

  def path: Path = parent match {
    case None => List()
    case Some(tree) => tree.path :+ name
  }

  def child(name: String): Tree = children(name)
  def childNames: Iterable[String] = children.keys

  def setChild(name: String, tree: Tree): Tree = {
    children(name) = tree
    tree.parent = Some(this)
    tree.name = name
    tree
  }

  def addChild(name: String) = setChild(name, Tree())

  def removeChild(name: String): Tree = {
    val removed = children remove name
    for (r <- removed) {
      r.parent = None
      r.name = ""
    }
    removed.getOrElse {
      throw new NoSuchElementException(name)
    }
  }

  def moveTo(newParent: Tree, newName: String) = {
    for (p <- parent) p.removeChild(name)
    newParent.setChild(newName, this)
  }

  override def toString = {
    def listChildren: Iterable[String] =
      for (c <- children.values)
      yield c.toString

    name + ":{" + listChildren.mkString(",") + "}"
  }

  override def equals(other: Any) = other match {
    case that: Tree => this equals that
    case _ => false
  }

  def equals(that: Tree) = {
    name == that.name &&
    children.zip(that.children).forall{case (c1, c2) => c1 == c2}
  }
}

object Tree {
  type Path = List[String]
  def apply() = new Tree(None, "", None)
  def apply(source: Tree) = new Tree(None, "", Some(source))

  def copy(tree: Tree): Tree = {
    val t = Tree(tree)
    for ((name, child) <- tree.children) {
      t.setChild(name, copy(child))
    }
    t
  }
}