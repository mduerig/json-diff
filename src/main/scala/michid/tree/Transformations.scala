package michid.tree

import michid.tree.Transformation.{addNode, removeNode}
import michid.tree.Tree.Path

trait Transformation {
  def apply(tree: Tree): Tree
}

class AddNode(path: Path, child: Tree) extends Transformation {
  require(!path.isEmpty)

  def apply(tree: Tree): Tree = path match {
    case name::Nil => tree.setChild(name, child)
    case name::names => addNode(names, child)(tree.child(name))
  }

  override def toString = "+" + path.mkString("/")
}

class RemoveNode(path: Path) extends Transformation {
  require(!path.isEmpty)

  def apply(tree: Tree): Tree = path match {
    case name::Nil => tree.removeChild(name)
    case name::names => removeNode(names)(tree.child(name))
  }

  override def toString = "-" + path.mkString("/")
}

class MoveNode(source: Path, target: Path) extends Transformation {
  def apply(tree: Tree): Tree = {
    val moved = removeNode(source)(tree)
    addNode(target, moved)(tree)
  }

  override def toString = ">" + source.mkString("/") + ":" + target.mkString("/")
}

object Transformation {
  def addNode(path: Path, tree: Tree) = new AddNode(path, tree)
  def addNode(path: Path) = new AddNode(path, Tree())
  def removeNode(path: Path) = new RemoveNode(path)
  def moveNode(source: Path, target: Path) = new MoveNode(source, target)
}