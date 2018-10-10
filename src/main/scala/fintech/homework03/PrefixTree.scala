package fintech.homework03

// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]
  def get: V

  def getTreeString(indent: Int): String
}

case class RootTrieNode[K, +V](children: Map[K, PrefixTree[K, V]] =
                                 Map.empty[K, PrefixTree[K, V]]) extends PrefixTree[K, V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U] = {
    if (path.isEmpty)
      throw new IllegalArgumentException
    else
    if (path.tail.isEmpty) {
      if (children.get(path.head).isDefined)
        copy(children = children.updated(path.head, children(path.head).put(path.tail, newValue)))
      else
        copy(children = children.updated(path.head, TrieNode(newValue, path.head)))
    } else {
      if (children.get(path.head).isDefined)
        copy(children = children.updated(path.head, children(path.head).put(path.tail, newValue)))
      else {
        copy(children = children.updated(path.head, EmptyTrieNode(path.head).put(path.tail, newValue)))
      }
    }
  }

  def sub(path: Seq[K]): PrefixTree[K, V] = this

  def get: V = throw new NoSuchElementException

  override def toString: String = "Root" + getTreeString(2)

  def getTreeString(indent: Int): String = {
    children.keys.foldLeft("")((acc, key) => {
      acc + "\n" + (" " * indent) + children(key).getTreeString(indent + 2)
    })
  }
}

case class EmptyTrieNode[K, +V](id: K,
                                children: Map[K, PrefixTree[K, V]] = Map.empty[K, PrefixTree[K, V]]
                               ) extends PrefixTree[K, V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U] = {
    if (path.isEmpty)
      TrieNode(newValue, id, children)
    else
    if (path.tail.isEmpty) {
      if (children.get(path.head).isDefined)
        copy(children = children.updated(path.head, children(path.head).put(path.tail, newValue)))
      else
        copy(children = children.updated(path.head, TrieNode(newValue, path.head)))
    } else {
      if (children.get(path.head).isDefined)
        copy(children = children.updated(path.head, children(path.head).put(path.tail, newValue)))
      else {
        copy(children = children.updated(path.head, EmptyTrieNode(path.head).put(path.tail, newValue)))
      }
    }
  }

  def sub(path: Seq[K]): PrefixTree[K, V] = ???

  def get: V = throw new NoSuchElementException

  override def toString: String = getTreeString(0)

  def getTreeString(indent: Int): String = {
    children.keys.foldLeft(id + "()")((acc, key) => {
      acc + "\n" + (" " * indent) + children(key).getTreeString(indent + 2)
    })
  }
}

case class TrieNode[K, +V](value: V, id: K,
                           children: Map[K, PrefixTree[K, V]] = Map.empty[K, PrefixTree[K, V]]
                           ) extends PrefixTree[K, V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U] = {
    if (path.isEmpty)
      copy(value = newValue)
    else {
      if (path.tail.isEmpty) {
        if (children.get(path.head).isDefined)
          copy(children = children.updated(path.head, children(path.head).put(path.tail, newValue)))
        else
          copy(children = children.updated(path.head, TrieNode(newValue, path.head)))
      } else {
        if (children.get(path.head).isDefined)
          copy(children = children.updated(path.head, children(path.head).put(path.tail, newValue)))
        else {
          copy(children = children.updated(path.head, EmptyTrieNode(path.head).put(path.tail, newValue)))
        }
      }
    }
  }

  def sub(path: Seq[K]): PrefixTree[K, V] = ???

  def get: V = value

  override def toString: String = getTreeString(0)

  def getTreeString(indent: Int): String = {
    children.keys.foldLeft(id + "(" + value + ")")((acc, key) => {
      acc + "\n" + (" " * indent) + children(key).getTreeString(indent + 2)
    })
  }
}

object Main extends App {
  println(RootTrieNode().put("abc", 1).put("abd", 2))

  val firstNode = RootTrieNode().put("a", 1)
  println(firstNode)
  val secondNode = firstNode.put("bc", 3)
  println(secondNode)
  println(secondNode.put("bc", 2))

  println(RootTrieNode(Map(
    'a' -> EmptyTrieNode('a'),
    'b' -> EmptyTrieNode('b', Map(
      'c' -> TrieNode(1, 'c'),
      'd' -> TrieNode(2, 'd')
    ))
  )))
}