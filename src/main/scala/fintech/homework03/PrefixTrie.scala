package fintech.homework03

// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

// TODO: написать Scaladoc

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]
  def get: V
  def isEmpty: Boolean

  def getTreeString(indent: Int): String
}

case class Trie[K, +V](id: Option[K], value: Option[V],
                       children: Map[K, PrefixTree[K, V]] = Map.empty[K, PrefixTree[K, V]]
                       ) extends PrefixTree[K, V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U] = {
    // Явно конвертируем path в List, чтобы у нас был метод cons для удобного паттерна head :: tail
    // Иначе мы не можем работать с Range, WrappedString и т.п.
    path.toList match {
      case Nil          => copy(value = Some(newValue))
      case head :: tail => children.get(head) match {
        case Some(childTrie) =>
          copy(children = children.updated(head, childTrie.put(tail, newValue)))
        case None =>
          if (tail.isEmpty)
            copy(children = children.updated(head, Trie(Some(head), Some(newValue))))
          else
            copy(children = children.updated(head, Trie.empty(Some(head)).put(tail, newValue)))
        }
    }
  }

  def sub(path: Seq[K]): PrefixTree[K, V] = {
    if (path.isEmpty)
      this
    else if (children.get(path.head).isDefined)
      children(path.head).sub(path.tail)
    else
      Trie.empty()
  }

  def get: V = value.get
  def isEmpty: Boolean = value.isEmpty
  override def toString: String = getTreeString(2)

  def getTreeString(indent: Int): String = {
    val startString = id match {
      case Some(idValue) => idValue + "(" + value.getOrElse("") + ")"
      case None          => "Root"
    }

    children.keys.foldLeft(startString)((acc, key) => {
      acc + "\n" + (" " * indent) + children(key).getTreeString(indent + 2)
    })
  }
}

object Trie {
  def empty[K, V](id: Option[K] = None) = Trie(id, None)
}

object Main extends App {
  val complexTrie = Trie.empty().put("abc",  1   ).
                                 put("ad",   '2' ).
                                 put("abe",  3.0 ).
                                 put("abcf", true)
  println(complexTrie)
  println(complexTrie.sub("abc").get)
  println(complexTrie.sub("ad").get)
  println(complexTrie.sub("abe").get)
  println(complexTrie.sub("abcf").get)
  // println(complexTrie.sub("abf").get) // проверка на вылет Exception
}