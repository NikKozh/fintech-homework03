package fintech.homework03

/** Позволяет организовать префиксное дерево: складывать объекты произвольного класса `V`
  * по заданному "пути" Seq[`K`] в дерево и изымать их, используя комбинацию методов sub и get
  *
  * @tparam K класс элементов, из которых состоит "путь" Seq[K]
  * @tparam V класс объектов, хранящихся в узлах дерева
  */
trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], newValue: U): PrefixTree[K, U]
  def sub(path: Seq[K]): PrefixTree[K, V]

  def get: V
  def isExisting: Boolean

  def getTrieString(indent: Int): String
}

/** Реализация интерфейса [[fintech.homework03.PrefixTree]]
  *
  * @param id       идентификатор, характеризующий положение этого узла в дереве. Отсутствие `id`
  *                 означает, что узел является корнем дерева
  *
  * @param value    значение текущего узла. Отсутствие `value` означает, что этот узел является промежуточным
  *
  * @param children Map со всеми детьми этого узла, где ключи - это их `id`, значения - сами дочерние узлы
  *
  * @tparam K       класс элементов, из которых состоит "путь" Seq[K]
  * @tparam V       класс объектов, хранящихся в узлах дерева
  */
case class Trie[K, +V](id: Option[K], value: Option[V],
                       children: Map[K, PrefixTree[K, V]] = Map.empty[K, PrefixTree[K, V]]
                       ) extends PrefixTree[K, V] {
  /** Помещает значение `newValue` по "пути" `path`, начиная с дочернего узла.
    *
    * @param path     последовательность идентификаторов типа K, обозначающая путь вставки значения.
    *                 Если передан пустой путь, значение помещается в текущий узел
    *
    * @param newValue значение, которое необходимо поместить. Если в конечном узле уже есть значение,
    *                 оно будет перезаписано
    *
    * @tparam U       тип помещаемого значения, может быть суперклассом для класса `V`
    * @return         новое дерево с учётом вставки нового значения
    */
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

  /** Возвращает под-дерево согласно заданному "пути" `path`. Последний идентификатор в "пути" будет
    * корнем возвращаемого дерева. Если такого пути не существует, будет возвращено пустое дерево.
    *
    * @param path "путь" до желаемого узла дерева
    */
  def sub(path: Seq[K]): PrefixTree[K, V] = {
    if (path.isEmpty)
      this
    else if (children.get(path.head).isDefined)
      children(path.head).sub(path.tail)
    else
      Trie.empty()
  }

  /** Возвращает значение текущего узла дерева.
    *
    * @throws java.util.NoSuchElementException если текущий узел является корневым или пустым.
    */
  def get: V = value.get

  /** Возвращает true если значение в текущем узле имеется, в противном случае false */
  def isExisting: Boolean = value.isDefined

  override def toString: String = getTrieString(2)

  /** Вспомогательный метод для "красивой" отрисовки дерева при вызове toString
    *
    * @param indent отступ слева, с которым нужно отрисовать текущий узел
    * @return       строку, визуализирующую дерево начиная с текущего узла
    */
  def getTrieString(indent: Int): String = {
    val startString = id match {
      case Some(idValue) => idValue + "(" + value.getOrElse("") + ")"
      case None          => "Root"
    }

    children.keys.foldLeft(startString)((acc, key) => {
      acc + "\n" + (" " * indent) + children(key).getTrieString(indent + 2)
    })
  }
}

/** Вспомогательный объект для быстрого создания пустых деревьев. */
object Trie {
  /** Возвращает пустое дерево с указанным идентификатором `id` или корень дерева
    *
    * @param id идентификатор пустого дерева. Если не указан, узел считается корневым
    * @tparam K класс элементов, из которых состоит "путь" Seq[K]
    * @tparam V класс объектов, хранящихся в узлах дерева
    */
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