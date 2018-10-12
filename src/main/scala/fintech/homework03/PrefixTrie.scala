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

  def getTrieString(indent: Int, id: Option[K]): String
}

/** Реализация интерфейса [[fintech.homework03.PrefixTree]]
  *
  * @param value    значение текущего узла. Отсутствие `value` означает, что этот узел является промежуточным
  *
  * @param children Map со всеми детьми этого узла, где ключи - это их идентификаторы, значения - сами дочерние узлы
  *
  * @tparam K       класс элементов, из которых состоит "путь" Seq[K]
  * @tparam V       класс объектов, хранящихся в узлах дерева
  */
case class Trie[K, +V](value: Option[V] = None,
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
    path match {
      case Seq()              => copy(value = Some(newValue))
      case Seq(head, tail@_*) => children.get(head) match {
        case Some(childTrie) =>
          copy(children = children.updated(head, childTrie.put(tail, newValue)))
        case None =>
          if (tail.isEmpty)
            copy(children = children.updated(head, Trie(Some(newValue))))
          else
            copy(children = children.updated(head, Trie().put[U](tail, newValue)))
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
      Trie()
  }

  /** Возвращает значение текущего узла дерева.
    *
    * @throws java.util.NoSuchElementException если текущий узел является корневым или пустым.
    */
  def get: V = value.get

  /** Возвращает true если значение в текущем узле имеется, в противном случае false */
  def isExisting: Boolean = value.isDefined

  override def toString: String = getTrieString(2, None)

  /** Вспомогательный метод для "красивой" отрисовки дерева при вызове toString
    *
    * @param indent отступ слева, с которым нужно отрисовать текущий узел
    * @return       строку, визуализирующую дерево начиная с текущего узла
    */
  def getTrieString(indent: Int, id: Option[K]): String = {
    val startString = id match {
      case Some(idValue) => idValue + "(" + value.getOrElse("") + ")"
      case None          => "Root"
    }

    children.keys.foldLeft(startString)((acc, key) => {
      acc + "\n" + (" " * indent) + children(key).getTrieString(indent + 2, Some(key))
    })
  }
}

object Main extends App {
  val complexTrie = Trie().put("abc",  1   ).
                           put("ad",   '2' ).
                           put("abe",  3.0 ).
                           put("abcf", true)
  println(complexTrie)
  println(complexTrie.sub("ab"))
  println(complexTrie.sub("ad").get)
  println(complexTrie.sub("abe").get)
  println(complexTrie.sub("abcf").get)
  // println(complexTrie.sub("abf").get) // проверка на вылет Exception
}