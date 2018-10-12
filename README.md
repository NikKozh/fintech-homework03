# Реализация префиксного дерева
Представлена одним case-классом.

Каждый узел дерева может хранить:
* значение типа Option[V];
* свои дочерние узлы в виде Map[K, PrefixTree[K, V]], где K - идентификатор дочернего узла, PrefixTree[K, V] - сам дочерний узел.

Узел дерева считается пустым, если у него есть идентификатор, но нет хранимого значения.

Поместить значение в дерево можно вызвав метод put.
Извлечь под-дерево можно вызвав метод sub.
Получить значение любого узла можно вызвав метод get. Вызов этого метода на пустом узле выбросит икслючение NoSuchElementException.

Помимо исходных требований, также был реализован "красивый" вывод дерева по вызову toString и дополнительный метод isExisting, чтобы можно было перед запросом значения проверить, существует ли оно в данном узле.

Подробнее см. [Scaladoc](https://github.com/NikKozh/fintech-homework03/blob/master/src/main/scala/fintech/homework03/PrefixTrie.scala) и [Spec](https://github.com/NikKozh/fintech-homework03/blob/master/src/test/scala/fintech/homework03/PrefixTreeSpec.scala).
