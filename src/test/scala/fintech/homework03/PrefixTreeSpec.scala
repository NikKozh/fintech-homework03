package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  "Root node" should "produce IllegalArgumentException when put called with empty path" in {
    assertThrows[IllegalArgumentException] {
      RootTrieNode().put(Seq.empty, 1)
    }
  }

  "Root and empty nodes" should "produce NoSuchElementException when get called" in {
    assertThrows[NoSuchElementException] {
      RootTrieNode().get
      EmptyTrieNode('a').get
    }
  }

  behavior of "Put, sub and get methods"

  it should "work well with strings" in {
    val tree: PrefixTree[Char, Int] = RootTrieNode()

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: PrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get  should be (42)
    withDouble.sub("ab").sub("cde").get should be (13.0)
  }

  val numericTrie: PrefixTree[Int, String] = RootTrieNode().put(1 to 5, "five"    ).
                                                            put(1 to 2, "two"     ).
                                                            put(List(1, 2, 5), "two-five")
  it should "work well with numbers" in {
    numericTrie.sub(1 to 3).sub(4 to 5).get should be ("five")
    numericTrie.sub(List(1)).sub(List(2)).get             should be ("two")
    numericTrie.sub(List(1, 2, 5)).get                    should be ("two-five")
  }

  it should "work well with basic cases" in {
    val basicTree: PrefixTree[Char, Int] = RootTrieNode().put("a", 5)
    basicTree.sub("a").get should be (5)
  }

  it should "work well with replacing cases" in {
    val basicTree: PrefixTree[Char, Int] = RootTrieNode().put("a", 5).
                                                          put("a", 6)
    basicTree.sub("a").get should be (6)
  }

  it should "work well with simple branching" in {
    val simpleTrie: PrefixTree[Char, Int] = RootTrieNode().put("abc", 1).
                                                           put("abd", 2)

    val simpleSubTree: PrefixTree[Char, Int] = simpleTrie.sub("a")

    simpleSubTree.sub("bc").get should be (1)
    simpleSubTree.sub("bd").get should be (2)
  }

  it should "work well with complex branching" in {
    val complexTrie: PrefixTree[Char, Int] = RootTrieNode().put("abc",  4).
                                                            put("ad",   3).
                                                            put("abe",  2).
                                                            put("abcf", 1)
    complexTrie.sub("abc" ).get should be (4)
    complexTrie.sub("ad"  ).get should be (3)
    complexTrie.sub("abe" ).get should be (2)
    complexTrie.sub("abcf").get should be (1)
  }

  val complexTypeTrie: PrefixTree[Char, AnyVal] = RootTrieNode().put("abc",  1   ).
                                                                 put("ad",   '2' ).
                                                                 put("abe",  3.0 ).
                                                                 put("abcf", true)
  it should "work well with complex branching and different types" in {
    complexTypeTrie.sub("abc" ).get should be (1)
    complexTypeTrie.sub("ad"  ).get should be ('2')
    complexTypeTrie.sub("abe" ).get should be (3.0)
    complexTypeTrie.sub("abcf").get shouldBe true
  }

  behavior of "PrefixTree"

  it should "be created correctly through constructors" in {
    val tree: PrefixTree[Char, Char] = RootTrieNode(Map(
      'a' -> EmptyTrieNode('a'),
      'b' -> EmptyTrieNode('b', Map(
        'c' -> TrieNode(1, 'c'),
        'd' -> TrieNode(2, 'd')
      ))
    ))

    tree.sub("bc").get should be (1)
    tree.sub("bd").get should be (2)
  }

  it should "be printed correctly with only root node" in {
    val testData = "Root"
    RootTrieNode().toString should be (testData)
  }

  it should "be printed correctly with only empty node" in {
    val testData = "a()"
    EmptyTrieNode('a').toString should be (testData)
  }

  it should "be printed correctly with only node with value" in {
    val testData = "a(1)"
    TrieNode(1, 'a').toString should be (testData)
  }

  it should "be printed correctly with complex branching" in {
    val testData =
      """|Root
         |  a()
         |    b()
         |      c(1)
         |        f(true)
         |      e(3.0)
         |    d(2)""".stripMargin

    complexTypeTrie.toString should be (testData)
  }

  it should "be printed correctly with numeric trie" in {
    val testData =
      """|Root
         |  1()
         |    2(two)
         |      3()
         |        4()
         |          5(five)
         |      5(two-five)""".stripMargin

    numericTrie.toString should be (testData)
  }
}