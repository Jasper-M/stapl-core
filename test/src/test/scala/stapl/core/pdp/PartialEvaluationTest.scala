package stapl.core.pdp

import org.junit.BeforeClass
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.Test
import stapl.core.AbstractPolicy
import stapl.core.Result
import stapl.core.NotApplicable
import stapl.core.Permit
import stapl.core.Deny
import stapl.core.Attribute

object PartialEvaluationTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }

}
/**
 * Some tests about the evaluation of policies.
 */
class PartialEvaluationTest extends AssertionsForJUnit {
  
  import stapl.core.dsl._
  import org.junit.Assert._
  
  object subject extends Subject {
    val a = Attribute[String]
    val b = Attribute[String]
  }
  
  @Before def setup() {
    // nothing to do
  }

  @Test def test1 {
    val policy = 
      Policy("test") := always apply PermitOverrides to (
        Rule("rule1") := deny iff (subject.a == "foo"),
        Rule("rule2") := permit iff (subject.b == "bar")
      )
    val pdp = new PartialEvaluationPDP(policy)
    val Left(id) = pdp.evaluate(Map[Attribute[_],Any](subject.a -> "foo"))
    val result = pdp.evaluate(id, Map[Attribute[_],Any](subject.b -> "bar"))
    assertEquals(Right(Result(Permit)), result)
  }
  
  @Test def test2 {
    val policy = 
      Policy("test") := always apply DenyOverrides to (
        Rule("rule1") := deny iff (subject.a == "foo"),
        Rule("rule2") := permit iff (subject.b == "bar")
      )
    val pdp = new PartialEvaluationPDP(policy)
    val result = pdp.evaluate(Map[Attribute[_],Any](subject.a -> "foo"))
    assertEquals(Right(Result(Deny)), result)
  }
  
  @Test def test3 {
    val policy = 
      Policy("test") := always apply FirstApplicable to (
        Rule("rule1") := deny iff (subject.a == "baz"),
        Rule("rule2") := permit iff (subject.b == "bar")
      )
    val pdp = new PartialEvaluationPDP(policy)
    val Left(id) = pdp.evaluate(Map[Attribute[_],Any](subject.a -> "foo"))
    val result = pdp.evaluate(id, Map[Attribute[_],Any](subject.b -> "bar"))
    assertEquals(Right(Result(Permit)), result)
  }
 
}