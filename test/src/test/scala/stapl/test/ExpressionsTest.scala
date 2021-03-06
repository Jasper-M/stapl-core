package stapl.test

import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit
import stapl.core.pdp.RemoteEvaluator
import stapl.core.Attribute
import stapl.core.CombinationAlgorithm
import stapl.core.pdp.EvaluationCtx
import stapl.core.Value
import stapl.core.dsl.Subject
import stapl.core.dsl.convertToValue
import stapl.core.dsl.fromValue
import stapl.core.pdp.RequestCtx

object ExpressionsTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
/**
 * Some tests about the handling of remote policies.
 * There are no real remote policies involved in these tests.
 */
class ExpressionsTest extends AssertionsForJUnit {

  case class DummyCtx(f: String => Any) extends EvaluationCtx {

    def request: RequestCtx = ???
    def evaluationId: String = ???
    def subjectId: String = ???
    def resourceId: String = ???
    def actionId: String = ???
    def remoteEvaluator: RemoteEvaluator = ???
    def cachedAttributes: Map[Attribute[_], Any] = ???
    def employedAttributes: Map[Attribute[_], Any] = ???
    
    def findAttribute[T](attribute: Attribute[T]): T = f(attribute.name).asInstanceOf[T]
  }
  
  @Before def setup() {
    // nothing to do
  }

  @Test def testExpressions() {
    import stapl.core.dsl._
    def expression[T](value: Value[T]) = value
    
    object sub extends Subject {
      val listString = Attribute[List[String]]
      val string = Attribute[String]
      val string2 = Attribute[String]
      val bool = Attribute[Boolean]
      val integer = Attribute[Int]
      val some = Attribute[Some[Int]]
    }
    
    def areEqual[T](a: T, b: T) = a == b
    
    assertEquals(
        1,
        expression{ sub.string indexOf sub.integer }.getConcreteValue(DummyCtx{case "string" => "abc"; case "integer" => 98})
    )
    /* unexpected tree in genload
    
    assertEquals(
        true,
        expression{ areEqual(sub.string, sub.string2) }.getConcreteValue(DummyCtx{case "string" => "abc"; case "string2" => "abc"})
    )*/
    assertEquals(
        1,
        expression{ "abc" indexOf sub.integer }.getConcreteValue(DummyCtx{case "integer" => 98})
    )
    assertEquals(
        true,
        expression{ sub.string == "abc" }.getConcreteValue(DummyCtx{case "string" => "abc"})
    )
    assertEquals(
        true,
        expression{ "abc" == sub.string }.getConcreteValue(DummyCtx{case "string" => "abc"})
    )
    assertEquals(
        true,
        expression{ (sub.string contains sub.string2) && sub.bool }.getConcreteValue(DummyCtx{case "string" => "abc"; case "string2" => "a"; case "bool" => true})
    )
    assertEquals(
        "abc",
        expression{ sub.string }.getConcreteValue(DummyCtx{case "string" => "abc"})
    )
    assertEquals(
        "abc",
        expression{ "abc" }.getConcreteValue(DummyCtx{case "string" => "abc"})
    )
    assertEquals(
        true,
        expression{ (sub.some.toString + "s") == "Some(3)s" }.getConcreteValue(DummyCtx{case "some" => Some(3)})
    )
    assertEquals(
        List(1,3,4,5) map (_ + 2),
        expression{ List(1,3,4,5) map (_ + sub.integer) }.getConcreteValue(DummyCtx{case "integer" => 2})
    )
  }

}