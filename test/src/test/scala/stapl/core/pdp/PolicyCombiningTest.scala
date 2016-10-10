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

object PolicyCombiningTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }

}
/**
 * Some tests about the evaluation of policies.
 */
class PolicyCombiningTest extends AssertionsForJUnit {
  
  import stapl.core.dsl._
  import org.junit.Assert._
  
  @Before def setup() {
    // nothing to do
  }

  @Test def testNotApplicableBecauseOfTarget {
    val policy = 
      Policy("test") := when(false) apply PermitOverrides to (
        Rule("rule1") := deny iff true,
        Rule("rule2") := permit iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(NotApplicable), result)
  }
  
  @Test def testNotApplicableBecauseOfRule {
    val policy = 
      Policy("test") := always apply PermitOverrides to (
        Rule("rule1") := deny iff false,
        Rule("rule2") := permit iff false
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(NotApplicable), result)
  }
  
  @Test def testPermitOverrides1 {
    val policy = 
      Policy("test") := always apply PermitOverrides to (
        Rule("rule1") := deny iff true,
        Rule("rule2") := permit iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
  
  @Test def testPermitOverrides2 {
    val policy = 
      Policy("test") := always apply PermitOverrides to (
        Rule("rule1") := deny iff true,
        Rule("rule2") := permit iff false
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Deny), result)
  }
  
  @Test def testPermitOverrides3 {
    val policy = 
      Policy("test") := always apply PermitOverrides to (
        Rule("rule1") := permit iff true,
        Rule("rule2") := deny iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
  
  @Test def testDenyOverrides1 {
    val policy = 
      Policy("test") := always apply DenyOverrides to (
        Rule("rule1") := permit iff true,
        Rule("rule2") := deny iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Deny), result)
  }
  
  @Test def testDenyOverrides2 {
    val policy = 
      Policy("test") := always apply DenyOverrides to (
        Rule("rule1") := permit iff true,
        Rule("rule2") := deny iff false
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
  
  @Test def testDenyOverrides3 {
    val policy = 
      Policy("test") := always apply DenyOverrides to (
        Rule("rule1") := deny iff true,
        Rule("rule2") := permit iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Deny), result)
  }
  
  @Test def testFirstApplicable1 {
    val policy = 
      Policy("test") := always apply FirstApplicable to (
        Rule("rule1") := permit iff true,
        Rule("rule2") := deny iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
  
  @Test def testFirstApplicable2 {
    val policy = 
      Policy("test") := always apply FirstApplicable to (
        Rule("rule1") := permit iff true,
        Rule("rule2") := deny iff false
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
  
  @Test def testFirstApplicable3 {
    val policy = 
      Policy("test") := always apply FirstApplicable to (
        Rule("rule1") := deny iff false,
        Rule("rule2") := permit iff true
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
  
  @Test def testNestedPolicy {
    val policy = 
      Policy("test") := always apply PermitOverrides to (
        Policy("test1") := always apply DenyOverrides to (
          Rule("rule1") := permit iff true,
          Rule("rule2") := deny iff true
        ),
        Policy("test2") := always apply FirstApplicable to (
          Rule("rule1") := deny iff false,
          Rule("rule2") := permit iff true
        )
      )
    val pdp = new PDP(policy)
    val result = pdp.evaluate("", "", "")
    
    assertEquals(Result(Permit), result)
  }
}