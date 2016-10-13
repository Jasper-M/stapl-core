package stapl.core.pdp

import scala.reflect.runtime.universe._
import stapl.core.Attribute
import stapl.core.SUBJECT
import stapl.core.RESOURCE
import stapl.core.ACTION

private[pdp] object Attributes {
  val subjectId = Attribute[String](SUBJECT, "id")(typeOf[String])
  val resourceId = Attribute[String](RESOURCE, "id")(typeOf[String])
  val actionId = Attribute[String](ACTION, "id")(typeOf[String]) 
}