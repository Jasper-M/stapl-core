package stapl.core

trait MetaProperty

object MetaProperty {
  case object DontCache extends MetaProperty
  case class CacheFor(milliseconds: Long) extends MetaProperty
  case class JSON(json: String) extends MetaProperty
}