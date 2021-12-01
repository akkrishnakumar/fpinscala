package chapter10

sealed trait WC

case class Part(lStub: String, count: Int, rStub: String) extends WC
case class Stub(chars: String)                            extends WC

object WC {

  def count(wc: WC) =
    wc match {
      case Part(lStub, count, rStub) => count + 2
      case Stub(chars)               => 0
    }

}
