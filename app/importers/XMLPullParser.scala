package importers

import scala.io.Source
import scala.xml.XML._
import scala.xml.pull._
import scala.xml._

package object XMLPullParser {
  // Pinched from:
  // http://stackoverflow.com/questions/8525675/how-to-get-a-streaming-iteratornode-from-a-large-xml-document
  def processSource[T](nodeLabel: String, input: Source)(f: scala.xml.NodeSeq => T) {
    new scala.xml.parsing.ConstructingParser(input, false) {
      var depth = 0 // track depth
      nextch // initialize per documentation
      document // trigger parsing by requesting document

      override def elemStart(pos: Int, pre: String, label: String,
          attrs: MetaData, scope: NamespaceBinding) {
        super.elemStart(pos, pre, label, attrs, scope)
        depth += 1
      }
      override def elemEnd(pos: Int, pre: String, label: String) {
        depth -= 1
        super.elemEnd(pos, pre, label)
      }
      override def elem(pos: Int, pre: String, label: String, attrs: MetaData,
          pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq = {
        val node = super.elem(pos, pre, label, attrs, pscope, nodes)
        label match {
          case s: String if depth == 1 => <dummy/>
          case s: String if s == nodeLabel => f(node); NodeSeq.Empty
          case _ => node // roll up other nodes
        }
      }
    }
  }
}

