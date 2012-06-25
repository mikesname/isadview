package controllers

import play.api._
import play.api.mvc._

trait ControllerHelpers {
  def isAjaxRequest[T](request: Request[T]): Boolean = {
      request.headers.get("X-REQUESTED-WITH").getOrElse("").toUpperCase() == "XMLHTTPREQUEST"
  }
  
  /*
   * Play Forms don't currently support multi-value select widgets. We 
   * need to transform the input from:
   *  key -> Seq(va1, val2, val3) to:
   *  key[0] -> Seq(val1), key[1] -> Seq(val2), key[2] -> Seq(val3)
   */
  def transformMultiSelects(formData: Option[Map[String,Seq[String]]], multies: List[String]) = {
    formData.map(b => {
      b.flatMap { (t: (String,Seq[String])) =>
        t match {
          case (n, s) if multies.contains(n) => {
            s.zipWithIndex.map(t => n + "[" + t._2 + "]" -> List(t._1))
          }
          case other => List(other)
        }
      }
    }).getOrElse(Map[String,Seq[String]]())
  }

}
