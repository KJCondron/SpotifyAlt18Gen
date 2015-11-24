package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import UAlt18Parser._

class UALT18Handler(val conn : HTTPWrapper) extends DefaultHandler {
  
  var inBody = false
  var inLink = false
  
  val res = ListBuffer[Option[String]]()
  
  override def startElement( uri : String, localName : String,
                    name : String, a : Attributes) : Unit = {               
    
    inBody = inBody || name == "body"
       
     val pattern ="""Results (\d+)/(\d+)/(\d+)""".r 
      
      if(inBody && name == "a") {
        res += tryGet(a, "title").flatMap {
      case s : String => tryIf(s.toUpperCase.contains("RESULTS")).flatMap( _ =>
        pattern.findFirstIn(s).flatMap( _ => 
          tryGet(a,"href").flatMap( href => 
           tryIf(href.toUpperCase().contains("RESULTS")).flatMap(_ => Some(href))   
          )
      ))
      case _ => None
    }
      
      val el = List[Option[String]]()
      
      val nextPageList = tryGet(a, "class").map( {
          case s if (s == "next page-numbers") => getUALT18(tryGet(a,"href").get, conn)
          case _ => el } ).getOrElse(el)
          
      res ++= nextPageList
    } 
  }     
}