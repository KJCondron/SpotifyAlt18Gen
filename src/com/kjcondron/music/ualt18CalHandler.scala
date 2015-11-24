package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import UAlt18Parser._

class UALT18CalHandler(
    conn : HTTPWrapper,
    val res : ListBuffer[Option[String]] = ListBuffer()) extends DefaultHandler {
  
  var inBody = false
  var inCalendar = false
  var calDepthCount = 0
  var inLink = false
  var foundPrev = true
  
  override def startElement( uri : String, localName : String,
                    name : String, a : Attributes) : Unit = {               
    
    inBody = inBody || name == "body" 
    calDepthCount += (if(inCalendar) 1 else 0)
    inCalendar = inCalendar || tryGet(a, "class").map( s => s.contains("calendar") ).getOrElse(false)
    
    if(inCalendar) {
      inLink = inLink || (inBody && name == "a")
      if(inLink)
        res += tryGet(a, "title").flatMap {
        case s : String if s.toUpperCase.contains("RESULTS") => Some(a.getValue("href"))
        case _ => None
      }  
      if(foundPrev) {
        foundPrev = false
        if(inLink)
          res ++= getUALT18CalO( tryGet(a,"href").get, conn)
      }   
      foundPrev = foundPrev || tryGet(a, "id").map( _ == "prev" ).getOrElse(false)
    }   
  }
                    
  override def endElement( uri : String, localName : String, name : String ) = { 
    if(inLink) {
      inLink = false
    }
  
  if(inCalendar) {
    calDepthCount -=1
        if(calDepthCount == 0) { 
          inCalendar = false
        }
  }
 }
}