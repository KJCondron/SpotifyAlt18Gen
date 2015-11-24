package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import UAlt18Parser._

class UALT18TableHandler extends DefaultHandler {
  
  var inBody = false
  var inTable = false
  var inTHead = false
  var inTBody = false
  var inTRow = false
  var inTrackCol = false
  var inTHCol = false
  var found1 = false
  var maxCol = 0
  var trackCol = ""
  var chars = ""
  
  val res = ListBuffer[(String)]()
  
  override def startElement( uri : String, localName : String,
                    name : String, a : Attributes) : Unit = {               
    
    inBody = inBody || name == "body"
    inTable = inTable || (inBody && name == "table")
    inTHead = inTHead || (inTable && name == "thead")
    if(inTHead && name == "th")
    {
      maxCol += 1
      inTHCol = true
    }
    
    inTBody = inTBody || (inTable && name == "tbody")
    inTRow = inTRow || (inTBody && name == "tr")
    inTrackCol = inTrackCol || (inTRow && name == "td" && tryGet(a,"class").getOrElse(false)==trackCol)
    found1 = found1 || inTrackCol
    
  }
  
  override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
  {
    if(inTrackCol)
      chars += new String(ch, start, length)
    if(inTHCol)
    {
      val chr = new String(ch, start, length)
      if(chr.contains("ARTIST")){
        trackCol = "column-" + maxCol
      }
        
    }
  }
    
  override def endElement( uri : String, localName : String, name : String ) = {
    if(inTHead && name == "thead")
      inTHead = false
      
    if(inTHCol && name == "th")
      inTHCol = false
      
    if(found1 && name == "table") {
      // just set all vars to false to stop parsing
      inBody = false
      inTable = false
      inTBody = false
      inTRow = false
      inTrackCol = false
    }
    
    if(inTrackCol && name == "td")
      inTrackCol = false
      
    if(chars != "") {
      res += chars
      chars = ""
    }
  }
}