package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import UAlt18Parser._

class UALT18AnnualHandler(
    conn : HTTPWrapper,
    val res : ListBuffer[Option[String]] = ListBuffer()) extends DefaultHandler {
  
  var inBody = false
  var inTable = false
  var inEOYTable = false
  var inWeeklyTable = false
  
  val eoyRes = ListBuffer[String]() 
  val weeklyRes = ListBuffer[String]() 
  
  override def startElement( uri : String, localName : String,
                    name : String, a : Attributes) : Unit = {               
    
    inBody = inBody || name == "body" 
    inTable = inTable || (inBody && name == "table")     
  }
                    
  override def endElement( uri : String, localName : String, name : String ) = { 
  }
  
  override def characters( ch : Array[Char], start : Int, length : Int) : Unit = {
    if (inTable) {
      val str = new String(ch, start, length)
      inEOYTable = inEOYTable || str.contains("Top 18 of")
      inWeeklyTable = inWeeklyTable || str.contains("Week ending")
    }
    
    inEOYTable = !inWeeklyTable // if we are in the weekly table we are done
  
  }
  
}