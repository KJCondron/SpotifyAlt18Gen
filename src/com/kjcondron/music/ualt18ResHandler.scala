package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

class UALT18ResHandler extends DefaultHandler {
  var inBody = false
  var foundArtist = false
  var tdCount = 0
  
  val res = ListBuffer[String]()
   
  override def startElement( uri : String, localName : String,
                    name : String, a : Attributes) : Unit = {               
    inBody = inBody || name == "body"
      
    tdCount += (if (inBody && foundArtist && name == "td") 1 else 0)
    
  }
                    
  override def characters( ch : Array[Char], start : Int, length : Int) : Unit = {
    if(inBody) {
      foundArtist = foundArtist || ch.contains("ARTIST")
     
      if(tdCount == 5)
      {
        val str = new String(ch, start, length)
        res += str
        tdCount = 0
      }
    }
  }
}