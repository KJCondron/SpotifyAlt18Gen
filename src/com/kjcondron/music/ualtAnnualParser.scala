package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

object UAlt18AnnualParser {
  def getUAlt18Annual( address : String ) : List[String] =
    io.Source.fromFile(address, "utf-8").getLines.toList
    
  val getUAlt18Annual : List[(String, List[String])] =
  {
    val dts = List(2011,2012)
    val loc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\%d.txt"""
    dts.map { dt => 
      (dt.toString,getUAlt18Annual(loc.format(dt)))
    }
  }
}