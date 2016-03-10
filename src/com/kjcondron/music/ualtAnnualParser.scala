package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

import UAlt18Parser._

object UAlt18AnnualParser {
  def getUAlt18Annual( address : String ) : List[String] =
    io.Source.fromFile(address, "utf-8").getLines.toList
    
  def getUAlt18Annual(conn : HTTPWrapper ) : List[(String, List[String])] =
  {
    val dts = List(2011,2012)
    val loc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\%d.txt"""
    val first = dts.map (dt => (dt.toString,getUAlt18Annual(loc.format(dt))) )
    
    val pre = "http://theunofficialalt18countdownplaylists.com/previous-results/"
  
    val second = (2013 to 2016).toList.map( x => {
      val add = pre + x.toString + "-2/"
      val trs = getUALT18Table(add, conn)
      (x.toString, trs)
    })
  
    first ++ second
  }
}