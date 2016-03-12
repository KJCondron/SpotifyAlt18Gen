package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import UAlt18Parser._
import java.util.Calendar

object UAlt18AnnualParser {
  def getUAlt18Annual( address : String ) : List[String] =
    io.Source.fromFile(address, "utf-8").getLines.toList
    
  def getUAlt18Annual( conn : HTTPWrapper ) : List[(String, List[String])] =
  {
    val dts = List(2011,2012)
    val loc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\%d.txt"""
    val first = dts.map (dt => (dt.toString,getUAlt18Annual(loc.format(dt))) )
    
    val pre = "http://theunofficialalt18countdownplaylists.com/previous-results/"
  
    val stYear = 2013
    val endYear = Calendar.getInstance().get(Calendar.YEAR)
    
    val years = (stYear to endYear)
    
    val more = years.dropRight(1).map( x => {
      val add = pre + x.toString + "-2/"
      val trs = getUALT18Table(add, conn)
      (x.toString, trs)
    }).toList
    
    val fin = pre + endYear.toString + "-2/"
    (endYear.toString, getUALT18Table(fin, conn, true)) :: more ::: first
    
  }
}