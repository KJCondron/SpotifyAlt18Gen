package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

object UAlt18Parser {
  
  def tryGet( a : Attributes, name : String) : Option[String] =
    if(a.getIndex(name) == -1 )
      None
      else
        Some(a.getValue(name))
        
  def tryIf(x : => Boolean) = if(x) Some() else None
        
  def getUALT18Cal( address : String, conn : HTTPWrapper  ) : List[String] = 
    getUALT18CalO(address, conn).flatten
  
  
  def getUALT18CalO( address : String, conn : HTTPWrapper ) : List[Option[String]] = {
    
    val HTML = conn.requestLatest(address)
  		
  	val h1 = new UALT18CalHandler(conn)
  	val parser = new SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList
	}
  
  def getUALT18( address : String, conn : HTTPWrapper  ) : List[Option[String]] = {
	  	
    val HTML = conn.request(address)
	
  	val h1 = new UALT18Handler(conn)
  	val parser = new SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList.distinct
  }
  
  def getUALT18Res( address : String, conn : HTTPWrapper  ) : List[String] = {
    
    val HTML = conn.request(address)
    	
  	val h1 = new UALT18ResHandler
  	val parser = new SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList
}
  //2
  def getUALT18Table( address : String, conn : HTTPWrapper, ignoreCache : Boolean = false  ) : List[String] = {
    
    val HTML = if (ignoreCache) conn.requestLatest(address) else conn.request(address)
    	
  	val h1 = new UALT18TableHandler
  	val parser = new SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList
}

def getUAlt18(conn : HTTPWrapper, conn2 : HTTPWrapper) = {
  val address = """http://theunofficialalt18countdownplaylists.com/"""
  
  val res = getUALT18Cal(address,conn)
  println("got getUALT18Cal 1")
  
  val alt18add = res.flatMap( resAdd => getUALT18(resAdd,conn).flatten )
  println("got getUALT18Cal 2")
  
  // results is each weeks alt-18 address (for which we can get the date)
  // and the "artist-title" list
  val alt18s = alt18add.collect( {
     case x  : String if(x.contains("results-")) => (x,getUALT18Table(x,conn2)) 
   } )
  alt18s
}  
}