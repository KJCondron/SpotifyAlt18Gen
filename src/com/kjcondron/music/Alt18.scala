package com.kjcondron.music

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

import com.kjcondron.web.HTTPWrapper

object Alt18 extends App {
  
  val conn = new HTTPWrapper("""C:\Users\KJCon\Documents\ALT18\""")
  
  def getALT18( address : String ) : List[String] = {
    
    val HTML = conn.request(address)
	
	val h1 = new ALT18Handler
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	parser.parse(HTML, h1)
	
	h1.res.toList.map( str => str.replace("(debut)", "").replace("“","").replace("”","") )
  }
  
    def getALT181W( address : String, includeDiv : Boolean = false ) : Map[Int,String] = {
    
		  val HTML = conn.request(address)
		  
		  val h1 = new ALT181WHandler(includeDiv)
		  val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
		  parser.parse(HTML, h1)
		
		  if (!includeDiv && h1.res.size == 0)
			  getALT181W(address,true)
		  else
	      {
		    println(h1.res)
			h1.res
		  }
  	}
  
  def splitStringOnce( sep : Char )( str : String ) =
  {
    str.split(sep) match {
      case Array(t1,t2) => (t1.trim,t2.trim)
      case _ => ( str.takeWhile(_!=sep).trim, str.dropWhile(_!=sep).drop(1).trim ) 
    }
  }
  
  def stringGrouper( str : String ) : String = 
    str.toUpperCase().filter(c => c.toInt > 64 && c.toInt < 91).replace("THE","").replace("AND","")
          
  val address = """http://blog.siriusxm.com/tag/alt-18/"""
  val allPages = List(address) ++ (2 to 6).map(address + """page/"""+ _ + """/""")
  val res = allPages.flatMap(getALT18)
  val AAndS = res.map(splitStringOnce('–'))
  val allItems = AAndS.distinct
  
  val groupedItems = allItems.groupBy{ case (name,_) => stringGrouper(name) }
  val filterdItems = groupedItems.map( x =>{ 
    val bandName = x._2.head._1
    val songs = x._2.map(_._2)
    val grpSngs = songs.groupBy(stringGrouper)
    val distinctSongs = grpSngs.map(_._2.head)
    (bandName, distinctSongs)
  } )
  
  allItems.foreach(println)
  
  val bands = allItems.map(_._1).distinct // throw out perfect matches first?
  val groupdBands = bands.groupBy( name => name.toUpperCase().filter(c => c.toInt > 64 && c.toInt < 91) )
  val allBands = groupdBands.map(_._2.head)
  
  allBands.foreach(println)
   
  filterdItems.foreach( e => {
    print(e._1 + ":")
    println(e._2.reduceLeft( (acc,e) => acc + " :: " + e ))
  })
  
  conn.dispose  
}

  
  class ALT18Handler extends DefaultHandler {
    
    var inBody = false
    var inLink = false
    var lastAttr : Attributes = null
    
    val res = ListBuffer[String]()
    
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes) : Unit = {               
      inBody = inBody || name == "body"
      inLink = inLink || (inBody && name == "a")
      if(inLink) { 
        lastAttr = a
      }
      
      }
                      
    override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
                      if(inLink) {
                        val str = new String(ch, start, length)
                        if("Continued" == str) {
                        	println( lastAttr.getValue("href") )
                        	res ++= Alt18.getALT181W(lastAttr.getValue("href")).values
                        }
                      }
    
    override def endElement( uri : String, localName : String, name : String ) = 
      if(inLink) {
        inLink = false
        lastAttr = null
      }
    
  }
  
  class ALT181WHandler(includeDiv : Boolean = false) extends DefaultHandler {
    
    var inBody = false
    var inP = false
    var inEntry = false
    var current = ""
    
    val numbers = (1 to 18).map( _ + ".")
    
    val res = Map[Int,String]()
    
    def checkP(name : String) =
      if(includeDiv)
        name == "div" || name == "p"
      else
        name == "p"
    
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes) : Unit = {               
      inBody = inBody || name == "body"
      inP = inP || (inBody && checkP(name))
    }
                      
    override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
                      if(inP) {
                        val str = new String(ch, start, length).replace("\n", "")
                        if( inEntry || numbers.exists( p=>str.take(p.length) == p) ) {
                            inEntry = true
                        	  current += str
                        }
                        	
                      }
    
    override def endElement( uri : String, localName : String, name : String ) =
      if(inP) {
        if(inEntry) { 
          val pos = current.takeWhile(_!='.').toInt
          val name = current.dropWhile(_!='.').drop(1).trim
          res += (pos->name) }
        inEntry = false
        current = ""
        
        if("p" == name) {
          inP = false
        }
      }    
  }

