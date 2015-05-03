package com.kjcondron.music

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import com.wrapper.spotify._
import scala.collection.JavaConversions._
import UAlt18F._
import com.wrapper.spotify.models.Artist
    
 
object UAlt18F {
  
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
  	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList
	}
  
  def getUALT18( address : String, conn : HTTPWrapper  ) : List[Option[String]] = {
	  	
    val HTML = conn.request(address)
	
  	val h1 = new UALT18Handler(conn)
  	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList.distinct
  }
  
  def getUALT18Res( address : String, conn : HTTPWrapper  ) : List[String] = {
    
    val HTML = conn.request(address)
    	
  	val h1 = new UALT18ResHandler
  	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList
}
  //2
  def getUALT18Table( address : String, conn : HTTPWrapper  ) : List[String] = {
    
    val HTML = conn.request(address)
    	
  	val h1 = new UALT18TableHandler
  	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
  	parser.parse(HTML, h1)
  	
  	h1.res.toList
}
  
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
    
    //override def endElement( uri : String, localName : String, name : String ) =  
  }

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
  
  class UALT18TableHandler extends DefaultHandler {
    
    var inBody = false
    var inTable = false
    var inTHead = false
    var inTBody = false
    var inTRow = false
    var inTrackCol = false
    var found1 = false
    var maxCol = 0
    var chars = ""
    
    val res = ListBuffer[(String)]()
    
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes) : Unit = {               
      
      inBody = inBody || name == "body"
      inTable = inTable || (inBody && name == "table")
      inTHead = inTHead || (inTable && name == "thead")
      if(inTHead && name == "th")
        maxCol += 1
      
      val trackCol = "column-"+maxCol  
      inTBody = inTBody || (inTable && name == "tbody")
      inTRow = inTRow || (inTBody && name == "tr")
      inTrackCol = inTrackCol || (inTRow && name == "td" && tryGet(a,"class").getOrElse(false)==trackCol)
      found1 = found1 || inTrackCol
      
    }
    
    override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
      if(inTrackCol)
        chars += new String(ch, start, length)
      
    
    override def endElement( uri : String, localName : String, name : String ) = {
      if(inTHead && name == "thead")
        inTHead = false
        
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
    
  def getSpotify(conn : HTTPWrapper) = {
    
   
    val clientId = "20212105b1764ecb81a226fca7796b4b"
    val secret = "57a3b615f9be4b17b61361da94b35204"
    val redir = "https://www.google.com/"
    
    val api = Api.builder.clientId(clientId)
              .clientSecret(secret)
              .redirectURI(redir)
              .build
              
    val request = api.clientCredentialsGrant().build();
    val response = request.get
    println(response.getAccessToken)
    
    val scopes = List(
        "user-read-private",
        "user-read-email",
        "playlist-read-private",
        "playlist-modify-public",
        "playlist-modify-private")
        
    val state = "mystate"
 //   val authorizeURL = api.createAuthorizeURL(scopes, state);
 //   println(authorizeURL)
    

    
//    val code = "AQDmajMzNOJwZfAUmsY3j2I_bHs0by73pnBhMt5kKek-oIicOZHpGwt9i6WJD0T-CV-bD8GbgZPLRfPbxDY0l_LbpT_MJvZREdDuZ99auQ-F3U2Zv071MFPBEzVd_AhIDReugdKhDqQ11slXO335aG-otNzYmz34CCvt2b08OKSVDjZzmfb9LuOqbxgc0KJ969UC6pnPZ1YbackSrkFoHf9HZS1ydNpymwYhHq1dIajmuYEtD7EMiesE0D12A_kt0t5398KNwwMkSrWwORZ5yAe9fQcuOnqgQAcZO_mCM9CqwRh2iqvIYZLsd_p-asDFPuKFe33nv_JS"
//    val credentialRequest = api.authorizationCodeGrant(code).build.get()
// 
//    val tok = credentialRequest.getAccessToken
//    val ref = credentialRequest.getRefreshToken
//    val expin = credentialRequest.getExpiresIn
//    println(tok)
//    println(ref)
//    println(expin)
//    //val token = getAccessToken
    
 //   val _tok = "BQB6-JvveLXYXo0FVK8tOsdq66XM-pfy_nErHVcpR9NBchKf7daAHtwnP3OrF42uoSWfIPEElSwBjm7d0D5DxoOkf00DV3iBkQMSC275JbXNIwsF5sXi9NWE49CHXrowjaEwWtB2tpNHtvDrV1x7LvcqCml7kl9QP3pfIopdeGEj1XQ4k9ChXZV5TZNSav-JApE80Wn1_aXlf2Pfrm5vhmbolE5h2_dsovGnD51iIfGEsZjA8BJ9RJKsIezU888jMp3Dcw"
    val _ref = "AQCrukKfpQDzHxXKgxKHOf7tw_vt2bS4A8sXVqWsyp66e6cBj3P-tmdhnNMnUwVCMN4_XPNv8aE7fJUgU6d69Ei32S3WKehwudq_U2Hye54Jql0Ihq1gQh1sgcA9IPidNug"
  //  api.setAccessToken(_tok)
    api.setRefreshToken(_ref)
        
//    
//    val CR2 = api.refreshAccessToken.grantType("refresh_token").
//      refreshToken(_ref).build.get
//    
//    val tok2 = CR2.getAccessToken
//    val expin2 = CR2.getExpiresIn
//    println(tok2)
//    println(expin2)
//      
    
   // tok2
    
    api
  }
  
  def compare(str1 : String, str2 : String) = {
    val s1 = str1.replace("&", "and")
    val s2 = str2.replace("&", "and")
    
    s1.trim.toLowerCase == s2.trim.toLowerCase
  }

}

object UAlt18 extends App {

  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\""")

  val s = getSpotify(conn)
  
  val conn2 = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\Res\""")
    
  val address = """http://theunofficialalt18countdownplaylists.com/"""
  val res = getUALT18Cal(address,conn)
 
 val alt18add = res.flatMap( resAdd => getUALT18(resAdd,conn).flatten )
 
 val results = alt18add.collect( {
   case x  : String if(x.contains("results-")) => getUALT18Table(x,conn2) 
 } )
 
 val fResults = results.flatten
 
 val ats = fResults.map ( x=> {
   val at = x.split("-")
   val artist = at(0)
   val title = if(at.size > 1) { at(1) } else { "" } 
   (artist, title) 
 })
 
 val byArtistMap = ats.groupBy(_._1.toLowerCase)
 
// ats.foreach(println)
// 
// byArtistMap.foreach( { case(a,_) => { 
//     val srch = s.searchArtists(a).build
//     val res = srch.get
//     val h = res.getItems.headOption
//     h.collect( { 
//       case x if !compare(x.getName, a) => {
//         println("Artist Name: " + a)
//         res.getItems.foreach(x=>println(x.getName)) }} )
//  }
// })
 
 
 def findArtist( name : String, filter : (String,String) => Boolean = (_,_)=>true ) = {
    val srch = s.searchArtists(name).build
    val res = srch.get
    val matches = res.getItems.filter( x=>filter(name,x.getName) )
    matches
  }
  
  def findTopArtist( name : String ) = {
    val srch = s.searchArtists(name).limit(1).build
    val res = srch.get
    res.getItems.head
  }
  
 val possibles = byArtistMap.map( { case (a,_) => (a,findArtist(a,compare)) } ) 
 val matches = possibles.filter(_._2.size==1)
 val multimatches = possibles.filter(_._2.size>1)
 val nomatches = possibles.filter(_._2.size==0)
 
 println("Matches Count:" + matches.size)
 
 println("MultiMatches Count:" + multimatches.size)
 
 multimatches.foreach({ case (k,v) =>
   println("For Artist " + k + " found " + v.size + " possibles")
   v.foreach(x=>println(x.getName + ":" + x.getId))
 })

 println("NoMatches Count:" + nomatches.size)
 nomatches.foreach({ case (k,v) =>
   println(k)
   findArtist(k).foreach { x => println(x.getName) }
   })
 
 conn.dispose
 conn2.dispose

} // end app