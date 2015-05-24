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
import com.wrapper.spotify.methods.TopTracksRequest
import com.wrapper.spotify.models.Track
import java.util.Calendar
import java.io.FileOutputStream
import java.io.File
import java.io.FileWriter
import scala.collection.mutable.Buffer
import java.io.BufferedWriter
import java.io.OutputStreamWriter
    
 
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
    
    /*
    val at = api.refreshAccessToken.build.get
    val tok = at.getAccessToken
    println("Access Token: " + tok)
    
    val me = api.getMe.accessToken(tok).build.get
    println("ID: " + me.getId)
    */
        
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
  
  def partitionMap[A,B,C]( l : List[A] )( pred : A=>Boolean, m1 : A=>B, m2 : A=>C ) : (List[B], List[C]) = 
    l.foldLeft( (List[B](), List[C]() ) )( (acc,x)=> 
      if( pred(x) )
        ( m1(x) :: acc._1, acc._2)
      else
        (acc._1, m2(x) :: acc._2)
        )
        
        
  def loadExisting(fileLoc : String) = {
    io.Source.fromFile(fileLoc, "utf-8").getLines.map( line => { 
      val entry = line.split(":")
      (entry(0), (entry(1), entry(2)) )
    }).toMap
  }
}

object UAlt18 extends App {
  
  val artistFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\artists.txt"""
  val artistFile = new File(artistFileLoc)
  val artists = if ( artistFile.exists ) Some( loadExisting(artistFileLoc) ) else None
  
  val songFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\songs.txt"""
  val songFile = new File(songFileLoc)
  val songs = if ( songFile.exists ) Some( loadExisting(songFileLoc) ) else None
  
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\""")
  val conn2 = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\Res\""")
  
  val s = getSpotify(conn)
  
  val address = """http://theunofficialalt18countdownplaylists.com/"""
  
  val res = getUALT18Cal(address,conn)
  
  val alt18add = res.flatMap( resAdd => getUALT18(resAdd,conn).flatten )
  
  val results = alt18add.collect( {
     case x  : String if(x.contains("results-")) => getUALT18Table(x,conn2) 
   } )
 
  val fResults = results.flatten
  
  def clean(s:String) = s.replace( 160.toChar, 32.toChar) 
 
  val ats = fResults.collect { case g if g.contains("-") => 
     val x = clean(g)  
     val at = x.split(" - ")
     val artist = at(0)
     val title = if(at.size > 1) { at(1) } else { "" }
     
     if( at.size == 1 ) { println(x)   }
     
     (artist, title) 
   }
 
  val byArtistMap = ats.groupBy(_._1.toLowerCase).map { case (k,v) => (k,v.map(_._2).distinct ) }
  
 def findArtist( name : String, filter : (String,String) => Boolean = (_,_)=>true ) = {
    println("SEARCHING FOR ARTIST: " + name)
    val srch = s.searchArtists(name).build
    val res = srch.get
    val matches = res.getItems.filter( x=>filter(name,x.getName) )
    matches
  }
 
 def findSong( name : String, filter : (String,String) => Boolean = (_,_)=>true ) = {
    val srch = s.searchTracks(name).build
    val res = srch.get
    val matches = res.getItems.filter( x=>filter(name,x.getName) )
    matches.foreach(t=>println("song: " + t.getName) )
    matches
  }
  
  def findTopArtist( name : String ) = {
    val srch = s.searchArtists(name).limit(1).build
    val res = srch.get
    res.getItems.headOption
  }
 
 // get all artists with exact name match, looking in existing map first
 val possibles = byArtistMap.map( { case (a,_) =>
   val artistId = artists.flatMap( _.get(a) )
   val buf = artistId.flatMap( { case (name,id) => {
     val artist = new Artist
     artist.setId(id)
     artist.setName(name)
     Some( Buffer( artist ) ) }} )
   (a, buf.getOrElse(findArtist(a,compare))) } ) 
  
 val (newMatches1, newNoMatches) = possibles.partition( _._2.size==1 )
 val newMatches = newMatches1.mapValues { _.head }

 def anySongMatches( songList : List[String])( track : Track ) =
   songList.exists(song => compare(song,track.getName))
 
 val newMapped : Map[String,Either[Artist,Boolean] ] = newNoMatches.map { 
   case (artistName,_) => 
     // for each possible artist get top tracks
     // if any top track matches any track in our
     // parsed list return artist id...else????
     val artistList = findArtist(artistName) // get all possible artists (no filter)
     val oArtist = artistList.find { artist => 
         s.getTopTracksForArtist(artist.getId, "US").build.get.exists(
             anySongMatches(byArtistMap(artistName)) _ ) }
     oArtist.map( a => (artistName, Left(a)) ).getOrElse( (artistName, Right(false)))
 }
   
 val (ll, rr) = newMapped.partition(_._2.isLeft)
 val (newMoreMatches, newFinalNoMatches) = (ll.collect { case (k,Left(v)) => (k,v) },  rr.keys)
  
 newFinalNoMatches.foreach { x =>
   println("Searching For: " + x)
   println("Songs: " + byArtistMap(x).mkString(","))
   findArtist(x).take(5).foreach({  t=>
     println("Artist: " + t.getName + ":" + t.getId)
     println( s.getTopTracksForArtist(t.getId, "US").build.get.map(_.getName).mkString(",") )
     })}
 
 val allMatches = newMatches ++ newMoreMatches
 println("Artists Parsed:" + byArtistMap.size)
 println("Matches Count:" + allMatches.size)
 
 val empty : Buffer[Track] = Buffer()
 newFinalNoMatches.map( a => {
   println("For Artist " + a + " based on tracks possible matches are: ")
   val songs = byArtistMap(a)
   val tracks = songs.flatMap( s => { println(s); if( s.size > 1) findSong(s) else empty } )
   tracks.foreach( _.getArtists.foreach ( at=>println("artist: " + at.getName +":" + at.getId) ) )
 })
 
 if (!artistFile.exists) artistFile.createNewFile
   
 val fl = new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream(artistFile), "UTF-8"));  
 
 (allMatches).foreach {
   case(k,v) => fl.write( k + ":" + v.getName + ":" + v.getId + "\n")
 }
 fl.close
 
 println("No Matches Count:" + newFinalNoMatches.size)
 
 val mismatchFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\mismatch.txt"""
 val mismatchFile = new File(mismatchFileLoc)
  
 if (!mismatchFile.exists)
   mismatchFile.createNewFile
 
 val writer = new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream(mismatchFile), "UTF-8"));  
   
 (newFinalNoMatches).foreach(x=> writer.write(x + "\n"))
 writer.close
 
 val foundSongs = allMatches.map { case (name,artist) =>
     val songList = byArtistMap(name)
     val topTracks = s.getTopTracksForArtist(artist.getId, "US").build.get
     songList.map( s => {
       val oT = songs.flatMap( _.get(s) )
       oT.flatMap( t => {
         val tr = new Track
         tr.setName(t._1)
         tr.setId(t._2)
         Some((s, Some(tr))) } ) .getOrElse( {
                       val matchingTracks = topTracks.filter { t => t.getName.toLowerCase == s.toLowerCase }
                       if (matchingTracks.size == 1) (s, Some(matchingTracks.head)) else (s,None)
                       } )})}.flatten
 
 val songsPass1 = foundSongs.collect { case (s,Some(x)) => (s,x) }
 val songsMissingPass1 = foundSongs.collect { case (s,None) => s }
 
 val sfl = new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream(songFile), "UTF-8"));  
 
 songsPass1.foreach {
   case (s,t) => sfl.write( s + ":" + t.getName + ":" + t.getId + "\n")
 }
 sfl.close
 
 println("Found " + songsPass1.size + " songs")
 println("Didn't Find " + songsMissingPass1.size + " songs")
 songsMissingPass1.foreach(println) 
 
 //val user = s.getMe.build.get
 //println(user.getId)
 //s.createPlaylist(uuid, name)
 conn.dispose
 conn2.dispose

} // end app