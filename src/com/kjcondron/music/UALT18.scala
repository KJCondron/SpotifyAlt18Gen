package com.kjcondron.music

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import com.kjcondron.web.HTTPWrapper
import com.wrapper.spotify._
import com.wrapper.spotify.models._
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
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
import com.wrapper.spotify.exceptions.BadRequestException
import com.wrapper.spotify.methods.TrackSearchRequest

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
  
  def getAPI = {
    val clientId = "20212105b1764ecb81a226fca7796b4b"
    val secret = "57a3b615f9be4b17b61361da94b35204"
    
    
    Api.builder.clientId(clientId)
              .clientSecret(secret)
              
  }
  
  // URL is always the same for given secret / cliient id and scopes
  def getSpotifyAuthURL(conn : HTTPWrapper) = {
    
    val redir = "https://www.google.com/"
    val api = getAPI.redirectURI(redir)
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
    val authorizeURL = api.createAuthorizeURL(scopes, state);
    println(authorizeURL)
    
    api
    
  }
  
  // using the url above, stick it in a web browser get the code and we can get our tokens
  // code is one time use
  def getSpotifyTok(conn : HTTPWrapper) = {
    
    val api = getAPI.build
  
    val code = "AQDi2DRVQy3ZAu562AWLU5EM_OhWbTm9L4sQlmXKWw-9qxN79yRDWx3eiNKnVg0OnLZ7n6jHOqgLwfpkJCGMpV7K518ZYjuLE_i_nHDE6328oxGpkpJjcua-AjyWGJUEIPfFmOlJJXnLeP5SC9KuFk9OBp775OY4wa8EP3eJiqp8GHeNumy5VGhqhEVFrzsB21eltOtRwj3y70qqecJBAHKyXED70ySnvVZNaoweqh1PAnTipaDs4C77jsxSHEt1PYQ_LSH7c7GwmBRowCFAKcOT9y6Ho-8b89TRmJHEAjh4vJQF5xP6QPDrr6RsYet89eCzQOLOyHzm"
    val authCode = api.authorizationCodeGrant(code).build.get
  
    val tok = authCode.getAccessToken
    val ref = authCode.getRefreshToken
    val expin = authCode.getExpiresIn
    println(tok)
    println(ref)
    println(expin)
    
    // must set tokens before have access
    api.setAccessToken(tok)
    api.setRefreshToken(ref)
    
    val user = api.getMe.build.get
    println(user.getId)
   
    
    api
  }
   
  // if we have a token, and it is new enough we can setup the api using just that
  // the refresh token can be used with secret to get a new access token
  def getSpotify(conn : HTTPWrapper) = {
             
    val _acc="BQBfTmcbyxisX-wFkVs4-AV2uXC07J3pm-Ws4lSl8QL2mknfNnqaQBIDGGPntPfHcpnJNPo5Aj1MDSfoqEeEY0xRFLab45IoX0lUhCb9Gqv_Gm1qPdhbzwveLC_JtI0pkKhwPUiiV9Z1Bkku5FHGXMHEbMeUBK0SZmga9N1sN5-aK9DonO_zFA3WV0s-eKKmnc78HQcaeNAH7o8QfAmSx_vkHTmVQQbLyRcvvJvXIJ-9nNuIiAmZhd2PI0ewxZsPI2EjwJbefQ"
    val _ref="AQC0OxT6ayAimF5iie5cfjU2PNBa0Fxc1T56tUfGtC57zn3peIrPfeuFom31Gt9uMJHpjiJf486TUdnMVPrtDXs6W-xch6fjzukOKfVjvZk7iIrjNxerlpHvj36w1JXjqYI" 
  
    val accLoc = """acc.tmp""" 
    val accFile = new File(accLoc)
    
    println(accFile.getAbsoluteFile)
    
    val apix = if(accFile.exists)
      try
      {
        val accTok = io.Source.fromFile(accFile, "utf-8").mkString
        Some(Api.builder.accessToken(accTok).build)
      }
      catch  {
         case t:Throwable => {
           println ("******STALE TOKEN*********") 
           accFile.delete
           accFile.createNewFile
           None
         }
      }
      else{
        println ("******NO FILE*********")
        accFile.createNewFile
        None
      }
        
    val api2 = apix.getOrElse( {
      val acc2Tk = getAPI.refreshToken(_ref).build.refreshAccessToken.build.get.getAccessToken
      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(accFile), "UTF-8"));  
      writer.write(acc2Tk)
      writer.close
      Api.builder.accessToken(acc2Tk).build
    })
    
    api2 
  }
  
  def compare(str1 : String, str2 : String) = {
    
    val s1 = str1.replace("&", "and")
    val s2 = str2.replace("&", "and")
    
    s1.trim.toLowerCase == s2.trim.toLowerCase
  }
  
  def compare2(str1 : String, str2 : String) = {
    
    println("Comparing:" + str1 + " and " + str2) 
        
    val sFilter = (x:Char) => x > 64 && x < 123
    val myRep = (s:String) => 
      s.replace("&", "and").filter(sFilter).trim.toLowerCase
      
    val dropParenClause = (s:String) => 
      s.takeWhile(_!= '(').trim.toLowerCase
      
    myRep(str1) == myRep(str2) || 
      dropParenClause(str1) == dropParenClause(str2)
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

case class MyTrack(artist : String, title : String)

// if you make this impl GenTraversableOnce then
// you can use coll.flatten to remove NoSongs from List[SearchedSongs]
// like you can with Option (clever huh?)
sealed abstract class SearchedSong {
  override def toString = alt18Name
  def alt18Name : String
  def isFound : Boolean = false
  def equals(that:Any) : Boolean
}
case class FoundSong(alt18Name : String, track : Track) extends SearchedSong
{
  def spotifyName = track.getName 
  def spotifyId = track.getId
  override def isFound = true
  override def equals(that:Any) : Boolean = 
    that match {
    case fs : FoundSong => fs.track.getId == track.getId
    case _ => false
  }
}
case class NoSong(alt18Name : String) extends SearchedSong {
override def equals(that:Any) : Boolean = 
    that match {
    case fs : NoSong => fs.alt18Name == alt18Name
    case _ => false
  }
}

object Util {
      
  def backOffLogic[T]( g : =>T, count : Int = 0 ) : T = 
     try { g  }
     catch {
        case b:BadRequestException if(b.getMessage == "429" && count < 10) => {
              println("backing off:" + count)
              Thread sleep 4000
              backOffLogic(g, count+1)
          }
        case t:Throwable => {println("count:" + count); throw t}
     }
}
    
import com.kjcondron.music.Util.backOffLogic

object UAlt18 extends App {
  
  println("start")
  
  val artistFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\artists.txt"""
  val artistFile = new File(artistFileLoc)
  val artistCache = if ( artistFile.exists ) Some( loadExisting(artistFileLoc) ) else None
  
  val songFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\songs.txt"""
  val songFile = new File(songFileLoc)
  val songs = if ( songFile.exists ) Some( loadExisting(songFileLoc) ) else None
  
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\""")
  val conn2 = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\Res\""")
  
  def clean(s:String) = s.replace(160.toChar, 32.toChar) 
 
  println("getting spotify")
  
  val s = getSpotify(conn)
  
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
    
  val parseArtistAndTitle : PartialFunction[String, MyTrack] = {
      case g if g.contains("-") => 
       val x = clean(g)  
       val at = x.split(" - ")
       val artist = at(0)
       val title = if(at.size > 1) { at(1) } else { "" }
       
       MyTrack(artist.toLowerCase, title)
  }
  
  val alt18wTracks = alt18s.map( { case(url,songs) => (url,songs.collect(parseArtistAndTitle)) })   
  
  val allTracks = alt18wTracks.map(_._2).flatten
  println("got artists " + allTracks.size)
  
  val allTracksByArtist = allTracks.groupBy(_.artist).map { case (k,v) => (k,v.map(_.title).distinct ) } 
  println("got distinct artists " + allTracksByArtist.size)
  
 def findArtist( name : String, filter : (String,String) => Boolean = (_,_)=>true ) = {
    println("SEARCHING FOR ARTIST: " + name)
    val fn = (nm:String) => {
      val srch = s.searchArtists(nm).build
      val res = srch.get
      res.getItems.filter( x=>filter(nm,x.getName) )
    }
    backOffLogic( fn(name) )
  }
  
  // not very important function. The filter isn't used this just gets possible songs
  // by title
 def findSong( name : String, filter : (String,String) => Boolean = (_,_)=>true ) = {
    val srch = s.searchTracks(name).build
    val res = srch.get
    val matches = res.getItems.filter( x=>filter(name,x.getName) )
    matches.foreach(t=>println("song: " + t.getName) )
    matches
  }
  
 // get all artists with EXACT name match (compare filter), looking in existing map first
 val possibles = allTracksByArtist.map( { case (a,_) =>
   val artistId = artistCache.flatMap( _.get(a) )
   val buf = artistId.map({ case (name,id) => {
     val artist = new Artist
     artist.setId(id)
     artist.setName(name)
     Buffer( artist )
     }})
   (a, buf.getOrElse(findArtist(a,compare))) } )
   
 val (matchingArtistsBuf, nonMatchingArtists) = possibles.partition( _._2.size==1 )
 val matchingArtists = matchingArtistsBuf.mapValues { _.head }
 
 // or (time these)
 //val matchingArtists1 = possibles.mapValues { case ms if ms.size==1 => ms.head }
 //val nonMatchingArtists1 = possibles.mapValues { case ms if ms.size!=1 => ms }
 //val nonMatchingArtists2 = possibles.filter(p => p._2.size != 1)
 
 def anySongMatches( songList : List[String])( track : Track ) =
   songList.exists(song => compare(song,track.getName))
 
 // for the not found artists, get all artist from spotify
 // if any song from alt 18 list matches any of the top
   // songs on spotify then we found it
 val moreArtists : Map[String,Option[Artist]] = nonMatchingArtists.map { 
   case (artistName,_) => 
     // for each possible artist get top tracks
     // if any top track matches any track in our
     // parsed list return artist id...else None
     val artistList = findArtist(artistName) // get all possible artists (no filter)
     val oArtist = artistList.find { artist => 
         backOffLogic(s.getTopTracksForArtist(artist.getId, "US").build.get.exists(
             anySongMatches(allTracksByArtist(artistName)) _ ) )}
     oArtist.map( a => (artistName, Some(a)) ).getOrElse( (artistName, None))
 }
   
 val (ll, rr) = moreArtists.partition(_._2.isDefined)
 val moreFoundArtists = ll.collect { case (k,Some(v)) => (k,v) }
 val missingArtists = rr.keys
 
 
 // for still missing artists print name, alt18 songs
 // top 5 artists on spotify and top tracks for those
 // artists on spotify
 missingArtists.foreach { x =>
   println("Searching For: " + x)
   println("Songs: " + allTracksByArtist(x).mkString(","))
   findArtist(x).take(5).foreach({  t=>
     println("Artist: " + t.getName + ":" + t.getId)
     println( backOffLogic(s.getTopTracksForArtist(t.getId, "US").build.get).map(_.getName).mkString(",") )
     })}
 
 val allArtists = matchingArtists ++ moreFoundArtists
 println("Artists Parsed:" + allTracksByArtist.size)
 println("Matches Count:" + allArtists.size)
 
 val empty : Buffer[Track] = Buffer()

 // another idea for finally missing artists
 // search for each song on spotify and print the
 // artist for eaach track
 missingArtists.foreach( a => {
   println("For Artist " + a + " based on tracks possible matches are: ")
   val songs = allTracksByArtist(a)
   val tracks = songs.flatMap( s => { println(s); if( s.size > 1) findSong(s) else empty } )
   tracks.foreach( _.getArtists.foreach ( at=>println("artist: " + at.getName +":" + at.getId) ) )
 })
 
 if (!artistFile.exists) artistFile.createNewFile
   
 val fl = new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream(artistFile), "UTF-8"));  
 
 (allArtists).foreach {
   case(k,v) => fl.write( k + ":" + v.getName + ":" + v.getId + "\n")
 }
 fl.close
 
 println("No Matches Count:" + missingArtists.size)
 
 val mismatchFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\mismatch.txt"""
 val mismatchFile = new File(mismatchFileLoc)
  
 if (!mismatchFile.exists)
   mismatchFile.createNewFile
 
 val writer = new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream(mismatchFile), "UTF-8"));  
   
 (missingArtists).foreach(x=> writer.write(x + "\n"))
  writer.close
  
  // the function used to be wrapped in a try, catch...but was this just for before backoff logic?
  def tryFindSongOnSpotify( title : String, artist : Artist ) = try {
    findSongOnSpotify(title, artist)
  }
 catch {
   case _ : Throwable => NoSong(title)
   }
  
 def findSongOnSpotify( title : String, artist : Artist ) : SearchedSong = {
                         val topTracks = backOffLogic(s.getTopTracksForArtist(artist.getId, "US").build.get)
                         val matchingTracks = topTracks.filter { t => compare2(t.getName,title) }
                         if (matchingTracks.size > 1) 
                           FoundSong(title, matchingTracks.head) else
                           NoSong(title)
                    }
 
 def findASong(artist : String, title : String) : java.util.List[Track] = 
   {
     val str = s"track:$title artist:$artist"
     val items = backOffLogic(s.searchTracks(str).market("US").build.get.getItems)
     
     if(items.size == 0 && (title.contains( "(" ) || artist.contains( "(" )))
       findASong(artist.takeWhile(_!='('),title.takeWhile(_!='('))
     else
       items
   }
 
  
  def makeTrack(name : String, id : String) = {
    val tr = new Track
    tr.setName(name)
    tr.setId(id)
    tr
  }
  
  val mt = (makeTrack _).tupled
  
  val foundSongs : Map[Artist, List[SearchedSong]] = allArtists.map { case (artistName,spotifyArtist) =>
       // for each artist
       val songsFromAlt18 = allTracksByArtist(artistName)
       val spotifySongs = songsFromAlt18.map( _songTitle => {
         def fSOS() = findSongOnSpotify(_songTitle, spotifyArtist)
         def mt2( x : (String,String)) = FoundSong(_songTitle, mt(x)) 
         val oT = songs.flatMap( _.get(_songTitle) ) // get song from local cache if both exist
         if(!oT.isDefined)
           println("*****CACHE MISS*****:  " + _songTitle)
         oT.map(mt2).getOrElse(fSOS)
         // or oT.fold(fSOS)( mt2 )
       })
       (spotifyArtist,spotifySongs) }
   
   // for these artist we found at least a song. This is artist and the found songs
   val fs = foundSongs.map { case(a,ss) =>
     (a, ss.collect { case fs : FoundSong => fs }) }.filter{ case(_,ts) => ts.size > 0}
   
   // for these artist we missed at least a song. This is artist and the missed songs
   val nfs = foundSongs.map { case(a,ss) => 
     (a, ss.collect { case ns : NoSong => ns.alt18Name }) }.filter{ case(_,ts) => ts.size > 0}
   
   println("Found " + fs.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   println("Didn't Find " + nfs.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   
   def compareBoth( t : Track, artist : Artist, title : String ) = {
     println(t.getArtists.head.getName +":"+ artist.getName +":" + t.getName + ":" + title)
     println(t.getArtists.head.getId +":"+ artist.getId)
     t.getArtists.head.getId == artist.getId &&
       compare2(t.getName, title)
   }
   
   val searchRes : Map[Artist, List[SearchedSong]] = nfs.map( { case(a,ss) => (a,{
     ss.map( song => { 
       val songList = findASong(a.getName, song)
       println("possibles for " + song + ":" + songList.map(t=>t.getName +":" + t.getId))
       songList.find( t=>compareBoth(t,a,song) ).map( 
           t => FoundSong(song,t) ).getOrElse(
               NoSong(song + ":" + songList.map(_.getName))) 
     })  
   })} )
   
   val finds : Map[Artist, List[FoundSong] ]= searchRes.mapValues( ts => ts.collect( {case fs : FoundSong => fs } ))
   
   val sfl = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(songFile), "UTF-8"));  
    
   val (inFs, notInFs) = finds.partition( p => fs.contains(p._1) )
   val fsp = fs ++ notInFs
   
   //val alls = (fs ++ finds)
   // where we have artists in both maps we need to merge the
   // values that share the same key
   val alls = inFs.foldLeft(fsp)( (acc,e) => acc + (e._1->(fsp(e._1)++e._2)) )
   
   val fspp = fs.toSeq ++ finds.toSeq
   val fsppg = fspp.groupBy(_._1).mapValues(_.map(_._2).flatten.toList)
   
   assert(fsppg.size==alls.size)
   assert(fsppg.keySet==alls.keySet)
   assert(fsppg.values.toSet==alls.values.toSet)
   // they both work, so time them
   
   alls.foreach {
     case (_,ti) => ti.foreach { tr =>
         sfl.write( tr.alt18Name + ":" + tr.spotifyName + ":" + tr.spotifyId + "\n")
   }}
   
   sfl.close()
  
   val stillMissing = searchRes.mapValues( ss => ss.collect({ case s:NoSong => s}) ).filter({case(_,fs)=>fs.size>0})
     
   println("now Found " + finds.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   finds.foreach({case (a,ls) if ls.size > 0 => { 
     println(a.getName + ":" + ls.mkString(","))
   }})
   
   println("Still Didn't Find " + stillMissing.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   stillMissing.foreach({case (a,ls) =>{ 
     println(a.getName + ":" + ls.mkString(","))
   }})
   
   val spotifySongs = alls.values.flatten.map( x=> (x.alt18Name,x.track) ).toMap
   val sps2 = spotifySongs.mapValues( t=> backOffLogic(s.getTrack(t.getId).build.get)  )
   
   val spotifySongs2 = alls.values.flatten
   val sps2_2 = spotifySongs2.map( t => backOffLogic(s.getTrack(t.track.getId).build.get)  )
   
   val uid = backOffLogic(s.getMe.build.get.getId)
   
   def getPL(name : String) = try {
     println("trying to get:" + name)
     val plpage = backOffLogic( s.getPlaylistsForUser(uid).build.get )
     val pls = plpage.getItems.filter(_.getName == name)
     pls.headOption
   }
   catch{
       case bre:BadRequestException => None
   }
   
   println("get all18 pl")
   val plid = getPL("all18").map(_.getId).getOrElse(
            s.createPlaylist(uid, "all18").build.get.getId)
            
   
   def getPLTracks(uid : String, plid : String, offset : Int = 0, acc : List[PlaylistTrack] = List()) : 
     List[PlaylistTrack] =
   {
      val currItems = s.getPlaylistTracks(uid, plid).offset(offset).build.get.getItems
      
      if(currItems.size == 0)
        acc
      else
        getPLTracks(uid,plid,offset+100,acc++currItems)
   }
               
   val pltracks = getPLTracks(uid, plid)
   
   implicit class Wrapper1(_tr : PlaylistTrack) {
       def matches(_tr2 : Track) = _tr.getTrack.getId == _tr2.getId  
   }
   def closure1(_tr2 : Track) = pltracks.exists( _.matches(_tr2) )
   val newtracks = sps2_2.filterNot( closure1 )
   
   println("spotify songs count:" + sps2.size)
   println("spotify 2 songs count:" + sps2_2.size)
   println("new track count:" + newtracks.size)
   
   val newPLTrackUris = newtracks.map(_.getUri).toList
   
   def addTracks(uid : String, plid : String, tracks : List[String]) : Unit = {  
       if(tracks.size > 0){
       val maxSize = 50
         if(tracks.size > maxSize)
           addTracks(uid, plid, tracks.drop(maxSize))
          
         val url = s.addTracksToPlaylist(uid, plid, tracks.take(maxSize)).build
         println(url)
         val _ = backOffLogic(url.get)
     }
   }
   
   addTracks(uid, plid, newPLTrackUris)
         
   conn.dispose
   conn2.dispose
   
} // end app