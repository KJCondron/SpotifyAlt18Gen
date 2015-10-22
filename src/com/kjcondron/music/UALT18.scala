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
             

    val _acc="BQA5Pe99HURTN_uHDkSnaZ6cqDae4pbFLX4tohvG7FQheCING8VqfOtMGaZLgyI33n6I849PC7zUmhBj20qFiiYo1_hqv60r-5ZmLJnYcY4hZDOySWvAbiUR7M61_F7Zjx5TjiuZpGgIHv8hgMBmSBxxvaVdPJpIhgW4jw2tjVQRNVM6QhuYR6tgtBW87ChWeriO0JZorM1CHfriFEaT3pe5tI8KVCLqTTjzOVu2RwwO7QP060s95tt9AzWcwd2dOF8BKw"
    val _ref="AQC0OxT6ayAimF5iie5cfjU2PNBa0Fxc1T56tUfGtC57zn3peIrPfeuFom31Gt9uMJHpjiJf486TUdnMVPrtDXs6W-xch6fjzukOKfVjvZk7iIrjNxerlpHvj36w1JXjqYI" 
  
    val api = 
     try{
       val _api = Api.builder.accessToken(_acc).build
       val user = _api.getMe.build.get
       _api
     }
     catch{
       case t:Throwable => {
         val _acc2 = getAPI.refreshToken(_ref).build.refreshAccessToken.build.get.getAccessToken
         println(_acc2)
         Api.builder.accessToken(_acc2).build
       }
     }
     
     val user = api.getMe.build.get
     println(user.getEmail)
    //api.createPlaylist(user.getId, "test").build.get
    api 
  }
  
  def compare(str1 : String, str2 : String) = {
    
    val s1 = str1.replace("&", "and")
    val s2 = str2.replace("&", "and")
    
    s1.trim.toLowerCase == s2.trim.toLowerCase
  }
  
  def compare2(str1 : String, str2 : String) = {
        
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

object UAlt18 extends App {
  
  println("start")
  
  val artistFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\artists.txt"""
  val artistFile = new File(artistFileLoc)
  val artists = if ( artistFile.exists ) Some( loadExisting(artistFileLoc) ) else None
  
  val songFileLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\songs.txt"""
  val songFile = new File(songFileLoc)
  val songs = if ( songFile.exists ) Some( loadExisting(songFileLoc) ) else None
  
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\""")
  val conn2 = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\Res\""")
  
  println("getting spotify")
  
  //val s = getSpotifyAuthURL(conn)
  //val s = getSpotifyTok(conn)
  val s = getSpotify(conn)
  
  val address = """http://theunofficialalt18countdownplaylists.com/"""
  
  val res = getUALT18Cal(address,conn)
  
  val alt18add = res.flatMap( resAdd => getUALT18(resAdd,conn).flatten )
  
  // results is each weeks alt-18 address (for which we can get the date)
  // and the "artist-title" list
  val results = alt18add.collect( {
     case x  : String if(x.contains("results-")) => (x,getUALT18Table(x,conn2)) 
   } )
    
  val results2 = results.map(_._2)
 
  val fResults = results2.flatten
  
  println("got results " + fResults.size)
  
  def clean(s:String) = s.replace(160.toChar, 32.toChar) 
 
  val parseArtistAndTitle : PartialFunction[String, (String,String)] = {
      case g if g.contains("-") => 
       val x = clean(g)  
       val at = x.split(" - ")
       val artist = at(0)
       val title = if(at.size > 1) { at(1) } else { "" }
       
       (artist, title)
  }
    
  val ats = fResults.collect(parseArtistAndTitle)
  println("got artists " + ats.size)
  val byArtistMap = ats.groupBy(_._1.toLowerCase).map { case (k,v) => (k,v.map(_._2).distinct ) }
  
 def findArtist( name : String, filter : (String,String) => Boolean = (_,_)=>true ) = {
    println("SEARCHING FOR ARTIST: " + name)
    
    val fn = (nm:String) => {
      val srch = s.searchArtists(nm).build
      val res = srch.get
      res.getItems.filter( x=>filter(nm,x.getName) )
    }  
    
    val matches = try {
      fn(name)
    }
    catch {
      case b:BadRequestException => {
        if(b.getMessage == "429") {
          // too many requests back off for 10 sec
          try {
            println("back off")
            Thread sleep 10000
            fn(name)
          }
          catch {
            // failed twice stop
            case t:Throwable => {
              println("2 exception s:" + t)
              Buffer[Artist]()
            }
          }
        }
        else
          Buffer[Artist]()
      }
      case t:Throwable => {
        println("exception:" + t)
        Buffer[Artist]()
      }
    }
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
 
 //try{
   val foundSongs = allMatches.map { case (name,artist) =>
       val songList = byArtistMap(name)
       (artist,songList.map( q => {
         val oT = songs.flatMap( _.get(q) ) // get song from local cache if both exist
         oT.map( t => { // we found the song in local cache
           val tr = new Track
           tr.setName(t._1)
           tr.setId(t._2)
           (q, Some(tr)) } ).getOrElse( try { // we did not find the song locally
                         val topTracks = s.getTopTracksForArtist(artist.getId, "US").build.get
                         val matchingTracks = topTracks.filter { t => compare2(t.getName,q) }
                         if (matchingTracks.size > 1) (q, Some(matchingTracks.head)) else (q, None)
                         }
                         catch {
                           case _ : Throwable => (q,None)
                         }
                         )}))}
   
   val fs = foundSongs.map { case(a,ss) => 
     (a, ss.collect { case (sn,Some(x)) => (sn,x) }) }.filter{ case(_,ts) => ts.size > 0}
   
   val nfs = foundSongs.map { case(a,ss) => 
     (a, ss.collect { case (sn,None) => sn }) }.filter{ case(_,ts) => ts.size > 0}
   
   val sfl = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(songFile), "UTF-8"));  
   
   fs.foreach {
     case (_,ti) => ti.foreach {
       case(s,t) =>
         sfl.write( s + ":" + t.getName + ":" + t.getId + "\n")
   }}
   
   sfl.close()
   
   println("Found " + fs.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   println("Didn't Find " + nfs.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   
   def findASong(artist : String, title : String) : java.util.List[Track] = 
   {
     val str = s"track:$title artist:$artist"
     val items = s.searchTracks(str).market("US").build.get.getItems
     
     if(items.size == 0 && (title.contains( "(" ) || artist.contains( "(" )))
       findASong(artist.takeWhile(_!='('),title.takeWhile(_!='('))
     else
       items
   }
 
   def compareBoth( t : Track, artist : Artist, title : String ) = {
     println(t.getArtists.head.getName +":"+ artist.getName +":" + t.getName + ":" + title)
     println(t.getArtists.head.getId +":"+ artist.getId)
     t.getArtists.head.getId == artist.getId &&
       compare2(t.getName, title)
   }
   
   val sfl2 = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(songFile,true), "UTF-8"));  
   
   def nastyHack(song : String, track : Track) = 
     sfl2.write( song + ":" + track.getName + ":" + track.getId + "\n")
   
   val searchRes : Map[Artist, List[Either[Track,String]]] = nfs.map( { case(a,ss) => (a,{
     ss.map( song => { 
       val songList = findASong(a.getName, song)
       println("possibles for " + song + ":" + songList.map(t=>t.getName +":" + t.getId))
       songList.find( t=>compareBoth(t,a,song) ).map( t=> { nastyHack(song,t); Left(t) } ).getOrElse(Right(song + ":" + songList.map(_.getName))) 
     })  
   })} )
   
   sfl2.close()
   
   val stillMissing = searchRes.map ( { case (a,ls) => (a, ls.filter(_.isRight)) }).filter( {case (a,ls) => ls.size > 0 } )
   val nowFound = searchRes.map ( { case (a,ls) => (a, ls.filter(_.isLeft)) }).filter( {case (a,ls) => ls.size > 0 } )
   
   println("now Found " + nowFound.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   nowFound.foreach({case (a,ls) =>{ 
     println(a.getName + ":" + ls.mkString(","))
   }})
   
   println("Still Didn't Find " + stillMissing.foldLeft(0)((acc,x) => acc+x._2.size) + " songs")
   stillMissing.foreach({case (a,ls) =>{ 
     println(a.getName + ":" + ls.mkString(","))
   }})
    
  conn.dispose
  conn2.dispose
 
   
} // end app