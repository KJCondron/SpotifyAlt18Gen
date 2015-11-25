package com.kjcondron.music

import scala.Option.option2Iterable
import com.kjcondron.web.HTTPWrapper
import com.wrapper.spotify._
import com.wrapper.spotify.models._
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import java.io.FileOutputStream
import java.io.File
import scala.collection.mutable.Buffer
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import com.wrapper.spotify.exceptions.BadRequestException
import UAlt18Parser._
import UAlt18F._

object UAlt18F {
          
  val getAPI = Api.builder.clientId("20212105b1764ecb81a226fca7796b4b")
                          .clientSecret("57a3b615f9be4b17b61361da94b35204")
                          
  // URL is always the same for given secret / client id and scopes
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
   
  // if we have a token, and it is new enough we can setup the api using just that
  // the refresh token can be used with secret to get a new access token
  def getSpotify(conn : HTTPWrapper) = {
             
    val _ref="AQC0OxT6ayAimF5iie5cfjU2PNBa0Fxc1T56tUfGtC57zn3peIrPfeuFom31Gt9uMJHpjiJf486TUdnMVPrtDXs6W-xch6fjzukOKfVjvZk7iIrjNxerlpHvj36w1JXjqYI" 
  
    val accLoc = """acc.tmp""" 
    val accFile = new File(accLoc)
    
    println(accFile.getAbsoluteFile)
    
    val apix = if(accFile.exists)
      try
      {
        val accTok = io.Source.fromFile(accFile, "utf-8").mkString
        val _api = Api.builder.accessToken(accTok).build
        // test token
        _api.getMe.build.get
        Some(_api)
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
  
  //val alt18s = getUAlt18(conn,conn2)
  val alt18s = UAlt18AnnualParser.getUAlt18Annual
  
  def fn(gg:String, sep : String) = { 
      val x = clean(gg)  
      val at = x.split(" " +sep+ " ")
      val artist = at(0)
      val title = if(at.size > 1) { at(1) } else { "" }
    
      MyTrack(artist.toLowerCase, title)
  }
    
  val parseArtistAndTitle : PartialFunction[String, MyTrack] = {
    case g if g.contains("-") => fn(g,"-")
    case g if g.contains("–") => fn(g,"–")
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
 
 println("matchingArtists:" + matchingArtistsBuf.size)
 println("non-matchingArtists:" + nonMatchingArtists.size)
 
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
   finds.foreach(
       {case (a,ls) if ls.size > 0 => { 
           println(a.getName + ":" + ls.mkString(","))}
       case (a,ls) => { 
           println(a.getName + ":" + "none")}
       })
   
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
   
   //addTracks(uid, plid, newPLTrackUris)
         
   conn.dispose
   conn2.dispose
   
} // end app