package com.kjcondron.music

import com.wrapper.spotify._
import com.wrapper.spotify.models.Track
import com.wrapper.spotify.exceptions.BadRequestException
import com.wrapper.spotify.exceptions.ServerErrorException

import scala.collection.JavaConversions.asScalaBuffer

import java.io.File
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream

import com.kjcondron.web.HTTPWrapper
import UAlt18Parser._

object funcs {
  
  val getAPI = Api.builder.clientId("20212105b1764ecb81a226fca7796b4b")
                          .clientSecret("57a3b615f9be4b17b61361da94b35204")
  
  def getSpotify() : Api = {
             
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

  def loadExistingWithArtist(fileLoc : String) = {
    io.Source.fromFile(fileLoc, "utf-8").getLines.map( line => { 
      val entry = line.split(":")
      if(entry.size != 4)  println(line)
      ( (entry(0),entry(1)), (entry(2), entry(3)) )
    }).toMap
  }
  
  def compareScore(str1 : String, str2 : String) = {
    val lstr1 = str1.toLowerCase
    val lstr2 = str2.toLowerCase
    val ms = lstr2.foldLeft(0)((count,c) => {
      val x : Int = if(lstr1.contains(c)) 1 else 0
      count+x
    })
    ms * 1.0 / lstr2.size
  }
  
  def compare2(str1 : String, str2 : String) = {
    
    println("Comparing:" + str1 + " and " + str2 + " " + compareScore(str1, str2) + " " + compareScore(str2, str1) + ":" + (compareScore(str1, str2)==1.0 || compareScore(str2, str1)==1.0)) 
        
    val sFilter = (x:Char) => x > 96 && x < 123 // filter out any non lower case letters
    val myRep = (s:String) => s.replace("&", "and").toLowerCase.filter(sFilter).trim // apply filter after toLower to remove anything outside [a-z]
      
    val dropParenClause = (s:String) => 
      s.takeWhile(_!= '(').trim.toLowerCase
      
    myRep(str1) == myRep(str2) || 
      dropParenClause(str1) == dropParenClause(str2) ||
      compareScore(str1, str2) == 1.0 ||
      compareScore(str2, str1) == 1.0
  }
  
}

object Util2 {
  def backOffLogic[T]( g : =>T, count : Int = 0 ) : T = 
     try { g  }
     catch {
        case b:BadRequestException if(b.getMessage == "429" && count < 10) => {
              println("backing off:" + count)
              Thread sleep 4000
              backOffLogic(g, count+1)
          }
        case b:BadRequestException if(b.getMessage == "404" && count < 10) => {
              println("backing off (404):" + count)
              Thread sleep 4000
              backOffLogic(g, count+1)
          }
        case s:ServerErrorException if(count < 10) => {
              println("backing off (ServerErrorException):" + count)
              Thread sleep 4000
              backOffLogic(g, count+1)
          }
        case t:Throwable => { println("back off caught other exception count:" + count); throw t }
     }
}

import funcs._
import Util2._

object BuildAllPlaylist2 extends App {
  
  val s = getSpotify

  // get names - titles from ualt18
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\Res3\""")
  val alt18s = UAlt18AnnualParser.getUAlt18Annual(conn)  // List(Year, List(Artist-SongName)) 
  conn.dispose
  
  val allLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\allsongs2.txt"""
  val allsongsbuff = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(new File(allLoc)), "UTF-8"));
  
  def clean(s:String) = s.replace(160.toChar, 32.toChar).replace('’', ''').replace("-","–").trim.toLowerCase
  val clean2 = (s:String) => s.replace(160.toChar, 32.toChar).replace('’', ''').replace("-","–").trim.toLowerCase
  
  // clean up know equivalent characters and remove duplicates
  val allsongs = alt18s.map(_._2).flatten.map(clean).sorted.distinct
  allsongs.foreach( str => { allsongsbuff.write(str); allsongsbuff.newLine() } )
  allsongsbuff.close
  
  def parse(gg:String) = { 
      val at = gg.split(" – ")
      val artist = at(0)
      val title = if(at.size > 1) { at(1) } else { "" }
      MyTrack(artist.toLowerCase, title)
  }
  
  // parse.....we should throw out un-parseable maybe?
  val parsed = allsongs.map(parse)  
  
  val songFileLoc2 = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\songs2.txt"""
  val songFile2 = new File(songFileLoc2)
  val songCache2 = if ( songFile2.exists ) Some( loadExistingWithArtist(songFileLoc2) ) else None
  
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
    tr.setUri("spotify:track:"+id)
    tr
  }
  
  def findSongOnSpotify2( tr : MyTrack ) : SearchedSong = {
    println("Looking for:" + tr.title + " by " + tr.artist)
    val topTracks  = findASong(tr.artist, tr.title)
    val matchingTracks = topTracks.filter { t => compare2(t.getName,tr.title) }
    if (matchingTracks.size >= 1)
    {
      println("found:" + matchingTracks.head.getName +":"+matchingTracks.head.getArtists.head.getName )
      FoundSong(tr.title, tr.artist, matchingTracks.head) 
    }
    else
      NoSong(tr.title)
  }
  
  def makeOrGet(tr : MyTrack) = {
    def fSOS = findSongOnSpotify2(tr) // closure that will only be called if req
    val mt2 = ( cacheHit : (String,String) ) => FoundSong(tr.title, tr.artist, makeTrack(cacheHit._1, cacheHit._2)) 
    songCache2.flatMap( _.get(tr.title,tr.artist).map(mt2)).getOrElse(fSOS)
  }
  // find in cache or via spotify
  val spotifySongs = parsed.map(makeOrGet)
  
  // report unfound songs
  val (found, notFound) = spotifySongs.partition( x => x match {
    case _ : FoundSong => true
    case _ => false
  })
  
  println("not Found:" + notFound.size)
  notFound.foreach( ss => println(ss.alt18Name))
   
  val step1 = alt18s.map( { case (yr,tracks) => (yr, tracks.map(clean).sorted.distinct) } )
  val step2 = step1.map( { case (yr,tracks) => (yr, tracks.map(parse) ) } )
  val step3 = step2.map( { case (yr,tracks) => (yr, tracks.map(makeOrGet) ) } )
  
  val fInOne = (clean _) andThen parse andThen makeOrGet 
  val step4 = alt18s.map( { case (yr,tracks) => (yr, tracks.map(fInOne).sorted.distinct ) } )
  
  assert(step2.size==step4.size)
  
}