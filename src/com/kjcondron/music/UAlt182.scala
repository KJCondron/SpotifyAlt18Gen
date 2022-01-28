package com.kjcondron.music

import com.wrapper.spotify.SpotifyApi
import com.wrapper.spotify.model_objects.specification.Track
import com.wrapper.spotify.model_objects.specification.PlaylistTrack

import com.wrapper.spotify.exceptions.detailed.BadRequestException
import com.wrapper.spotify.exceptions._

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList

import java.io.File
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream

import com.kjcondron.web.HTTPWrapper
import UAlt18Parser._

import scala.util.Try

object Util2 {
  def backOffLogic[T](g: => T, count: Int = 0): T =
    try { g }
    catch {
      case b: BadRequestException if (b.getMessage == "429" && count < 10) => {
        println("backing off:" + count)
        Thread sleep 4000
        backOffLogic(g, count + 1)
      }
      case b: BadRequestException if (b.getMessage == "404" && count < 10) => {
        println("backing off (404):" + count)
        Thread sleep 4000
        backOffLogic(g, count + 1)
      }
      case b: BadRequestException if (count < 10) => {
        val msg = b.getMessage
        println(s"backing off $msg:" + count)
        Thread sleep 4000
        backOffLogic(g, count + 1)
      }
      case s: SpotifyWebApiException if (count < 10) => {
        println("backing off (ServerErrorException):" + count)
        Thread sleep 4000
        backOffLogic(g, count + 1)
      }
      case t: Throwable => { println("back off caught other exception count:" + count); throw t }
    }
}

object funcs {

  def loadExistingWithArtist(fileLoc: String) = {
    io.Source.fromFile(fileLoc, "utf-8").getLines.map(line => {
      val entry = line.split(":")
      if (entry.size != 4) println(line)
      ((entry(0), entry(1)), (entry(2), entry(3)))
    }).toMap
  }

  def compareScore(str1: String, str2: String) = {
    val lstr1 = str1.toLowerCase
    val lstr2 = str2.toLowerCase
    val ms = lstr2.foldLeft(0)((count, c) => {
      val x: Int = if (lstr1.contains(c)) 1 else 0
      count + x
    })
    ms * 1.0 / lstr2.size
  }

  def compare2(str1: String, str2: String, threshhold: Double = 1.0) = {

    println("Comparing:" + str1 + " and " + str2 + " " + compareScore(str1, str2) + " " + compareScore(str2, str1) + ":" + (compareScore(str1, str2) == 1.0 || compareScore(str2, str1) == 1.0))

    val sFilter = (x: Char) => x > 96 && x < 123 // filter out any non lower case letters
    val myRep = (s: String) => s.replace("&", "and").toLowerCase.filter(sFilter).trim // apply filter after toLower to remove anything outside [a-z]

    val dropParenClause = (s: String) =>
      s.takeWhile(_ != '(').trim.toLowerCase

    myRep(str1) == myRep(str2) ||
      dropParenClause(str1) == dropParenClause(str2) ||
      compareScore(str1, str2) >= threshhold ||
      compareScore(str2, str1) >= threshhold
  }

}

case class MyTrack2(artist : String, title : String)
case class SpotifyTrack2(name : String, id : String)


// if you make this impl GenTraversableOnce then
// you can use coll.flatten to remove NoSong2s from List[SearchedSong2s]
// like you can with Option (clever huh?)
sealed abstract class SearchedSong2 extends Ordered[SearchedSong2] {
  override def toString = alt18Name
  def alt18Name: String
  def isFound: Boolean = false
  def equals(that: Any): Boolean

  def compare(that: SearchedSong2) = alt18Name.compare(that.alt18Name)
}
case class FoundSong2(alt18Name: String, alt18Artist: String, track: Track) extends SearchedSong2 {
  def spotifyName = track.getName
  def spotifyId = track.getId
  def spotifyArtist(api: SpotifyApi) =
    try {
      val tr = Util2.backOffLogic(api.getTrack(spotifyId).build().execute())
      /*val artists = track.getArtists
      println("got artists")
      val optH = artists.headOption
      println("maybe got head")
      val nm = optH.map(_.getName).getOrElse("NO_ARTIST")
      println("got name")*/
      tr.getArtists.head.getName
    } catch {
      case t: Throwable => "ERROR"
    }

  override def isFound = true
  override def equals(that: Any): Boolean =
    that match {
      case fs: FoundSong2 => fs.track.getId == track.getId
      case _ => false
    }
}

case class NoSong2(tr: MyTrack2) extends SearchedSong2 {
 
  override def alt18Name: String = tr.title

  override def equals(that: Any): Boolean =
    that match {
      case ns: NoSong2 => ns.alt18Name == alt18Name
      case _ => false
    }
}

import funcs._
import Util2._

class Spotify(val spfy: SpotifyApi) {

  val uid = spfy.getCurrentUsersProfile.build.execute.getId

  def findSong(
      artist: String,
      title: String,
      mkt: com.neovisionaries.i18n.CountryCode = com.neovisionaries.i18n.CountryCode.US) : List[Track] =
    {
      val str = s"track:$title artist:$artist"
      val items = backOffLogic(spfy.searchTracks(str).market(mkt).build.execute.getItems)
      if (items.size == 0 && (title.contains("(") || artist.contains("(")))
        findSong(artist.takeWhile(_ != '('), title.takeWhile(_ != '('), mkt)
      else
        items.toList
    }

  def findByTitle(
      title: String,
      mkt: com.neovisionaries.i18n.CountryCode = com.neovisionaries.i18n.CountryCode.US) : List[Track] =
    {
      val str = s"track:$title"
      backOffLogic(spfy.searchTracks(str).market(mkt).build.execute.getItems).toList
    }

  def findSpotify(
      tr: MyTrack2,
      mkt: com.neovisionaries.i18n.CountryCode = com.neovisionaries.i18n.CountryCode.US): SearchedSong2 = {
    println("Looking for:" + tr.title + " by " + tr.artist)
    if (tr.artist.size > 0 && tr.title.size > 0) {
      val topTracks = findSong(tr.artist, tr.title, mkt)
      val matchingTracks = topTracks.filter { t => compare2(t.getName, tr.title) }
      if (matchingTracks.size >= 1) {
        println("found:" + matchingTracks.head.getName + ":" + matchingTracks.head.getArtists.head.getName)
        FoundSong2(tr.title, tr.artist, matchingTracks.head)
      } else
        NoSong2(tr)
    } else
      NoSong2(tr)
  }

  def NoSongLookup(nss: List[NoSong2]): Map[String, List[(NoSong2, List[Track])]] = {
    val temp = nss.map(ns => {
      val topTracks = findSong(ns.tr.artist, ns.tr.title)
      if (topTracks.size == 0) {
        val titleOnly = findByTitle(ns.tr.title)
        if (titleOnly.size == 0)
          ("Remove", (ns, List()))
        else
          ("Titles", (ns, titleOnly))
      } else
        ("Spotify Returned", (ns, topTracks))
    })

    val ret = temp.groupBy(f => f._1).mapValues(g => g.map(_._2))
    ret
  }

  def getPLTracks(plid: String, offset: Int = 0, acc: List[PlaylistTrack] = List()): List[PlaylistTrack] =
    {
      val currItemsBld = spfy.getPlaylistsTracks(plid).offset(offset).build
      val currItems = backOffLogic(currItemsBld.execute.getItems)
      if (currItems.size == 0)
        acc
      else
        getPLTracks(plid, offset + 100, acc ++ currItems)
    }
  
  def getOtherPLTracks(plid: String) : List[PlaylistTrack] = 
    {
      val currItemsBld = spfy.getPlaylistsTracks(plid).build
      val currItems : List[PlaylistTrack] = backOffLogic(currItemsBld.execute.getItems).toList
      val currItems2 = if(currItems.size()==100) {
         val currItemsBld2 = spfy.getPlaylistsTracks(plid).offset(100).build
         currItems ++ backOffLogic(currItemsBld2.execute.getItems)
      }
      else
        currItems
        
        currItems2
    }
  
  def getPLName(plid : String) = 
    spfy.getPlaylist(plid).build().execute.getName
  
  def addToPL(plid: String, tracks: Iterable[FoundSong2]): Unit =
    {
      val pltracks = getPLTracks(plid)
      implicit class Wrapper1(_tr: PlaylistTrack) { def matches(_tr2: Track) = _tr.getTrack.getId == _tr2.getId }
      def closure2(_fs: FoundSong2) = pltracks.exists(_.matches(_fs.track))
      val newtracks2 = tracks.filterNot(closure2)
      println("adding " + newtracks2.size + " new tracks out of " + tracks.size)
      newtracks2.foreach {  x => println(x.alt18Artist + ":" + x.alt18Name) }
      val newPLTrackUris2 = newtracks2.map(_.track.getUri).toList
      val distinctUris = newPLTrackUris2.distinct
      addTracks(plid, distinctUris)
    }

  def addTracks(plid: String, tracks: List[String]): Unit =
    if (tracks.size > 0) {
      val maxSize = 50
      if (tracks.size > maxSize)
        addTracks(plid, tracks.drop(maxSize))
      val url = spfy.addTracksToPlaylist(plid, tracks.take(maxSize).toArray).build
      backOffLogic(url.execute)
    }

  def getPL(uuid:String, name: String) = Try({
    println("trying to get:" + name)
    val plpage = backOffLogic(spfy.getListOfUsersPlaylists(uuid).limit(50).build.execute)
    val pls = backOffLogic(plpage.getItems.filter(_.getName == name))
    pls.head
  }).toOption
  
  def getUsrPls(uuid:String) = Try({
    println("trying to get playlists of:" + uuid)
    val plpage = backOffLogic(spfy.getListOfUsersPlaylists(uuid).limit(50).build.execute)
    plpage
  }).toOption

  def getPLId(name: String) = getPL(uid, name).map(_.getId).
    getOrElse({
      println("creating:" + name)
      spfy.createPlaylist(uid, name).build.execute.getId
    })
 
  def makePlaylists(
    tracks: List[(String, List[String])],
    cache: SongCache,
    logLocation: String,
    bigPLName: String,
    plPre: String,
    mkt: com.neovisionaries.i18n.CountryCode = com.neovisionaries.i18n.CountryCode.US) =
    oMakePlaylists(tracks, Some(cache), logLocation, bigPLName, plPre, mkt)

  def oMakePlaylists(
    tracks: List[(String, List[String])],
    cache: Option[SongCache],
    logLocation: String,
    bigPLName: String,
    plPre: String,
    mkt: com.neovisionaries.i18n.CountryCode = com.neovisionaries.i18n.CountryCode.US) = {

    val clean = (s: String) => s.replace(160.toChar, 32.toChar).replace('’', ''').replace("-", "–").trim.toLowerCase

    val allsongsbuff = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(new File(logLocation)), "UTF-8"));
    val allsongs = tracks.map(_._2).flatten.map(clean).sorted.distinct
    allsongs.foreach(str => { allsongsbuff.write(str); allsongsbuff.newLine() })
    allsongsbuff.close

    def parse(gg: String) = {
      val at = gg.split(" – ")
      if (at.size > 1) {
        val artist = at(0)
        val title = at(1)
        Some(MyTrack2(artist.toLowerCase, title))
      } else
        None
    }

    val pParse: PartialFunction[String, MyTrack2] = Function.unlift((parse _))

    // parse.....removing un-parseable
    val parsed = allsongs.collect(pParse)

    def makeTrack(name: String, id: String) = {
      spfy.getTrack(id).build.execute
    }
    
    val makeOrGet = (tr: MyTrack2) => cache.flatMap(_.get(tr)).getOrElse(findSpotify(tr, mkt))
    val f: PartialFunction[SearchedSong2, FoundSong2] = { case fs: FoundSong2 => fs }
    val nf: PartialFunction[SearchedSong2, NoSong2] = { case ns: NoSong2 => ns }

    /// find in cache or via spotify  - As a 'one' liner
    val fInOne = new PartialFunction[String, SearchedSong2] {
      def isDefinedAt(s: String) = pParse.isDefinedAt(clean(s))
      def apply(s: String) = myApply(s)
      val myApply = clean andThen pParse andThen makeOrGet
    }
    val step4 = tracks.map({ case (yr, tracks) => (yr, tracks.collect(fInOne).sorted.distinct.collect(f)) })
    step4.map(x => addToPL(getPLId(plPre + x._1), x._2))
    ///////////////////////

    // find in cache or via spotify - The 'easy'...but this is on the flattened list! so only creates big PL
    val spotifySongs = parsed.map(makeOrGet)
    val foundSongs = spotifySongs.collect(f)
    val notFound = spotifySongs.collect(nf)
    val mainPLId = getPLId(bigPLName)
    addToPL(mainPLId, foundSongs)

    (notFound, foundSongs)
  }
}

object Spotify {

  val bldr = SpotifyApi.builder.setClientId("20212105b1764ecb81a226fca7796b4b")
    .setClientSecret("35a98cd90f5d49dca804b9005d4e67ea")

  def apply(): Spotify = {
    
    println("connecting")

    val _ref = "AQCVtM9Bokw9Mlbq-lfqRtwiejpKcUhhh0QXwC5eelPGt6z07OZyJbrOK4Sb4IwSuCQjfr17JFXVfRBbiUV5x7iWLppZ6e3h59J1UAo5f0ABdObNo30J0UF9tue2vnzoWjM"

    val accLoc = "acc.tmp"
    val accFile = new File(accLoc)

    def useAccess(tok: String) = {
      val _api = SpotifyApi.builder.setAccessToken(tok).build
      val id = _api.getCurrentUsersProfile.build.execute // test it worked
      println("id is " + id)
      _api
    }

    def tryAccess(fn: File): Try[SpotifyApi] = Try(useAccess(io.Source.fromFile(fn, "utf-8").mkString))

    val apix : Option[SpotifyApi] = if (accFile.exists) tryAccess(accFile).toOption
    else {
      println("******NO FILE*********")
      accFile.createNewFile
      None
    }

    def useRefresh() : Option[SpotifyApi] = {
      println("refreshing access token")
      val api = bldr.setRefreshToken(_ref).build
      val auth = api.authorizationCodeRefresh().build();
      val acc2Tk = auth.execute.getAccessToken
      if(acc2Tk != null){
        accFile.delete
        accFile.createNewFile
        val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(accFile), "UTF-8"));
        writer.write(acc2Tk)
        writer.close
        val api : SpotifyApi = SpotifyApi.builder.setAccessToken(acc2Tk).build
        Some(api)
      }
      else
        None
    }
    
    // use this to get a code and update auth2
    def useAuth() : SpotifyApi = {
      println("using auth")
      val api = bldr.setRedirectUri(new java.net.URI("http://localhost:8888/callback")).build()
      val uri = api.authorizationCodeUri.build.execute
      println(uri)
      Thread.sleep(5*60*1000)
      api
    }
    
    def useAuth2 = {
      val code = "AQDyox9snYMS_ktOplQBKyTOAHKEfgVVDXkD1aTlOPs1kGfHBTYhvIriN0CUJ3Vyi25KNoQUxCysXnXW713sTZ99cOWIBWS-r95b3Vu2QMWnCuS4mJ4Rv5wGGpUV3wE9Ypw0tIeedj3hx0z1lGHaf8LiO7-sRmIMEM9aiU1grJ23Cg"
      
      val api = bldr.setRedirectUri(new java.net.URI("http://localhost:8888/callback")).build()
      
      val auth = api.authorizationCode(code).build.execute
      
      api.setAccessToken(auth.getAccessToken());
      api.setRefreshToken(auth.getRefreshToken());
      
      println("acc:"+auth.getAccessToken())
      println("ref:"+auth.getRefreshToken())
    
     api
    }
    
    val api2 = apix.getOrElse(useRefresh.getOrElse(useAuth2)) // useAuth to get login URL, login, get code from redirect update getAuth2 and run again
    new Spotify(api2)

  }
}

object SongCache {

  def apply(spfy:SpotifyApi, oMap: Option[Map[(String, String), (String, String)]]): SongCache = new SongCache(spfy, oMap)
  def apply(spfy:SpotifyApi, loc: String): SongCache = apply(spfy, load(loc))

  private def load(fileLoc: String) = {
    val songFile = new File(fileLoc)
    val oFile = if (songFile.exists) Some(fileLoc) else None
    oFile.map(f => io.Source.fromFile(f, "utf-8").getLines.map(line => {
      val entry = line.split(":")
      if (entry.size != 4) println(line)
      ((entry(0), entry(1)), (entry(2), entry(3)))
    }).toMap)
  }

  def outputCache(loc: String, fss: List[FoundSong2]) =
    {
      val cacheFile = new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(loc), "UTF-8"));

      fss.foreach(
        fs => {
          cacheFile.write(fs.alt18Name + ":" + fs.alt18Artist + ":" + fs.spotifyName + ":" + fs.spotifyId)
          cacheFile.newLine
        })
      cacheFile.close
    }
}

class SongCache private (private val spfy:SpotifyApi, private val _theMap: Option[Map[(String, String), (String, String)]]) {
  def getOrElse(key: MyTrack2)(f: => SearchedSong2) =
    get(key).getOrElse(f)

  def get(key: MyTrack2): Option[FoundSong2] =
    _theMap.flatMap(_.get(key.title, key.artist)).map(
      x => FoundSong2(key.title, key.artist, makeTrack(x._1, x._2)))

  // maybe one day we can use this
  def update(key: (String, String), value: (String, String)) = {
    val currMap: Map[(String, String), (String, String)] = _theMap.getOrElse(Map())
    val newMap = currMap + (key -> value)
    SongCache(spfy, Some(newMap))
  }
  
  private def makeTrack(name: String, id: String) = {
    spfy.getTrack(id).build.execute
  }

}

import Util2._

object BuildAllPlaylist extends App {

  val spotify = Spotify()

  // get names - titles from ualt18
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\Res3\""")
  val alt18s = UAlt18AnnualParser.getUAlt18Annual(conn) // List(Year, List(Artist-SongName)) 
  conn.dispose

  val cacheLoc = """C:\Users\KJCon\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\songs2.txt"""
  val logLoc = """C:\Users\KJCon\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\allsongs2.log"""

  val cache = SongCache(spotify.spfy, cacheLoc)
  val (nf, songs) = spotify.makePlaylists(alt18s, cache, logLoc, "all18V2", "alt18", com.neovisionaries.i18n.CountryCode.US)

  val newCacheLoc = """C:\Users\KJCon\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\songs2.txt"""
  SongCache.outputCache(newCacheLoc, songs)

}

object MyPlaylistsFromUnofficalPlaylist extends App {

  val spotify = Spotify()
  
  val uuid = "tyler_v2"
  val plids = List("0wBkw63jf90ZoQIHz759No","7HPqQZHYXLlFnu6ssfZfga","3oQSkHECYyWQZNpQIoxaNV")
      
  // get tracks
  plids.map( plid => {
  val tracks = spotify.getOtherPLTracks(plid)
  
  // get year for this playlist
  val plName = spotify.getPLName(plid)
  val idx = plName.indexOf("20")
  val year = plName.substring(idx,idx+4)
  
  println(year)
  
  // convert to "foundsong type", for use with generic addToPL function
  val fs = tracks.map( 
      plt => FoundSong2(
          plt.getTrack.getName,
          plt.getTrack.getArtists.head.getName,
          plt.getTrack))
  
  // add to all and annual playlist
  spotify.addToPL(spotify.getPLId("all18V2"), fs)
  spotify.addToPL(spotify.getPLId("alt18"++year), fs)
  })
}

object BuildShinePlaylist extends App {

  val spotify = Spotify()

  val fn = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\ALL36,TXT.txt"""

  val as = UAlt18AnnualParser.fromFile(fn)

  def split(xs: List[String]): List[List[String]] = {
    val first = xs.takeWhile(_.trim.size > 0)
    val rest = xs.dropWhile(_.trim.size > 0).drop(1)
    if (rest.size == 0)
      List(first)
    else
      List(first) ::: split(rest)
  }

  val shines = split(as).map(s => (s.head, s.tail.collect({ case s: String if (s != "CD1" && s != "CD2") => s })))

  val songFileLoc2 = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\shineCache.txt"""
  val logLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\shineLog.txt"""

  val cache = SongCache(spotify.spfy, songFileLoc2)
  val (nf, songs) = spotify.makePlaylists(shines, cache, logLoc, "Alt362", "")
  println("Found: " + songs.size + " songs")
  val newCacheLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\shineCache.txt"""
  SongCache.outputCache(newCacheLoc, songs)

  println("NotFound In US: " + nf.size + " songs")

  // now try "GB" market....when everything is in the cache this is a bit of a waste
  val (nf2, songs2) = spotify.makePlaylists(shines, cache, logLoc, "Alt362", "", com.neovisionaries.i18n.CountryCode.GB)

  println("NotFound: " + nf2.size + " songs")

  spotify.NoSongLookup(nf2).foreach({
    case (desc, nss) => {
      println(desc + ":" + nss.size)
      nss.foreach(f => {
        println(f._1.tr.title + " by " + f._1.tr.artist)
        //f._2.foreach( tr => println(tr.getName + ":" + tr.getArtists.head.getName))
      })
    }
  })

}

object BuildBillboardPlaylist extends App {

  val spotify = Spotify()

  val fn = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\GreatestAdultPopSongs.txt"""

  val as = UAlt18AnnualParser.fromFile(fn)

  val songFileLoc2 = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\bbCache.txt"""
  val logLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\bbLog.txt"""

  val cache = SongCache(spotify.spfy, songFileLoc2)
  val (nf, songs) = spotify.makePlaylists(List(("BBAdultPop", as)), cache, logLoc, "BBAdultPop", "")
  println("Found: " + songs.size + " songs")
  val newCacheLoc = """C:\Users\Karl\Documents\GitHub\SpotifyAlt18Gen\src\com\kjcondron\music\bbCache.txt"""
  SongCache.outputCache(newCacheLoc, songs)

  println("NotFound: " + nf.size + " songs")
  nf.foreach(println)

}

object TestApp extends App {

  val spotify = Spotify()
  
  val songs = spotify.findSong("Mika", "Grace Kelly")
  
  println(songs.size())
  
  val opls = spotify.getUsrPls("tyler_v2")
  
  opls.map( pls=> pls.getItems.map(pl=>println(pl.getName)) )

}

