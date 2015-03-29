/**
 * Created by gmgilmore on 3/16/15.
 */

import org.scalajs.dom
import dom.html
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js
import org.scalajs.jquery._
import scala.scalajs.js.JSApp
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import dom.ext._
import scala.scalajs
  .concurrent
  .JSExecutionContext
  .Implicits
  .runNow


/**
 *
 * @param urlPath
 * @param port
 * @param uniqueID
 * @param hasError
 */
case class StreamInformation(urlPath:String, port: Int, uniqueID:Long, hasError:Boolean)

case class TableDifferences(changedError:Set[StreamInformation], newEntries:Set[StreamInformation])

case class TableInfo(entries: Set[StreamInformation]){
  def diffOtherTableInfo(that:TableInfo): TableDifferences = {
    val thisMap:Map[Long, StreamInformation] =
      entries.foldLeft(Map.empty[Long, StreamInformation])((m,s) => m + (s.uniqueID -> s))

    val thatMap:Map[Long, StreamInformation] =
      that.entries.foldLeft(Map.empty[Long, StreamInformation])((m,s) => m + (s.uniqueID -> s))

    val newEntries:Set[StreamInformation] =
      thatMap.keySet.diff(thisMap.keySet).foldLeft(Set.empty[StreamInformation])((s,n) => s + thatMap(n))

    val changedError:Set[StreamInformation] = {
      val newKeys:Set[Long] = newEntries.foldLeft(Set.empty[Long])((s,n) => s + n.uniqueID) //new processes that were added

      //get rid of the new entries from "thatMap"
      val sameEntriesSet = thatMap.keySet.diff(newKeys).foldLeft(Set.empty[StreamInformation])((s,k) => s + thatMap(k))

      //see which entries have different isError values
      sameEntriesSet.diff(thisMap.values.toSet)
    }

    //see the streams that were deleted (by the clear button) since the table was last rendered.
    val removedEntries:Set[StreamInformation] = {
      (thisMap.keySet diff thatMap.keySet).foldLeft(Set[StreamInformation]())((s, k) => s + thisMap(k))
    }
    TableDifferences(changedError union removedEntries, newEntries)
  }
}

@JSExport
object main {
  @JSExport
  def main(tableDivName:String ="streamTableLocation", tableBodyName:String = "streamTableBody", tableName:String = "streamTable", streamTableJsonPath:String = "/urls") ={

    /**
     * Should be called only once, initially adds the table (with id = to the tableName parameter) as a child of the
     * div (with id = to the tableDivName parameter).
     *
     */
    def initializeTable = {
      //add table element
      val div = dom.document.getElementById(tableDivName).asInstanceOf[html.Div]
      div.appendChild(table(id:=tableName).render)
      val myTable = dom.document.getElementById(tableName).asInstanceOf[html.Table]
      //create and configure header row
      val header = myTable.createTHead.asInstanceOf[html.TableSection]
      val headerRow = header.insertRow(0).asInstanceOf[html.TableRow]
      val streamURL = headerRow.insertCell(0)
      streamURL.innerHTML = "STREAM URL"
      val portNumber = headerRow.insertCell(1)
      portNumber.innerHTML = "PORT #"
      val tableBody = myTable.createTBody.asInstanceOf[html.TableSection]
      tableBody.id = tableBodyName

      myTable.align = "center"
      getJSONAndRunCallback(renderTable, tableBody, div)
      div.appendChild(myTable)

      println("table initialized")
    }

    def buildTableInfoFromJSON(json:js.Array[js.Dictionary[js.Dynamic]]):TableInfo = {
      var streamInfoSet:Set[StreamInformation] = Set()
      json.foreach(stream => {
        val (urlPath, port, uniqueID, hasError) = (stream("urlPath").toString, stream("port").toString.toInt,
          stream("id").toString.toLong, stream("errorOrEnded").toString.toBoolean)
        streamInfoSet += StreamInformation(urlPath, port, uniqueID, hasError)
      })
      println("buildTableInfoFromJSON: " + streamInfoSet)
      TableInfo(streamInfoSet)
    }

    def buildTableInfoFromCurrentTable:TableInfo = {
      val myTable = dom.document.getElementById(tableName).asInstanceOf[html.Table]
      var streamInfoSet:Set[StreamInformation] = Set()
      myTable.rows.drop(1).foreach(elm => {
        val row = elm.asInstanceOf[html.TableRow]
        val (urlPath:String, port:Int) = (row.cells(0).innerHTML, row.cells(1).innerHTML.toInt)
        val uniqueID:Long = row.getAttribute("id").toLong
        val hasError:Boolean = (row.getAttribute("class") == "invalid")
        streamInfoSet += StreamInformation(urlPath,port, uniqueID, hasError)
      })
      println("buildTableInfoFromCurrentTable: " + streamInfoSet)
      TableInfo(streamInfoSet)
    }


    /**
     * Gets the process list JSON from the server and then runs the callback using the information contained in the JSON.
     * @param callback the function to run when the JSON is successfully acquired
     * @param tableBody the tableBody that is manipulated using the callback
     * @param div the div that is manipuated using the callback
     */
    def getJSONAndRunCallback(callback: (js.Array[js.Dictionary[js.Dynamic]], html.TableSection) => Unit, tableBody: html.TableSection, div:html.Div) ={
      Ajax.get(streamTableJsonPath).onSuccess{case xhr=>{
        js.JSON.parse(xhr.responseText) match {
          case processList:js.Array[js.Dictionary[js.Dynamic]] =>{
            callback(processList, tableBody)
          }
          case _ => p("SOMEONE SCREWED UP BIG TIME!!!")
        }
      }
      }
    }


    /**
     * Updates the table with the new information from the JSON with the stream information.
     */
    def refreshTable = {
      println("refreshing table")
      val myTableBody = dom.document.getElementById(tableBodyName).asInstanceOf[html.TableSection]
      val div = dom.document.getElementById(tableDivName).asInstanceOf[html.Div]
      getJSONAndRunCallback(renderTable, myTableBody, div)
    }

    /**
     *
     * @param streamTable The JSON array containing the sequence of stream information. If there have been any new
     *                    streams added (and that is reflected in the JSON), the table will have a new row containing
     *                    the information associated with the new stream. If a stream was removed, the corresponding row
     *                    will turn red and be be crossed out in the table.
     * @param tableBody The tableBody that we are adding rows to or modifying some of the rows on
     */
    def renderTable(streamTable:js.Array[js.Dictionary[js.Dynamic]], tableBody: html.TableSection) = {

      val differences:TableDifferences =
        buildTableInfoFromCurrentTable.diffOtherTableInfo(buildTableInfoFromJSON(streamTable))

      println("differences: " + differences)

      differences.changedError.foreach(info =>{
        val row = dom.document.getElementById(info.uniqueID.toString).asInstanceOf[html.TableRow]
        row.setAttribute("class", "invalid")
      })

      differences.newEntries.foreach(info =>{
        val row = tableBody.insertRow(0).asInstanceOf[html.TableRow]
        renderRow(info, row)
      })

    }

    /**
     * Adds the url and port cells to a single table row
     * @param streamInformation the information (from the JSON) associated with the stream that we are adding.
     * @param row the row that we are adding the cells to
     */
    def renderRow(streamInformation:StreamInformation, row: html.TableRow) = {
//      println("Did I manage to render the row?")
//      println(miniDict.keys)
//      miniDict.values.foreach(x => println(x))


      val (url, port) = (streamInformation.urlPath, streamInformation.port)
      row.insertCell(0).innerHTML =  url
      row.insertCell(1).innerHTML =  port.toString
      if (streamInformation.hasError) row.setAttribute("class", "invalid") else row.setAttribute("class", "valid")
      row.id = streamInformation.uniqueID.toString
    }

    initializeTable
    dom.setInterval(()=>refreshTable, 2000)

  }
}
