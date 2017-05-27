/*
    MET-API

    Copyright (C) 2014 met.no
    Contact information:
    Norwegian Meteorological Institute
    Box 43 Blindern
    0313 OSLO
    NORWAY
    E-mail: met-api@met.no

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301, USA
*/

package services.records

import scala.language.postfixOps
import play.api.Play.current
import play.api.db._
import anorm._
import anorm.SqlParser._
import util.{ Try, Success, Failure }
import models._
import no.met.data._

import play.Logger


//$COVERAGE-OFF$ Not testing database queries

/**
  * Overall records access from station sources.
  */
class StationRecordsAccess extends ProdRecordsAccess {

  // supported element IDs with corresponding legacy codes (### hard-coded for now)
  private val elemMap: Map[String, String] = Map(
    "min(air_temperature P1D)" -> "TAN",
    "max(air_temperature P1D)" -> "TAX"
  )
  private val invElemMap = elemMap.map(_.swap) // ### WARNING: This assumes that elemMap is a one-to-one relation
  private def fromLegacyElem(legacyElem: String) = invElemMap(legacyElem)

  // Returns true iff words is None or w ("" if None) matches any word in the comma-separated list words. The matching is case-insensitive and a word in
  // words is allowed to contain asterisks to represent zero or more characters.
  private def matchesWords(w: Option[String], words: Option[String]) = {
    words match {
      case Some(wlist) => {
        val w1 = w.getOrElse("").trim.toLowerCase
        wlist.split(",").toSet.exists((w2: String) => { w1.matches(w2.trim.toLowerCase.replace("*", ".*")) } )
      }
      case None => true
    }
  }

  // Returns a sorted list of integers from an input string of the form "1,3,5-7" (i.e. a comma-separated list of integers or integer ranges).
  private def getIntList(os: Option[String], min: Int, max: Int, itype: String): List[Int] = {

    def toIntSet(s: String): Set[Int] = {
      val psingle = "([0-9]+)".r
      val prange = "([0-9]+)-([0-9]+)".r
      s.trim match {
        case prange(a,b) => (a.toInt to b.toInt).toSet
        case psingle(a) => Set[Int](a.toInt)
        case _ => throw new BadRequestException(s"$itype: syntax error: $s (not of the form <int> or <int1>-<int2>)")
      }
    }

    os match {
      case Some(s) => {
        val list = s.split(",").foldLeft(Set[Int]()) { (acc, cur) => acc ++ toIntSet(cur) }.toList.sorted
        if (list.nonEmpty && ((list.head < min) || (list.last > max))) {
          throw new BadRequestException(s"$itype outside valid range [$min, $max]")
        }
        list
      }
      case None => List[Int]()
    }
  }

  private val parser: RowParser[Record] = {
    get[String]("sourceid") ~
      get[String]("sourcename") ~
      get[String]("county") ~
      get[String]("municipality") ~
      get[String]("elementId") ~
      get[Int]("month") ~
      get[String]("date1") ~
      get[Option[String]]("date2") ~
      get[Double]("record") map {
      case sourceid~sourcename~county~municipality~elementid~month~date1~date2~record
      => Record(
        Some(sourceid),
        Some(sourcename),
        Some(county),
        Some(municipality),
        Some(fromLegacyElem(elementid)),
        Some(month),
        Some(date1),
        date2,
        Some(record)
      )
    }
  }

  private def getMainQuery = {
    s"""
       |SELECT
       |  'SN' || stnr AS sourceid,
       |  name AS sourcename,
       |  county,
       |  municipality,
       |  elem_code AS elementid,
       |  to_char(dato_d, 'MM')::int AS month,
       |  to_char(dato_d, 'YYYY-MM-DD') as date1,
       |  to_char(dato_m, 'YYYY-MM-DD') as date2,
       |  record
       |FROM t_records
      """.stripMargin
  }


  // scalastyle:off cyclomatic.complexity
  override def records(qp: RecordsQueryParameters): List[Record] = {

    val fields :Set[String] = FieldSpecification.parse(qp.fields)
    val suppFields = Set("sourceid", "sourcename", "county", "municipality", "elementid", "month", "date1", "date2", "record")
    fields.foreach(f => if (!suppFields.contains(f)) {
      throw new BadRequestException(s"Unsupported field: $f", Some(s"Supported fields: ${suppFields.mkString(", ")}"))
    })

    val recs = DB.withConnection("kdvh") { implicit connection =>
      val query = getMainQuery
      //        Logger.debug(query)
      SQL(query).as(parser *)
    }

    // scalastyle:off magic.number
    val mlist = getIntList(qp.months, 1, 12, "months")
    // scalastyle:on magic.number
    val omitSourceId     = fields.nonEmpty && !fields.contains("sourceid")
    val omitSourceName   = fields.nonEmpty && !fields.contains("sourcename")
    val omitCounty       = fields.nonEmpty && !fields.contains("county")
    val omitMunicipality = fields.nonEmpty && !fields.contains("municipality")
    val omitElementId    = fields.nonEmpty && !fields.contains("elementid")
    val omitMonth        = fields.nonEmpty && !fields.contains("month")
    val omitDate1        = fields.nonEmpty && !fields.contains("date1")
    val omitDate2        = fields.nonEmpty && !fields.contains("date2")
    val omitRecord       = fields.nonEmpty && !fields.contains("record")
    recs
      .filter(r => matchesWords(r.sourceId, qp.sources))
      .filter(r => matchesWords(r.sourceName, qp.sourcenames))
      .filter(r => matchesWords(r.county, qp.counties))
      .filter(r => matchesWords(r.municipality, qp.municipalities))
      .filter(r => matchesWords(r.elementId, qp.elements))
      .filter(r => mlist.isEmpty || mlist.contains(r.month.get))
      .map(r => r.copy(
        sourceId = if (omitSourceId) None else r.sourceId,
        sourceName = if (omitSourceId) None else r.sourceName,
        county = if (omitCounty) None else r.county,
        municipality = if (omitMunicipality) None else r.municipality,
        elementId = if (omitElementId) None else r.elementId,
        month = if (omitMonth) None else r.month,
        date1 = if (omitDate1) None else r.date1,
        date2 = if (omitDate2) None else r.date2,
        record = if (omitRecord) None else r.record
      )).distinct.sortBy(r => (r.elementId, r.county, r.month, -Math.abs(r.record.get)))

  }
  // scalastyle:on cyclomatic.complexity
}

//$COVERAGE-ON$
