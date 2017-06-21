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

package controllers

import play.api._
import play.api.mvc._
import play.api.http.Status._
import com.github.nscala_time.time.Imports._
import javax.inject.Inject
import io.swagger.annotations._
import scala.language.postfixOps
import util._
import no.met.data._
import models.Record
import services.records._


@Api(value = "records")
class RecordsController @Inject()(recordsAccess: RecordsAccess) extends Controller {

  @ApiOperation(
    value = "Get records.",
    notes = "Get records. To be expanded.",
    response = classOf[models.RecordsResponse],
    httpMethod = "GET")
  @ApiResponses(Array(
    // scalastyle:off magic.number
    new ApiResponse(code = 400, message = "Invalid parameter value or malformed request."),
    new ApiResponse(code = 401, message = "Unauthorized client ID."),
    new ApiResponse(code = 404, message = "No data was found for this combination of query parameters."),
    new ApiResponse(code = 500, message = "Internal server error."))
    // scalastyle:on magic.number
  )
  def getRecords( // scalastyle:ignore public.methods.have.type
    // scalastyle:off line.size.limit
    @ApiParam(value = "The sources to get records for as a comma-separated list of <a href=concepts#searchfilter>search filters</a>. If left out, the output is not filtered on source.")
    sources: Option[String],
    @ApiParam(value = "The source names to get records for as a comma-separated list of <a href=concepts#searchfilter>search filters</a>. If left out, the output is not filtered on source name.")
    sourcenames: Option[String],
    @ApiParam(value = "The counties to get records for as a comma-separated list of <a href=concepts#searchfilter>search filters</a>. If left out, the output is not filtered on county.")
    counties: Option[String],
    @ApiParam(value = "The municipalities to get records for as a comma-separated list of <a href=concepts#searchfilter>search filters</a>. If left out, the output is not filtered on municipality.")
    municipalities: Option[String],
    @ApiParam(value = "The elements to get records for as a comma-separated list of <a href=concepts#searchfilter>search filters</a>. If left out, the output is not filtered on element.")
    elements: Option[String],
    @ApiParam(value = "The months to get records for as a comma-separated list of integers or integer ranges between 1 and 12, e.g. '1,5,8-12'.  If left out, the output is not filtered on month.")
    months: Option[String],
    @ApiParam(value = "The information to return as a comma-separated list of 'sourceid', 'sourcename', 'county', 'municipality', 'elementid', 'month', 'referencetime', or 'value'. For example 'county,month,referencetime1,elementid,value'. If omitted, all fields are returned.")
    fields: Option[String],
    @ApiParam(value = "The output format of the result.",
      allowableValues = "jsonld",
      defaultValue = "jsonld")
    format: String) = no.met.security.AuthorizedAction { implicit request =>
    // scalastyle:on line.size.limit

    val start = DateTime.now(DateTimeZone.UTC) // start the clock

    Try  {
      // ensure that the query string contains supported fields only
      QueryStringUtil.ensureSubset(Set("sources", "sourcenames", "counties", "municipalities", "elements", "months", "fields"), request.queryString.keySet)

      recordsAccess.records(RecordsQueryParameters(sources, sourcenames, counties, municipalities, elements, months, fields))
    } match {
      case Success(data) =>
        if (data isEmpty) {
          Error.error(NOT_FOUND, Option("Could not find records for this combination of query parameters"), None, start)
        } else {
          format.toLowerCase() match {
            case "jsonld" => Ok(new RecordsJsonFormat().format(start, data)) as "application/vnd.no.met.data.records-v0+json"
            case x        => Error.error(BAD_REQUEST, Some(s"Invalid output format: $x"), Some("Supported output formats: jsonld"), start)
          }
        }
      case Failure(x: BadRequestException) =>
        Error.error(BAD_REQUEST, Some(x getLocalizedMessage), x help, start)
      case Failure(x) => {
        //$COVERAGE-OFF$
        Logger.error(x.getLocalizedMessage)
        Error.error(INTERNAL_SERVER_ERROR, Some("An internal error occurred"), None, start)
        //$COVERAGE-ON$
      }
    }
  }

}
