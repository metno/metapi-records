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

import models._
import scala.util._
import no.met.data._
import no.met.geometry._


/**
 * Holds query string parameters from the original request for the base endpoint.
 */
case class RecordsQueryParameters(
  sources: Option[String] = None, sourcenames: Option[String] = None, counties: Option[String] = None, municipalities: Option[String] = None,
  elements: Option[String] = None, months: Option[String], fields: Option[String] = None)


/**
  * Interface for overall records access. Implementations of this interface are injected in the controller in either production or
  * development mode.
  */
trait RecordsAccess {

  /**
    * Extracts records based on query parameters.
    */
  def records(qp: RecordsQueryParameters): List[Record]
}
