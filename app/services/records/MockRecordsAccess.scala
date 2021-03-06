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
import no.met.data._
import scala.util._


/**
  * Mocked records access.
  */
class MockRecordsAccess extends RecordsAccess {

  // scalastyle:off magic.number

  def records(qp: RecordsQueryParameters): List[Record] = {
    List(new Record(Some("SN9600"), Some("TYNSET"), Some("Hedmark"), Some("Tynset"), Some("min(air_temperature P1D)"), Some(2), Some("1912-02-01"),
      Some(-46.6)))
  }

  // scalastyle:on magic.number
}
