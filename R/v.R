#' Select administrative boundary vector given a place code
#'
#' Accepts a place code and returns an administrative boundary vector.  This
#' function will service place codes from rnaturalearth place names, Federal
#' Information Processing Standard (FIPS) codes, and hierarchical administrative
#' subdivision codes (HASC).
#'
#' Vector data is downloaded from rnaturalearth (place names and HASC) or tigris
#' (FIPS codes)
#'
#' @param country A country name identified in naturalearth country names
#' @param hasc A HASC
#' @param fips A FIPS code
#' @param cb Logical.  Applies only to FIPS codes.  If true will return a
#'   generalized resolution cartographic boundary vector, otherwise returns a
#'   higher resolution vector.  Default TRUE.
#' @param resolution  The resolution if cb is TRUE.  Applies only to FIPS.
#'   Options are 500k, 5m, and 20m for 1:500k, 1:5 million, and 1:20 million.
#' @param buffer Integer.  If greater than zero will buffer the vector data to
#'   the value of this argument.  Will not work with `returnclass' set to 'sf'.
#' @param returnclass set the class of the returned geometry.  Accepts a value
#'   of either `sf' or `sp'.
#' @param ... Additional arguments, unused.
#' @return A vector for the administrative boundary identified by the location
#'   code
#' @seealso \code{\link[rnaturalearth]{ne_countries}}
#'   \code{\link[rnaturalearth]{ne_states}} \code{\link[tigris]{counties}}
#'   \code{\link[countrycode]{countrycode}}
#' @export
v <- function(country=NULL, hasc=NULL, fips=NULL, cb=TRUE, resolution='20m', buffer=0,
              returnclass='sp', ...) {
    ## if(sum(!sapply(formals()[1:3], is.null))>1) stop('Supply only one of country, fips, or hasc')
    if(!is.null(fips)) {
        if(!is.null(country) && country=='United States of America') {
            stop('FIPS code only available for USA')
        }
        if(!is.null(hasc)) warning('FIPS present, ignoring HASC')
        state <- substr(fips, 1, 2)
        county <- substr(fips, 3, 5)
        counties <- tigris::counties(state=unique(state), cb, resolution, class=returnclass)
        v <- counties[counties $GEOID %in% fips, ]
    } else if(!is.null(hasc)) {
        if(!is.null(country)) warning('HASC present, ignoring country')
        lHasc <- strsplit(hasc, '\\.')
        country <- sapply(lHasc, '[', 1)
        country <- countrycode::countrycode(country, 'iso2c', 'country.name')
        states <- rnaturalearth::ne_states(country=country, returnclass=returnclass)
        v <- states[states $code_hasc %in% hasc, ]
    } else if(!is.null(country)) {
        v <- rnaturalearth::ne_countries(country=country, returnclass=returnclass)
    }
    if(buffer) v <- rgeos::gBuffer(v, buffer)
    v
}
