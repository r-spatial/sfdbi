setOldClass(c("data.frame", "sf"))

#' Driver for SFSQL.
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
setClass("SFSQLDriver", contains = "DBIDriver")
setClass("SFSQL_PGDriver", contains = "DBIDriver")


#' @export
#' @rdname SFSQLDriver-class
setMethod("dbUnloadDriver", "SFSQLDriver", function(drv, ...) {
  TRUE
})



#' SFSQL
#'
#' SFSQL driver
#' https://gdal.org/user/ogr_sql_dialect.html
#' @export
SFSQL <- function() {
  new("SFSQLDriver")
}

#' SFSQL_PG
#'
#' SFSQL_PG driver, a wrapper for SFSQL to construct the full connection string required by
#' GDAL from host,dbname,user,password
#' @export
SFSQL_PG <- function() {
  new("SFSQL_PGDriver")
}

#' SFSQL connection class.
#' @rdname SFSQLConnection-class
#' @export
#' @keywords internal
setClass("SFSQLConnection",
         contains = "DBIConnection",
         slots = list(
           DSN = "character",
           readonly = "logical")
)


#' @rdname SFSQLConnection-class
#' @export
setMethod("show", "SFSQLConnection", function(object) {
  cat("<SFSQLConnection>\n")
  tables <- DBI::dbListTables(object)
  cat("   DSN: ", object@DSN, "\n", sep = "")
  cat("tables: ", paste(tables, collapse = ", "), "\n", sep = "")
})
#' dbConnect
#'
#' dbConnect
#'
#' https://gdal.org/user/ogr_sql_dialect.html
#' @param drv SFSQLDriver created by \code{SFSQL()}
#' @param DSN  data source name, may be a file, or folder path, database connection string, or URL
#' @param readonly open in readonly mode?
#' @export
#' @rdname SFSQL
#' @examples
#' \dontrun{
#' ## a nothing connection not allowed
#' ## ERR: dbConnect(SFSQL())
#' afile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' db <- dbConnect(SFSQL(), afile)
#' dbSendQuery(db, 'SELECT * FROM "nc.gpkg"')
#' }
setMethod("dbConnect", "SFSQLDriver",
          function(drv, DSN = "", readonly = TRUE, ...) {
            ## FIXME: could be a new MEM dataset
            if (nchar(DSN) < 1) stop("DSN must be a valid data source name (file, connection string, url, ...)")
            new("SFSQLConnection", DSN = DSN,  readonly = readonly, ...)
          })


#' dbConnect
#'
#' dbConnect for PostgreSQL via GDAL
#'
#' https://gdal.org/drivers/vector/pg.html
#' @param drv SFSQL_PGDriver created by \code{SFSQL_PG()}
#' @param host database server
#' @param dbname database name
#' @param user user name if needed
#' @param password password if needed
#' @param readonly open in readonly mode?
#' @export
#' @rdname SFSQL_PG
#' @examples
#' \dontrun{
#' db <- dbConnect(SFSQL_PG(), afile)
#' dbSendQuery(db,
#' }
setMethod("dbConnect", "SFSQL_PGDriver",
          function(drv, host = "", dbname = "", user = "", password = "", readonly = TRUE, ...) {
            DSN <- glue::glue("PG:host='{host}' dbname='{dbname}' user='{user}' password='{password}'")
            new("SFSQLConnection", DSN = as.character(DSN),  readonly = readonly, ...)
          })


#' @export
setMethod("show", "SFSQLDriver", function(object) {
  cat("<SFSQLDriver>\n")
})
#' @export
setMethod("dbDisconnect", "SFSQLConnection",
          function(conn, ...) {
            conn@DSN <- ""
            conn
          })


#' SFSQL results class
#'
#' @keywords internal
#' @export
setClass("SFSQLResult",
         contains = "DBIResult",
         slots = c(layer_data = "ANY")
)

#' Send a query to SFSQL.
#'
#' @param conn database connection, s created by \code{\link{dbConnect}}
#' @param statement OGR SQL, see http://www.gdal.org/ogr_sql.html
#' @param ... for compatibility with generic
#' @export
#' @importFrom vapour vapour_read_attributes vapour_read_geometry_text
#' @importFrom wk new_wk_wkb
#' @examples
#' afile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' db <- dbConnect(SFSQL(), afile)
#' dbSendQuery(db, "SELECT * FROM \"nc.gpkg\" WHERE FID < 1")
setMethod("dbSendQuery", "SFSQLConnection",
          function(conn, statement, ...) {
            ## may not be a file
            DSN <- conn@DSN

            ## quiet and fake layer because we aren't using layer  = (it's in the query)
            layer_data <- sf::read_sf(DSN, layer = "<this is unused>", query = statement, quiet = TRUE)
            if (inherits(layer_data, "try-error")) {
              message("executing SQL failed:")
              writeLines(statement)
              if (length(gregexpr("SELECT", statement, ignore.case = TRUE)[[1]]) > 1) {
                stop("perhaps driver in use does not support sub-queries?")
              } else {
                stop("")
              }
            }
            new("SFSQLResult",
                layer_data = layer_data)

          })


#' @export
setMethod("dbClearResult", "SFSQLResult", function(res, ...) {
  ## FIXME maybe a ResetReading here  if we use a pointer not a DSN string?
  TRUE
})
#' @importFrom utils head
#' @export
setMethod("show", "SFSQLResult",
          function(object) {
            cat(sprintf("Field names: %s\n",
                        paste(names(object@layer_data), collapse = ", ")))
            invisible(NULL)
          })
#' Retrieve records from SFSQL query
#' @export
setMethod("dbFetch", "SFSQLResult", function(res, n = -1, ...) {
  res@layer_data
})


#' @export
setMethod("dbHasCompleted", "SFSQLResult", function(res, ...) {
  TRUE
})



#' @export
setMethod("dbReadTable", c(conn = "SFSQLConnection", name = "character"),
          function(conn, name, ...){
            x <- dbSendQuery(conn, sprintf("SELECT * FROM %s", name))
            dbFetch(x)
          })


#' @importFrom vapour vapour_layer_names
#' @export
setMethod("dbListTables", c(conn = "SFSQLConnection"),
          function(conn, ...){
            layers <- sf::st_layers(conn@DSN)
            layers$name
          })

#' @export
setMethod("dbExistsTable", c(conn = "SFSQLConnection"),
          function(conn, name, ...){
            name %in% dbListTables(conn)
          })

#' @export
setMethod("dbDataType", "SFSQLDriver", function(dbObj, obj, ...) {
  ## see "type of the fields" http://www.gdal.org/ogr_sql.html
  if (is.factor(obj)) return("character")
  if (is.data.frame(obj)) return(callNextMethod(dbObj, obj))

  switch(typeof(obj),
         logical = "boolean",
         character = "character",
         double = "numeric",
         integer = "integer",
         list = "character",
         raw = "character",
         blob = "character",
         stop("Unsupported type", call. = FALSE)
  )
}


)
#' @export
setMethod("dbGetInfo", "SFSQLDriver",
          function(dbObj, ...) {
            list(name = "SFSQLDriver",
                 note = "virtual SQL driver for GDAL",
                 driver.version = "0.0.1.9001",
                 client.version = "0.0.1.9001")
          })

