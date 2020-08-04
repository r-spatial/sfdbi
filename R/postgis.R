#' Postgis driver
#' @param con a connection to a database
#' @param ... unused
#' @export
postgis <- function(con, ...) {
  x <- append_class(con, "postgis_connection")
  check_spatial_connection(x)
}

unpostgis <- function(con, ...) {
  remove_class(con, "postgis_connection")
}

#' @export
#' @importFrom dplyr tbl
copy_to.postgis_connection <- function(dest, df, name = deparse(substitute(df)), ...)  {
  x <- remove_class(con, "postgis_connection")
  sf::st_write(df, x, layer = name, ...)
  df <- dplyr::tbl(x, name)
  append_class(df, "tbl_sf")
}

#' @export
#' @importFrom dplyr tbl
tbl.postgis_connection <- function(src, ...) {
  NextMethod()
}

#' @export
collect.tbl_sf <- function(x, ...) {
  y <- NextMethod()
  to_tbl_sf(y, conn = x[["src"]][["con"]])
}

to_tbl_sf <- function(x, classes = c("pq_geometry"), EWKB = TRUE, conn = dsn) {
  # TODO: We should probably catch the class sooner than in`tbl()`, so we know
  # what connection was used, and which class to look for. See
  # `sf:::is_geometry_column`. Also the `tbl` object passed to `collect()` contains
  # useful information.

  # Copied from sf
  .is_geometry_column <- sf:::is_geometry_column.PqConnection
  geometry_column <- .is_geometry_column(con, x)
  x[geometry_column] <- lapply(x[geometry_column], sf:::try_postgis_as_sfc, EWKB = EWKB, conn = dsn)
  return(x)
}

append_class <- function(x, new_class) {
  structure(x, class = c(new_class, class(x)))
}

remove_class <- function(x, rm_class) {
  structure(x, class = setdiff(class(x), rm_class))
}

check_spatial_connection <- function(x, ...) {
  UseMethod("check_spatial_connection", x)
}

check_spatial_connection.default <- function(x) {
  return(x)
}

check_spatial_connection.postgis <- function(x) {
  version <- DBI::dbGetQuery(x, "SELECT postgis_version()")
}
