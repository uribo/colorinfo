#' Color to XY Conversion
#' @inheritParams col_block
#' @examples
#' col2xy("#FF003F")
#' col2xy(rgb(221, 235, 30, maxColorValue = 255))
#' @export
col2xy <- function(color) {
  rgb_values <-
    t(grDevices::col2rgb(color) / 255)
  g_values <-
    sapply(
      rgb_values,
      function(x) {
        ifelse(x > 0.04045, ((x + 0.055) / (1.0 + 0.055))^2.4, x / 12.92)})
  xyz <-
    list(
      X = g_values[1] * 0.664511 + g_values[2] * 0.154324 + g_values[3] * 0.162028,
      Y = g_values[1] * 0.283881 + g_values[2] * 0.668433 + g_values[3] * 0.047685,
      Z = g_values[1] * 0.000088 + g_values[2] * 0.072310 + g_values[3] * 0.986039)
  xy <-
    list(
      x = xyz[["X"]] / (xyz[["X"]] + xyz[["Y"]] + xyz[["Z"]]),
      y = xyz[["Y"]] / (xyz[["X"]] + xyz[["Y"]] + xyz[["Z"]]))
  xy
}
