#' Display Color Code Blocks
#' @description Outputs a color close to the given color code on
#' the command line.
#' @param color color code
#' @param bg background color
#' @examples
#' \dontrun{
#' col_block("#00af00")
#' col_block("#00af00", bg = "white")
#' col_block(rgb(221, 235, 30, maxColorValue = 255), bg = "black")
#' }
#' @export
col_block <- function(color, bg = NULL) {
  purrr::walk(
    color,
    function(.x) {
      .cb <- crayon::make_style(.x)
      if (!is.null(bg)) {
        .bg <- crayon::make_style(bg, bg = TRUE)
        .cb <- crayon::combine_styles(.cb, .bg)
      }
      res <-
        .cb(glue::glue("{c} {.x}",
                       c = cli::symbol$square))
      #if (crayon::has_hyperlink() == TRUE) {
      #  cat(crayon::hyperlink(res, ""))
      #} else {
      cat(res)
      #}
    }
  )
}
