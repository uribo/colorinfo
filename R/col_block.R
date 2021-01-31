#' Display Color Code Blocks
#' @param color color code
#' @param bg background color
#' @examples
#' \dontrun{
#' col_block("#00af00")
#' col_block("#00af00", bg = "white")
#' col_block("#00af00", bg = "black")
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
