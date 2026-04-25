#' Save a gt table as a PNG using ragg (no webshot/chrome required)
#'
#' @description Renders a `gt` table to PNG using the `ragg` graphics device,
#'   avoiding the need for `webshot2`/`chromote`. This is the preferred method
#'   for saving `gt` tables on all platforms, including Raspberry Pi.
#'
#' @param gt_table A `gt_tbl` object to save
#' @param filename Output PNG file path
#' @param width Width in pixels (default 1400)
#' @param height Height in pixels (default 800)
#' @param scale Scaling factor (default 1)
#'
#' @return The filename (invisibly)
#' @export
save_gt_as_png_ragg <- function(
  gt_table,
  filename,
  width = 1400,
  height = 800,
  scale = 1
) {
  if (!requireNamespace("ragg", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ragg} is required for PNG export.",
        "i" = "Install it with {.code install.packages('ragg')}."
      )
    )
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg gt} is required for this function.",
        "i" = "Install it with {.code install.packages('gt')}."
      )
    )
  }
  gt_grob <- gt::as_gtable(gt_table)
  ragg::agg_png(
    filename,
    width = width,
    height = height,
    units = "px",
    scaling = scale
  )
  grid::grid.draw(gt_grob)
  grDevices::dev.off()
  invisible(filename)
}
