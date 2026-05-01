#' Save a gt table as a PNG using ragg (no webshot/chrome required)
#'
#' @description Renders a `gt` table to PNG using the `ragg` graphics device,
#'   avoiding the need for `webshot2`/`chromote`. This is the preferred method
#'   for saving `gt` tables on all platforms, including Raspberry Pi.
#'
#'   When `width` or `height` is `NULL` (the default), the optimal canvas size
#'   is determined automatically from the table's natural dimensions, avoiding
#'   excessive margins around the content.
#'
#' @param gt_table A `gt_tbl` object to save
#' @param filename Output PNG file path
#' @param width Width in pixels. If `NULL` (default), auto-detected from the
#'   table's natural width.
#' @param height Height in pixels. If `NULL` (default), auto-detected from the
#'   table's natural height.
#' @param dpi Resolution in dots per inch used for auto-detection and rendering
#'   (default 150).
#' @param scale Scaling factor passed to `ragg::agg_png()` (default 1).
#' @param padding Extra pixels added to each auto-detected dimension for
#'   breathing room (default 20). Ignored when `width`/`height` are supplied
#'   explicitly.
#'
#' @return The filename (invisibly)
#' @export
save_gt_as_png <- function(
  gt_table,
  filename,
  width = NULL,
  height = NULL,
  dpi = 150,
  scale = 1,
  padding = 20
) {
  now <- Sys.time()
  tryCatch(
    gt::gtsave(gt_table, filename),
    error = function(e) {
      cli::cli_alert_danger("Couldn't save file with `gtsave`.")
    }
  )

  if (file.exists(filename) && file.info(filename)$mtime >= now) {
    return(invisible(filename))
  }

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
  if (is.null(width) || is.null(height)) {
    # Open a null PDF device so grid unit conversion has a valid device context.
    # The null units in the gtable (flexible spacers) resolve to near-zero in
    # this context, leaving us with the minimum content-only dimensions.
    grDevices::pdf(nullfile())
    pdf_dev <- grDevices::dev.cur()
    tryCatch(
      {
        if (is.null(width)) {
          width <- ceiling(
            grid::convertWidth(
              sum(gt_grob$widths),
              "inches",
              valueOnly = TRUE
            ) *
              dpi
          ) +
            padding
        }
        if (is.null(height)) {
          height <- ceiling(
            grid::convertHeight(
              sum(gt_grob$heights),
              "inches",
              valueOnly = TRUE
            ) *
              dpi
          ) +
            padding
        }
      },
      finally = grDevices::dev.off(pdf_dev)
    )
  }
  ragg::agg_png(
    filename,
    width = width,
    height = height,
    units = "px",
    res = dpi,
    scaling = scale
  )
  grid::grid.draw(gt_grob)
  grDevices::dev.off()
  invisible(filename)
}
