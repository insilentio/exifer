#' Prepare originals for export
#'
#' @description Performs necessary steps on metadata side for new pictures
#' in Import folder after Development work in ON1 and before Export
#'
#' 1. copy xmp metadata information onto original file
#' 2. remove unnecessary side files
#' 3. flatten hierarchical keywords for writing into keywords and subject tag
#' 4. amend lens information for Nikon in order to have consistent metadata
#' 5. amend lens information for Apple in order to have prime/zoom lens info consistent
#' 6. ensures completeness of location tags information
#' 7. harmonize createtime and datetimeoriginal information
#'
#' @param imp_path current import path to work on. The number of subordinate directory levels is indicated
#' by the parameter 'level_below'.
#' @param level_below indicates to which directory level relative to imp_path it is looking for xmp files and
#' therefore may need to be changed (e.g. on year level -> 2, on album level -> 3)
#' @param offset the number of hours of offset of the timezone the photos were taken in compared to UTC.
#' 1 for MEZ, 2 for MESZ
#'
#' @returns nothing
#' @export
prepare_export <- function(
  imp_path = "/Volumes/NoBackup/Bilder/Import/2025/",
  level_below = "1",
  offset
) {
  # Write XMP into original files and cleanup ----------------------------------------------------
  # (this needs to be done because ON1 writes raw file metadata mostly into xmp)
  exiftoolr::exif_call(
    args = c(
      "-r",
      "-ext",
      "nef",
      "-ext",
      "cr2",
      "-ext",
      "arw",
      "-tagsfromfile",
      paste0(imp_path, "/%-", level_below, ":d%f.xmp")
    ),
    path = imp_path
  )
  system(paste0('find "', imp_path, '" -name "*xmp" -exec rm {} \\;'))
  exiftoolr::exif_call(args = c("-r", "-delete_original!"), path = imp_path)

  # generate file list and run metadata functions ---------------------------

  # get complete file list
  imported <- list.files(imp_path, recursive = TRUE, full.names = TRUE)

  # the metadata functions can be run without writing the metadata already. This is better for performance,
  # because we can execute the time consuming exiftool command only once.

  # flatten hierarchical keywords
  fs <- flatten_subject(imported, csv_execute = FALSE)

  # lens information
  hl <- harmonize_lensinfo(imported, csv_execute = FALSE)

  # complete the information in the various location tags
  cl <- complete_location(imported, csv_execute = FALSE)

  # harmonize time information
  ht <- harmonize_time(imported, csv_execute = FALSE, offset = offset)

  modify <- fs |>
    dplyr::full_join(hl, by = dplyr::join_by("SourceFile")) |>
    dplyr::full_join(cl, by = dplyr::join_by("SourceFile")) |>
    dplyr::full_join(ht, by = dplyr::join_by("SourceFile"))

  handle_return(
    modify,
    csv_execute = TRUE,
    paths = imported,
    csv_path = "~/Pictures/exifer.csv",
    delete_original = TRUE,
    with_sep = ", "
  )
}
