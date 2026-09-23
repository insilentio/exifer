#' Flatten hierarchical keywords
#'
#' @description Write flat keywords and subject tags derived from hierarchical ones (tag XMP:HierarchicalSubject)
#' into photo metadata. Only writes the lowest level keywords back to the pictures. Hierarchical keyword tag is not modified.
#' Also deals with rating: aligns EXIF and XMP values and sets an additional keyword for Apples Photo App;
#' in order to deal with the non-existing ratings there - but only for export photos.
#' (There is no metadata flag for Apples favourites -> must be set manually based on this keyword).
#' It uses per default the csv option of exifool to handle different values for different files,
#' which is much faster than a for loop which calls exiftool every time.
#' Exiftool per default creates copies of the original files (*_original); this behaviour can be modified by parameter.
#'
#' @param paths List of photos to be modified. Needs a character vector with full file names or a directory name. In the latter case, you need to set `recursive` accordingly.
#' @param csv_execute logical, to determine whether modification should be directly written or returned as tibble
#' @param csv_path path and file name of the output csv file
#' @param delete_original whether the original copies of the photos should be deleted afterwards or not
#' @param recursive tells exiftool whether to read files recursively or not. Choose `TRUE` if you use a directory path instead of a list of files for the `paths` parameter.
#'
#' @returns depending on param csv_execute:
#' - if TRUE,  writes the tag values as csv file and writes them via exiftool to pictures
#' - if FALSE, returns the tags as tibble
#' @export
flatten_subject <- function(
  paths,
  csv_execute = TRUE,
  csv_path = '~/Pictures/subjects.csv',
  delete_original = FALSE,
  recursive = FALSE
) {
  args <- c("-G", "-s", "-hierarchicalsubject", "-rating")

  subjects <- exiftoolr::exif_read(
    args = args,
    path = paths,
    recursive = recursive
  )
  
  if (recursive)
    paths <- list.files(paths,
               recursive = TRUE,
               full.names = TRUE)

  if (!any(grepl("XMP:Rating", colnames(subjects)))) {
    subjects <- subjects |> dplyr::mutate(`XMP:Rating` = NA)
  }
  if (!any(grepl("EXIF:Rating", colnames(subjects)))) {
    subjects <- subjects |> dplyr::mutate(`EXIF:Rating` = NA)
  }

  subjects <- tibble::tibble(subjects) |>
    dplyr::mutate(
      rating = ifelse(is.na(`EXIF:Rating`), `XMP:Rating`, `EXIF:Rating`)
    ) |>
    dplyr::mutate(
      subject = lapply(
        `XMP:HierarchicalSubject`,
        stringr::str_extract,
        pattern = "([^\\|]+)$"
      )
    ) |>
    dplyr::mutate(isExport = stringr::str_detect(SourceFile, "Export")) |>
    # dplyr::mutate(
    #   subject = ifelse(
    #     rating >= 4 & isExport,
    #     lapply(subject, c, "Apple-Favourite"),
    #     subject
    #   )
    # ) |>
  dplyr::mutate(
    subject = ifelse(!isExport, subject, dplyr::case_when(
      rating == 1 ~ lapply(subject, c, "1star"),
      rating == 2 ~ lapply(subject, c, "2stars"),
      rating == 3 ~ lapply(subject, c, "3stars"),
      rating == 4 ~ lapply(subject, c, "4stars"),
      rating == 5 ~ lapply(subject, c, "5stars"),
      )
    )
    ) |>
    dplyr::mutate(
      subject = lapply(subject, function(x) {
        paste(unlist(x), sep = '', collapse = ', ')
      })
    ) |>
    dplyr::mutate(subject = unlist(subject)) |>
    dplyr::select(SourceFile, subject, rating) |>
    dplyr::mutate(subject = ifelse(subject == "NA", NA, subject)) |>
    dplyr::mutate(`IPTC:Keywords` = subject, `EXIF:Rating` = rating) |>
    dplyr::rename(`XMP:Subject` = subject, `XMP:Rating` = rating)

  handle_return(
    subjects,
    csv_execute,
    paths,
    csv_path,
    delete_original,
    with_sep = ", "
  )
}
