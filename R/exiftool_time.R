#' sets datetimeoriginal to createdate
#'
#' @description
#' Sets the datetimeoriginal tag to the same value as createdate to obtain more coherent time of creation.
#' Especially important for mixed camera scenarioes.
#' Does not solve all time related issues yet, needs more work and clarification with regard to ON1.
#'
#' @param paths List of photos to be modified. Needs a character vector with full file names
#' @param csv_execute logical, to determine whether modification should be directly written or returned as tibble
#' @param csv_path path and file name of the output csv file
#' @param delete_original whether the original copies of the photos should be deleted afterwards or not
#' @param offset the timezone offset in hours (compared to UTC)
#'
#' @returns depending on param csv_execute:
#' - if TRUE,  writes the tag values as csv file and writes them via exiftool to pictures
#' - if FALSE, returns the tags as tibble
#' @export
harmonize_time <- function(
  paths,
  csv_execute = TRUE,
  csv_path = '~/Pictures/time.csv',
  delete_original = FALSE,
  offset
) {
  args <- c(
    "-G",
    "-s",
    "-n",
    "-e",
    "-time:all",
    "--makernotes:all",
    "--file:all",
    "-exif:make"
  )

  offset <- convert_hours(offset)

  times <- exiftoolr::exif_read(args = args, path = paths)

  # check if some tags are completely missing and add them if necessary
  if (!any(grepl("EXIF:DateTimeOriginal", colnames(times)))) {
    times <- times |> tibble::add_column("EXIF:DateTimeOriginal" = NA)
  }
  if (!any(grepl("IPTC:TimeCreated", colnames(times)))) {
    times <- times |> tibble::add_column("IPTC:TimeCreated" = NA)
  }
  if (!any(grepl("IPTC:DateCreated", colnames(times)))) {
    times <- times |> tibble::add_column("IPTC:DateCreated" = NA)
  }
  if (!any(grepl("EXIF:CreateDate", colnames(times)))) {
    times <- times |> dplyr::mutate(`EXIF:CreateDate` = `EXIF:DateTimeOriginal`)
  }

  times <- times |>
    dplyr::mutate(`EXIF:DateTimeOriginal` = `EXIF:CreateDate`) |>
    dplyr::mutate(`XMP:DateTimeOriginal` = `EXIF:CreateDate`) |>
    dplyr::mutate(`IPTC:TimeCreated` = "-") |>
    dplyr::mutate(`IPTC:DateCreated` = "-")

  # for iPhone pictures, we need to rewrite some time values to get proper order in the various tools.
  times_iphone <- times |>
    dplyr::filter(`EXIF:Make` == "Apple") |>
    dplyr::mutate(`XMP:CreateDate` = `EXIF:CreateDate`) |>
    dplyr::mutate(`EXIF:OffsetTime` = offset) |>
    dplyr::mutate(`EXIF:OffsetTimeOriginal` = offset) |>
    dplyr::mutate(`EXIF:OffsetTimeDigitized` = offset)

  # combine the iPhone ond non-iPhone pix
  times <- times_iphone |>
    dplyr::add_row(times |> dplyr::filter(`EXIF:Make` != "Apple")) |>
    dplyr::select(
      SourceFile,
      `EXIF:DateTimeOriginal`,
      `EXIF:CreateDate`,
      `EXIF:OffsetTime`,
      `EXIF:OffsetTimeOriginal`,
      `EXIF:OffsetTimeDigitized`,
      `XMP:DateTimeOriginal`,
      `XMP:CreateDate`,
      `IPTC:TimeCreated`,
      `IPTC:DateCreated`
    )

  handle_return(times, csv_execute, paths, csv_path, delete_original)
}
