#' Generates the lens info (for zoom or prime) from the lensmodel
#' 
#' @description Format is: min focal range, max focal range, min aperture @ min range, min aperture @ max range
#' for primes, V2 and V4 are identical to V1 and V3 respectively
#'
#' @param lensmodel the lensmodel information from the respective metadata tag (usually from exiftool)
#' @param as_tags if TRUE, function returns lensinfo as exiftool arguments, else as raw information
#'
#' @returns a tibble with either the raw lensinfo or already prepared as exiftool arguments, depending on param as_tags
#' @export
create_lensinfo <- function(lensmodel, as_tags = TRUE){
  
  typenew = as.data.frame(
    stringr::str_remove(lensmodel, ".* (?=[0-9.]{1,5}mm)") |> 
      stringr::str_extract_all("[0-9.]{1,5}", simplify = TRUE))
  
  # build proper format
  info <- (if (ncol(typenew) == 2) {
    typenew |> 
      dplyr::mutate(V3 = V1, V4 = V2) |> 
      dplyr::mutate(V5 = paste(V1, V3, V2, V4)) 
  } else if (ncol(typenew) == 3) {
    typenew |> 
      dplyr::mutate(V4 = V3) |> 
      dplyr::mutate(V5 = paste(V1, V2, V3, V4)) 
  } else if (ncol(typenew) == 4) {
    typenew |> 
      dplyr::mutate(V5 = paste(V1, V2, V3, V4)) 
  }) |> 
    dplyr::select(5) |> 
    dplyr::pull()
  
  # prepare the lens tag
  lenses <- c(paste0("-exif:lensinfo=", info),
              paste0("-xmp:lensinfo=", info))
  
  if (as_tags)
    lenses
  else
    info
}


#' Write lens information
#' 
#' @description Writes various lens information tags obtained from a mapping table (LensMapping.csv) and existing metadata
#' into photo metadata. LensInfo is created dynamically and can be written separately by the according parameter
#' (this is often necessary because programs omit this information which is relevant for prime/zoom differentiation).
#' It also adds the 35mm equivalent focal length which relies on a hardcoded mapping table (CropFactor.csv).
#' It uses per default the csv option of exifool to handle different values for different files,
#' which is much faster than a for loop which calls exiftool every time
#'
#' @param paths List of photos to be modified. Needs a character vector with full file names
#' @param csv_execute logical, to determine whether modification should be directly written or returned as tibble
#' @param csv_path path and file name of the output csv file
#' @param delete_original whether the original copies of the photos should be deleted afterwards or not
#' @param lensinfo_only if TRUE, only the tag "lensinfo" is changed (this is often omitted by many programs)
#'
#' @returns depending on param csv_execute:
#' - if TRUE,  writes the tag values as csv file and writes them via exiftool to pictures
#' - if FALSE, returns the tags as tibble
#' @export
harmonize_lensinfo <- function(paths,
                               csv_execute = TRUE,
                               csv_path = '~/Pictures/lensinfo.csv',
                               delete_original = FALSE,
                               lensinfo_only = FALSE){
  
  args <- c("-G", "-s", "-n", "-lensinfo", "-lensmodel", "-XMP:lens", "-lensmake", "-exif:focallength*", "-exif:model")

  # read only metadata of photos where LensModel is known
  li <- exiftoolr::exif_read(args = args, path = paths) |> 
    dplyr::filter(!is.na(`EXIF:LensModel`))
  
  # check if the lensinfo and lensmake tag are completely missing and add them if necessary
  if (!"EXIF:LensInfo" %in% colnames(li))
    li <- li |> tibble::add_column("EXIF:LensInfo" = NA)
  if (!"EXIF:LensMake" %in% colnames(li))
    li <- li |> tibble::add_column("EXIF:LensMake" = NA)  

  # amend Nikon and Apple lens information
  # means lower case for lensmake and adaptation of lensmodel
  
  # change the information by joining the mapping table
  # for Viltrox (or non-Nikon?) lenses on Zf, lensmake is missing
  modify <- li |> 
    dplyr::left_join(exifer::lensMapping, by = dplyr::join_by(`EXIF:LensModel` == model_old)) |> 
    dplyr::mutate(`EXIF:LensModel` = ifelse(is.na(model_new), `EXIF:LensModel`, model_new)) |> 
    dplyr::mutate(`XMP:Lens` = `EXIF:LensModel`) |> 
    dplyr::mutate(`EXIF:LensMake` = ifelse(`EXIF:LensMake` == "NIKON", "Nikon", `EXIF:LensMake`)) |> 
    dplyr::mutate(`EXIF:LensMake` = ifelse(stringr::str_starts(`EXIF:LensModel`, "Viltrox"), "Viltrox", `EXIF:LensMake`)) |> 
    dplyr::mutate(`EXIF:LensMake` = ifelse(stringr::str_starts(`EXIF:LensModel`, "Sigma"), "Sigma", `EXIF:LensMake`)) |> 
    dplyr::mutate(`EXIF:LensInfo` = ifelse((is.na(`EXIF:LensInfo`) | (`EXIF:LensMake` == "Apple")),
                                           unlist(Vectorize(create_lensinfo)(`EXIF:LensModel`, FALSE)),
                                           `EXIF:LensInfo`)) |> 
    dplyr::mutate(`XMP:LensInfo` = `EXIF:LensInfo`) |> 
    dplyr::select(-model_new)
  
  if (lensinfo_only) {
    modify <- modify |> 
      dplyr::select(SourceFile, `EXIF:LensInfo`, `XMP:LensInfo`)
  }
  

# 35mm equivalent focallength ---------------------------------------------
  
  if (!any(grepl("FocalLengthIn35mmFormat", colnames(modify))))
    modify <- modify |> dplyr::mutate(`EXIF:FocalLengthIn35mmFormat` = NA)
  
  # Real cameras always have the same scaling factor for all lenses, smartphones have different ones
  # per lens. Therefore the mapping table's primary key sometimes consists of 1 column, sometimes of 2.
  # There's no elegant way to do the joining for that reason, so we have to do it in 2 steps and then
  # merge it together.
  focallength_1 <- modify |> 
    dplyr::rename(fl35Original = `EXIF:FocalLengthIn35mmFormat`) |> 
    dplyr::left_join(exifer::cropFactor,
                     by = dplyr::join_by("EXIF:Model" == "model", "EXIF:LensModel" == "lensmodel"))
  focallength_2 <- focallength_1 |> 
    dplyr::filter(is.na(factor)) |> 
    dplyr::select(SourceFile, `EXIF:FocalLength`, fl35Original, `EXIF:Model`, `EXIF:LensModel`) |> 
    dplyr::left_join(exifer::cropFactor |> dplyr::select(-lensmodel),
                     by = dplyr::join_by("EXIF:Model" == "model"))
  
  focallength <- focallength_1 |> 
    dplyr::filter(!is.na(factor)) |> 
    dplyr::add_row(focallength_2) |> 
    dplyr::mutate(`EXIF:FocalLengthIn35mmFormat` = ifelse(is.na(fl35Original),
                                                          floor(`EXIF:FocalLength`*factor),
                                                          fl35Original)) |> 
    dplyr::select(SourceFile, `EXIF:FocalLengthIn35mmFormat`)
  
  modify <- modify |> 
    dplyr::select(-`EXIF:FocalLengthIn35mmFormat`) |> 
    dplyr::left_join(focallength,
                     by = dplyr::join_by("SourceFile"))
  
  handle_return(modify, csv_execute, paths, csv_path, delete_original)
}
