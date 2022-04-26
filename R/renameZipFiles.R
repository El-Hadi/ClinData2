#' Title
#'
#'
#' @export
#' @importFrom rlang .data
#' @examples
renameZipFiles <- function(){


  zipfiles<- fs::dir_ls("Donnees/", glob = "*.zip")

  zipfiles |>
    purrr::walk(.f=function(x){

      zipfile <- x
      fichier<- zip::zip_list(zipfile = zipfile) |>
        tibble::as_tibble() |>
        dplyr::filter(stringr::str_detect(.data$filename, ".csv$")) |>
        dplyr::slice_head(n=1) |>
        dplyr::pull(.data$filename)


      nom<- vroom::vroom(file = unz(zipfile, fichier), show_col_types = FALSE) |>
        dplyr::select(.data$STUDY_ID, .data$EXTRACTION_DATE) |>
        dplyr::mutate(EXTRACTION_DATE=.data$EXTRACTION_DATE |> stringr::str_remove('[=\"]') |> stringr::str_replace_all('\"', '') |> stringr::str_sub(1,10)) |>
        dplyr::mutate(EXTRACTION_DATE=as.Date(.data$EXTRACTION_DATE, format="%d/%m/%Y")) |>
        dplyr::mutate(EXTRACTION_DATE=as.character(.data$EXTRACTION_DATE) |> stringr::str_remove_all("-")) |>
        dplyr::mutate(EXTRATION=paste0(.data$STUDY_ID, "_", .data$EXTRACTION_DATE)) |>
        dplyr::select(.data$EXTRATION) |>
        dplyr::distinct() |>
        dplyr::pull()

      file.rename(zipfile, to = paste0("Donnees/", nom,".zip"))

    })
}
