


#' Title
#'
#' @param date_extract date d'extraction
#'
#' @export
#' @importFrom rlang .data
#' @examples
getDataBase <- function(date_extract){

  pos <- 1
  #message(getwd())
  #chemin <- paste0(getwd(),"/Donnees/", date_extract,"/")
  fichiers <- fs::dir_ls(paste0("./Donnees/", date_extract,"/"), glob = "*.csv")
  # message(fichiers)
  vroom::locale(encoding = "Windows-1252", decimal_mark = ".", grouping_mark = "")

  pb <- progress::progress_bar$new(total = length(fichiers))
  fichiers %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$value %>% stringr::str_detect(".csv$")) %>%
    tidyr::separate(
      .data$value,
      into = c("autre", "racine", "date", "fichier"),
      sep = "/",
      remove = F
    ) %>%
    tidyr::separate(.data$fichier, into = c("fichier", "extension"), "\\.") %>%
    dplyr::group_split(.data$value) %>%
    purrr::walk(function(x) {
      pb$tick()
      Sys.sleep(0.1)
      df <- vroom::vroom(x$value, locale = vroom::locale(encoding = "Windows-1252", decimal_mark = ".", grouping_mark = ""), na = "", guess_max = 10000, show_col_types = FALSE)
      assign(x$fichier, df, envir = as.environment(pos))

    })
}
