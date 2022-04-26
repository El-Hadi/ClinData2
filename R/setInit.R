


#' Title
#'
#' @param date_extract date d'extraction
#' @param dhm dictionnaire
#'
#
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
setInit <- function(date_extract, dhm){

  pos <- 1

  message(stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "Lecture de la base de donn<U+00E9>es")))

  getDataBase(date_extract)




  assign("path", paste0("Donnees/",date_extract,"/"), envir = as.environment(pos))

  fichier<- fs::dir_ls(get("path"), glob = "*.sas")


  #Labels ----
  pb <- progress::progress_bar$new(total = length(fichier), )


  message(stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "Cr<U+00E9>ation de labels")))

  fichier %>%
    purrr::walk(~{setLabels(.x); pb$tick();Sys.sleep(0.1)})

  dic <- dhm %>%
        janitor::clean_names()

  dico_code<- dic %>%
    dplyr::mutate(module = ifelse(is.na(.data$type), .data$id, NA_character_)) %>%
    tidyr::fill(.data$module) %>%
    #  filter(str_detect(Format, "G2 ")) %>%
    # select(Format)
    #filter(!((is.na(Type)|is.na(R?f?rence)))) %>%

    dplyr::filter(.data$type %in% c("Radio buttons", "Combo box")) %>%
    #filter(R\U00E9f\U00E9rence=="ANTI_GAD_DONE_INCL") %>%
    tidyr::separate_rows(.data$format, sep="\n") %>%
    # count(Format) %>%
    # tail()
    tidyr::separate(.data$format, into=c("Code", "Label"), sep="=", extra = "merge") %>%
    dplyr::mutate(Code=stringr::str_trim(.data$Code, side="both"),
           Label=stringr::str_trim(.data$Label, side="both")) %>%
    dplyr::filter(!.data$intitule %in% c("Unit\U00E9")) %>%
    dplyr::mutate(Label=stringr::str_replace_all(.data$Label, "=", "-")) %>%
    dplyr::mutate(Label=stringr::str_replace_all(.data$Label, ":", "-")) %>%
    dplyr::mutate(Label=stringr::str_replace_all(.data$Label, '\"', "'")) %>%
    #select(Code, Label)
    dplyr::mutate(format=glue::glue('"{Code}"="{Label}"')) %>%
    #filter(str_detect(Label, "=")) %>%
    dplyr::select(-.data$id, -.data$saisie, -.data$type) %>%
    dplyr::group_by(.data$reference, .data$intitule, .data$module) %>%
    dplyr::summarise(format=paste0(.data$format, collapse = "; "), .groups = "keep") %>%
    dplyr::ungroup()

  assign("dico_code", dico_code, envir = as.environment(pos))

  #Formats----

  pb <- progress::progress_bar$new(total = length(ls(envir = as.environment(pos))))

  message(stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "Cr<U+00E9>ation des formats des variables cat<U+00E9>gorielles (choix unique)")))

  ls(envir = as.environment(pos)) %>%
    purrr::walk( ~ {setFormat(.x, dico_code); pb$tick(); Sys.sleep(0.1)})


  #Dates ----
  pb <- progress::progress_bar$new(total = length(ls(envir = as.environment(pos))))

  message(stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "Cr<U+00E9>ation des dates format caract<U+00E8>res")))


  ls(envir = as.environment(pos)) %>%
    purrr::walk(~{setDates(.x, dhm);pb$tick(); Sys.sleep(0.1)})




  #variable cat\U00E9gorielle ----
  pb <- progress::progress_bar$new(total = length(ls(envir = as.environment(pos))))
  message(stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "Cr<U+00E9>ation des variables choix multiples")))



  #setCat("mmtt_v59", dhm)

  ls(envir = as.environment(pos)) %>%
    purrr::walk(~{setCat(.x, dhm);pb$tick(); Sys.sleep(0.1)})


  # #variables syst\U00E8mes ----

  pb <- progress::progress_bar$new(total = length(ls(envir = as.environment(pos))))

  message(stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "suppression des variables syst<U+00E8>mes")))

  ls(envir = as.environment(pos)) %>%
    purrr::walk(~{supprVarSys(.x);pb$tick(); Sys.sleep(0.1)})

}
