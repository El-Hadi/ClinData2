

#' Title
#'
#' @param Var variable à controler
#' @param cond condition à vérifier pour la variable
#' @param com  commentaire
#' @param fichier dataframe où se trouve la variable
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
coherence3 <- function (Var, cond, com, fichier){


  if (!exists("i")) {
    i <- 0
  }
  i <<- i + 1
  if (!exists("test")) {
    test <-
      tibble::tibble(
        id_patient = character(),
        Centre = character(),
        Fichier = character(),
        Var = character(),
        Label=character(),
        Val = character(),
        Text = character(),
        Nquerie = integer()
      )
  }


  filtre <- rlang::parse_expr(cond)

  valeur <- stringr::str_split(com, ":", simplify = T)[1] %>% stringr::str_trim(side="both")
  message(valeur)
  commentaire<-stringr::str_split(com, ":", simplify = T)[2] %>% stringr::str_trim(side="both")

  fi <- deparse(substitute(fichier))
  message(Var)
  labs <- Hmisc::label(fichier[[Var]])
  #browser()
  essai <-
    fichier %>%
    dplyr::filter(!!filtre) %>%
    dplyr::rename(MODULE=dplyr::contains("MODULE")) %>%
    dplyr::mutate(
      fich = fi,
      Nquerie = i,
      Fichier = .data$MODULE,
      Var = Var,
      Val = as.character(!!rlang::parse_expr(valeur)),
      Label=labs,
      #Label=paste0(Hmisc::label(!!rlang::parse_expr(Var))),
      Text = paste0( Hmisc::label(!!valeur),": ", commentaire),
      `Centre` = .data$SITE_ID,
      id_patient = as.character(.data$SUBJECT_REF)
    ) %>%
    # dplyr::mutate(
    #                     ,
    #                     mod_form = "fichier") %>%
    dplyr::select(.data$id_patient,
                  .data$Centre,
                  .data$Fichier,
                  .data$Var,
                  .data$Val,
                  .data$Label,
                  #mod_form,
                  .data$Nquerie,
                  .data$Text)

  essai$Val <- as.character(essai$Val)
  test <<- test %>% dplyr::union(essai)
}
