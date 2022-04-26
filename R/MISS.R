#' Title
#'
#' @param Var variable
#' @param lvar lab
#' @param fichier dataframe
#'
#'
#' @export
#'
#' @examples
MISS <- function (Var, lvar, fichier)
{
  if (!exists("i")) {
    i <- 0
  }
  i <<- i + 1
  filtre <- rlang::parse_expr(Var)
  lab <- Hmisc::label(fichier[[Var]])
  fi <- deparse(substitute(fichier))
  print(lvar)
  module <- stringr::str_split(deparse(substitute(fichier)),
                               "_")[1]
  if (!exists("test")) {
    test <-
      tibble::tibble(
        id_patient = character(),
        Centre = character(),
        Fichier = character(),
        Label=character(),
        Var = character(),
        Val = character(),
        Text = character(),
        Nquerie = integer()
      )
  }
  essai <-
    fichier %>%
    dplyr::filter(is.na(!!filtre)) %>%
    dplyr::mutate(
      # fich = fi,
      Nquerie = i,
      Fichier = fi,
      Var = Var,
      Val = "" ,
      Label=lab,
      Text = paste0(
        # filtre,
        Hmisc::label(!!filtre),
        ": la reponse est manquante. Veuillez completer SVP."
      )
    ) %>%
    dplyr::mutate(Centre = .data$SITE_ID, id_patient = as.character(.data$SUBJECT_REF)) %>%
    dplyr::select(.data$id_patient,
                  .data$Centre ,
                  .data$Fichier,
                  .data$Var,
                  .data$Val,
                  .data$Label,
                  .data$Nquerie,
                  .data$Text)
  test <<- test %>% dplyr::union(essai)
}
