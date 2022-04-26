


#' Title
#'
#' @param df dataframe
#' @param dico dictionnaire
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
setDates <-function(df, dico){


  pos <- 1

  dico <- dico %>%
    janitor::clean_names()
  # ref <- stringr::str_c("R", "\U00E9", "f", "\U00E9", "rence")
  nf <- get(df)
  colonne<-colnames(nf)
  expo1<- paste0(df, sep="")

  dico1 <- dico %>%
    dplyr::filter(.data$reference %in% colonne) %>%
    dplyr::filter(.data$type=="Date")

  dico1 %>%
    dplyr::group_split(.data$reference) %>%
    purrr::walk(function(x){

      var <- rlang::sym(x$reference[[1]])

      mois <- rlang::sym(paste0(x$reference[[1]], "_M", sep=""))
      jour <- rlang::sym(paste0(x$reference[[1]], "_D", sep=""))
      annee <- rlang::sym(paste0(x$reference[[1]], "_Y", sep=""))
      datec <- rlang::sym(paste0(x$reference[[1]], ".c", sep=""))
      datebis <- rlang::sym(paste0(x$reference[[1]], ".bis", sep=""))
      #message(var)
      lab<- attr(nf[[var]], which = "label")

      nf <<- nf %>%
        dplyr::mutate(!!var:=as.Date(!!var, "%d/%m/%Y")) %>%
        # mutate(!!datebis:=ifelse(!(is.na({{annee}})|is.na({{mois}})|is.na({{jour}})), paste0(!!annee,"-",!!mois, "-",!!jour, sep=""), NA)) %>%
        tidyr::unite(!!datec, c(!!annee, !!mois, !!jour), sep = "-", remove = FALSE) %>%
        dplyr::mutate(!!datec:=ifelse(!!datec=="NA-NA-NA"&is.na(!!annee),NA, !!datec)) %>%
        dplyr::select(-!!annee, -!!mois, -!!jour)

      #message(attr(nf[[var]], which = "label"))
      #message(sjlabelled::get_label(nf[[var]]))
      #message(lab)
      attr(nf[[datec]], which = "label") <<- lab
      attr(nf[[var]], which = "label") <<- lab
      # message(sjlabelled::get_label(nf[[datec]]))

    })

    assign(expo1, nf, envir=as.environment(pos))

}


