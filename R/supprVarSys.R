
#' Title
#'
#' @param df dataframe
#'
#' @export
#'
#' @examples
supprVarSys<- function(df){

  pos <- 1

  nf <- get(df)


  expo1<- paste0(df, sep="")

  #  message(class(nf))
  if(is.data.frame(nf)&(!df %in% c("dhm", "dico_code"))){

    # message(expo1)
    # message(ncol(nf))
    nf <-nf %>%
      dplyr::select(-dplyr::starts_with("REF"), -dplyr::contains("EXTRACTION"), -dplyr::ends_with("_ID"),.data$SITE_ID) #%>%
    #select(SITE_ID, SUBJECT_REF, everything())
    #select(-starts_with("REF_"), -ends_with("_ID"), -starts_with("EXTRACTION_"), -starts_with("MODULE_"))
    #print(ncol(nf))


      assign(expo1, nf, envir = as.environment(pos))

  }
}
