

#' Title
#'
#' @param df dataframe
#' @param dico dictionnaire
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
setCat <- function(df, dico){

pos <- 1

#ref <- stringr::str_c("R", "\U00E9", "f", "\U00E9", "rence")

dico <- dico %>%
        janitor::clean_names()
 # browser()
  nf <- get(df)
  if(is.data.frame(nf)){

    colonne<-stringr::str_remove(colnames(nf), "_C[0-9]")
    expo1<- paste0(df, sep="")

    ess<- nf %>%
      dplyr::select(dplyr::contains("MODULE"), dplyr::contains("FORM")) %>%
      names()
    #print(df)
    #print(colonne)
    #print(names(dico))

    cat_var<- dico %>%
      dplyr::filter(.data$reference %in% colonne) %>%
      dplyr::filter(stringr::str_detect(.data$type , "Cases \U00E0 cocher")) %>%
      dplyr::pull(.data$reference)

    #print(cat_var)

    #    print(tabyl(dico, Type))

    dico1 <- dico %>%
      dplyr::filter(.data$reference %in% colonne) %>%
      dplyr::filter(.data$type %in% c("Cases \U00E0 cocher")) %>%
      tidyr::separate_rows(.data$format, sep="\n") %>%
      tidyr::separate(.data$format, into = c("Cat", "Label"), sep="=", extra = "merge") %>%
      dplyr::mutate(reference = paste0(.data$reference, "_C", .data$Cat)) %>%
      dplyr::mutate(reference=stringr::str_trim(.data$reference, side="both")) %>%
      dplyr::mutate(Label=stringr::str_trim(.data$Label, side="both")) %>%
      dplyr::select(.data$reference, .data$Label)



    #print(dico1)
    #
    if(length(cat_var)!=0){



      #    message(paste0("variables cases à cocher: ",cat_var))
      #browser()
      #message(ess)
      # nf %>%
      #   select(FORM_MMTT)
      #   get_dupes(SUBJECT_REF, FORM_MMTT)

      nf_bis<- nf %>%
        dplyr::select(.data$SUBJECT_REF, dplyr::contains(cat_var), dplyr::contains("MODULE"), dplyr::contains("FORM")) %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::pivot_longer(-c("SUBJECT_REF", dplyr::all_of(ess))) %>%
        #get_dupes(SUBJECT_REF, MODULE_MMTT_VISIT, name)
        dplyr::left_join(dico1, by=c("name"="reference")) %>%
        dplyr::mutate(value2=ifelse(.data$value==1, .data$Label, .data$value)) %>%
        #filter(value==1)
        dplyr::select(-.data$Label, -.data$value) %>%

        tidyr::pivot_wider(names_from = .data$name, values_from = .data$value2)

      # message(df)
      # print(dim(nf))
      # print(dim(nf_bis))
      nf_tot<- nf %>%
        dplyr::select(-.data$SUBJECT_REF, -dplyr::contains(cat_var), -dplyr::contains("MODULE"), -dplyr::contains("FORM")) %>%
        dplyr::bind_cols(nf_bis)
      #   unite(col = "PATHO_CV",contains("PATHO_CV") ,sep = "|", na.rm = TRUE)

      # dim(nf_tot) %>%
      #   print()
      #
      cat_var %>%
        purrr::walk(.f=function(x){
          #  message(x)

          nf_tot<<- nf_tot %>%
            tidyr::unite(col = !!x, dplyr:: matches(paste0(x, "_C[0-9]*")), sep = "|", na.rm = TRUE) %>%
            dplyr::na_if(y="")

        })




        assign(expo1,nf_tot, envir=as.environment(pos))

    }
  }



}
