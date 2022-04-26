#' Title
#'
#' @param df dataframe à recoder(pour les variables catégorielles)
#' @param dico dictionnaire de données(dhm)
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
#'
#' @examples
setFormat <- function(df, dico){

  #browser()
  pos <- 1

  dico <- dico %>%
    janitor::clean_names()

  nf <- get(df, envir = .GlobalEnv)
  if(is.data.frame(nf)){
    colonne <- colnames(nf)
    #message(rlang::ensym(df))
    # message(rlang::ensym(nf))
    # print(str_split(nf, "_ANSI", simplify = TRUE))
    expo <- paste0(stringr::str_split(df, "_ANSI", simplify = TRUE)[1],"_F")
    #browser()
    dico1 <- dico %>%
      dplyr::filter(.data$reference %in% colonne) %>%
      dplyr::distinct(.data$reference, .keep_all=T)

    # print(colnames(nf))
    # message(expo)
    dico1 %>%
      dplyr::ungroup() %>%
      #slice(1:2) %>%
      #distinct(R?f?rence, .keep_all=T) %>%
      dplyr::group_split(.data$reference) %>%


      purrr::map(function(x){
        # if(length(x$R?f?rence)>1){
        #   message(x$R?f?rence)
        # }

        #if(x$R?f?rence %in% colnames(nf)){
        # print("Ok ref")
        # message(x$R?f?rence[[1]])
        # message(length(x$R?f?rence))
        # message(x$Format)

        #      print(x$Référence[[1]])
        var=rlang::sym(x$reference[[1]])
        form=rlang::sym(x$format[[1]])

        form0<- x$format[[1]]
        #     message(paste0("Recodage : ", var))
        #print(form0)

        var.f<-paste0(var, ".f")

        if(var=="EI_TT_FREQ_UNIT"){
          labs <- "Unit"
        }else{
          #message("OK")
          #print(x$Intitulé[[1]])
          if(is.na(x$intitule[[1]])){

            labs <- ""
          }else{
            labs <- rlang::sym(x$intitule[[1]])
          }

        }


        nf<<- nf %>%
          #mutate(!!var.f:=!!var) %>%
          dplyr::mutate(!!var.f:=car::Recode(!!var, form0) %>%  stringr::str_trim(side="both") %>% as.factor()) %>%
          sjlabelled::var_labels(!!var.f:=!!labs)
        # mutate(!!var.f:=str_trim(!!var.f, side="both"))


        # if(exists(expo)){
        #   #message("ok")
        #   data <- get(expo)
        # }else{
        #
        #   data <- get(df)
        # }
        #


        # df1 <<- data %>%
        #   mutate(!!var.f:=recode(!!var, form)) %>%
        #   mutate(!!var.f:=str_trim(!!var, side="both"))
        #


        #}

        assign(df, nf, envir=as.environment(pos))

      })

    assign(df, nf, envir=as.environment(pos))
  }

  # df1 %>%
  #  assign(df, ., envir=.GlobalEnv)
}
