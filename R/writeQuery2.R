#' Title
#'
#' @param data dataframe test
#' @param cheminQuery chemin des queries
#' @param etude nom de l'étude
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
writeQuery2<-function(data, cheminQuery, etude){

  if(!fs::dir_exists(cheminQuery)){

    fs::dir_create(cheminQuery)
  }

  # Query %>%
  #   set_names(
  #     names(.) %>% str_to_title()
  #   )

  for (site in unique(data$Centre)) {
    nom_fichier_query <-
      paste(
        cheminQuery,
        etude,
        "_queries_Centre_",
        site,
        "_",
        "Date_",
        Sys.Date(),
        ".xlsx",
        sep = ""
      )
    #browser()
    wb <- openxlsx::createWorkbook('Queries')
    temp <- data %>%
      dplyr::filter(.data$Centre == {
        {
          site
        }
       }) #%>%
      # purrr::set_names(
      #   names() %>% stringr::str_to_title()
      # )
    names(temp) <- stringr::str_to_title(names(temp))

    ref=paste0("Queries_Centre_",site)
    openxlsx::addWorksheet(wb, ref, gridLines = F)
    openxlsx::writeData(wb, sheet = ref, temp, rowNames = F)
    openxlsx::addFilter(wb, ref, row = 1, cols = 1:ncol(temp))
    headerStyle <-
      openxlsx::createStyle(
        fontSize = 10,
        fontColour = "#FFFFFF",
        halign = "center",
        fgFill = "#003366",
        border = "TopBottom",
        borderColour = "#4F81BD"
      )
    openxlsx::addStyle(
      wb,
      sheet = ref,
      headerStyle,
      rows = 1,
      cols = 1:ncol(temp),
      gridExpand = TRUE
    )
    bodyStyle <-
      openxlsx::createStyle(border = "TopBottomLeftRight",
                  borderColour = "#4F81BD",
                  wrapText = T)
    openxlsx::addStyle(
      wb,
      sheet = ref,
      bodyStyle,
      rows = 1:nrow(temp) + 1,
      cols = 1:ncol(temp),
      gridExpand = TRUE
    )
    #setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
    openxlsx::setColWidths(wb,
                 ref,
                 cols = 1:9,
                 widths = c(8,  15, 10, 20, 25, 25,25,50,25))
    openxlsx::saveWorkbook(wb, nom_fichier_query, overwrite = TRUE)
  }


}
