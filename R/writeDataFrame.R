
#' Title
#'
#' @param df dataframe à exporter en fichier Excel
#' @param sheetName le nom de la feuille Excel
#' @param fileName  le nom du fichier Excel
#'
#' @export
#'
#' @examples
writeDataFrame <- function(df, sheetName, fileName){
  wb <- openxlsx::createWorkbook(sheetName)

  openxlsx::addWorksheet(wb, sheetName, gridLines = F)
  openxlsx::writeData(wb, sheet = sheetName, df, rowNames = F)
  openxlsx::addFilter(wb, sheetName, row = 1, cols = 1:ncol(df))
  # negStyle <- creadfyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  # posStyle <- creadfyle(fontColour = "#006100",  bgFill = "#C6EFCE")
  #
  # videSTyle <- creadfyle(fontColour = "#161616", bgFill = "#FFFFFF")
  #
  #
  # conditionalFormatting(wb,sheet = "Reconciliation", cols = 7, rows = 2:(nrow(Reco)+1),rule = ">0.3",  negStyle)
  #
  # conditionalFormatting(wb,sheet = "Reconciliation", cols = 7, rows = 2:(nrow(Reco)+1),rule = '<=0.3'  , posStyle)
  #
  # conditionalFormatting(wb,sheet = "Reconciliation", cols = 7, rows = 2:(nrow(Reco)+1),rule = '=""',  videSTyle)
  #

  headerStyle <-
    openxlsx::createStyle(
      fontSize = 10,
      fontColour = "#FFFFFF",
      halign = "center",
      fgFill = "#003366",
      border = "TopBottomLeftRight",
      borderColour = "#FFFFFF",
      wrapText = T
    )
  openxlsx::addStyle(
    wb,
    sheet = sheetName,
    headerStyle,
    rows = 1,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )
  bodyStyle <-
    openxlsx::createStyle(border = "TopBottomLeftRight",
                borderColour = "#4F81BD",
                wrapText = T)
  openxlsx::addStyle(
    wb,
    sheet = sheetName,
    bodyStyle,
    rows = 1:nrow(df) + 1,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  ncol(df)
  #setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
  openxlsx::setColWidths(wb,
               sheetName,
               cols = 1:ncol(df),
               widths = c(15,  25, 30))


  # pct <- creadfyle(numFmt = "0%", border = "TopBottomLeftRight",
  #                    borderColour = "#4F81BD")
  # addStyle(
  #   wb,
  #   sheet = "Reconciliation",
  #   style = pct,
  #   rows = 1:nrow(Reco) + 1,
  #   cols = 7,
  #   gridExpand = TRUE
  # )

  #
  # etude<- Formulaire_eig %>%
  #   select(contains("STUDY")) %>%
  #   unique() %>%
  #   #distinct() %>%
  #   pull()


  openxlsx::saveWorkbook(wb, paste0(fileName, ".xlsx"), overwrite = TRUE)
}
