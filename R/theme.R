
# theme_UNHCR <- fresh::create_theme(
#   fresh::adminlte_color(
#   #   light_blue = "#0072BC",
#   #   blue = "#0072BC"
#   ),
#   fresh::adminlte_sidebar(
#     # width = "300px",
#     # dark_bg = "#0063a3",
#     # dark_hover_bg = "#81A1C1",
#     # dark_color = "#2E3440"
#   ),
#   fresh::adminlte_global(
#     # content_bg = "#DCE9FF",
#     # box_bg = "#FFF",
#     # info_box_bg = "#D8DEE9"
#   )
# )


#' theme_dashboard
#'
#'
#@return nothing
#'
#@importFrom dashboardthemes shinyDashboardThemeDIY
#'
#@noRd
theme_dashboard <- function() {
  primary <- "#0072BC"
  accent <- "#FAEB00"
  secondary <- "#FAEB00"

  dashboardthemes::shinyDashboardThemeDIY(

    #general
    appFontFamily = "Nunito Sans"
    ,appFontColor = "#000"
    ,primaryFontColor = "#000"
    ,infoFontColor = "#000"
    ,successFontColor = "#000"
    ,warningFontColor = "#000"
    ,dangerFontColor = "#000"
    ,bodyBackColor = "#f5f5f5"

    #header
    ,logoBackColor = primary
    ,headerButtonBackColor = primary
    ,headerButtonIconColor = "white"
    ,headerButtonBackColorHover = primary
    ,headerButtonIconColorHover = "#66AAD7"
    ,headerBackColor = primary
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "0px 0px 0px"

    #sidebar
    ,sidebarBackColor = "#343a40"

    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = "0px 0px 0px"
    ,sidebarShadowColor = "#aaaaaa"

    ,sidebarUserTextColor = "#696969"

    ,sidebarSearchBackColor = primary
    ,sidebarSearchIconColor = "#fff"
    ,sidebarSearchBorderColor = primary

    ,sidebarTabTextColor = "#FFF"
    ,sidebarTabTextSize = 16
    ,sidebarTabBorderStyle = "none none none solid"
    ,sidebarTabBorderColor = "transparent"
    ,sidebarTabBorderWidth = 0
    ,sidebarTabBackColorSelected = primary
    ,sidebarTabTextColorSelected = "#FFF"
    ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
    ,sidebarTabBackColorHover = "transparent"
    ,sidebarTabTextColorHover = "#e2e2e2"
    ,sidebarTabBorderStyleHover = "none none none solid"
    ,sidebarTabBorderColorHover = "#489bd1"
    ,sidebarTabBorderWidthHover = 5
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"

    ### boxes
    ,boxBackColor = "white"
    ,boxBorderRadius = 0
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = paste0(primary,"30")
    ,boxTitleSize = 24
    ,boxDefaultColor = "#FFF"
    ,boxPrimaryColor = "#FFF"
    ,boxInfoColor = "#FFF"
    ,boxSuccessColor = "#FFF"
    ,boxWarningColor = "#FFF"
    ,boxDangerColor = "#FFF"

    ,tabBoxTabColor = "white"
    ,tabBoxTabTextSize = 12
    ,tabBoxTabTextColor = "#696969"
    ,tabBoxTabTextColorSelected = primary
    ,tabBoxBackColor = "#FFF"
    ,tabBoxHighlightColor = primary
    ,tabBoxBorderRadius = 0

    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 0
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 0
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"

    ### tables
    ,tableBackColor = NA
    ,tableBorderColor = NA
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
  )
} #theme

