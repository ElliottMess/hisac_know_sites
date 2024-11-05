library(shiny)
library(dplyr)
library(googlesheets4)
library(leaflet)
library(leaflet.extras)
library(glue)

gs4_deauth()

gs_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1sjKxTfHzkiR_dsGTF7cZswjOi47nWqCLjmT_F32tTIE/edit?usp=sharing") %>%
  dplyr::rename(longitude = `Longitude site`,
         latitude = `Latitude site`) %>%
  dplyr::mutate(`finstroke link` = if_else(is.na(`finstroke link`), "Not available", `finstroke link`)) %>%
  filter(!is.na(latitude))

getColor <- function(gs_data) {
  sapply(gs_data$`Risk assessment completed`, function(risk) {
    if(risk) {
      "green"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'flag-outline',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(gs_data)
)


ui <- fluidPage(
  title = "Know sites to HISAC",
  # fluidRow(textOutput("text"))
  leafletOutput("map" , width = "99vw", height = "95vh")
)


server <- function(input, output) {
  
  # output$text <- renderText("test")
  output$map <- renderLeaflet({
    leaflet(gs_data) %>%
      addTiles()%>%
    addAwesomeMarkers(~longitude, ~latitude,
                      popup = paste0(
                        "<b>Dive site name:</b> ", gs_data$`Dive site name`, "<br>",
                        "<b>What3Words:</b> ", gs_data$What3Words, "<br>",
                        "<b>Finstroke link:</b> ", ifelse(gs_data$`finstroke link` == 'Not available', 'Not available<br>', paste0("<a href='", gs_data$`finstroke link`, "'>", gs_data$`finstroke link`, "</a><br>")),
                        "<b>Risk assessment completed:</b> ", ifelse(gs_data$`Risk assessment completed`, glue::glue("<span style='color:green'>Yes</span>"), "<span style='color:red'>No</span>")
                      ),
                      icon=icons
    ) %>% 
      addLegend("bottomright",
                colors = c("green", "red"),
                labels = c("Has Risk Assessment", "No Risk Assessment ")
                )
  #   %>%
  # addFullscreenControl()
  })
}

shinyApp(ui = ui, server = server)