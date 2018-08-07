library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(rvest)
library(stringr)

get_general_info <- function(id) {
  url <- modify_url("https://store.steampowered.com/api/appdetails", 
                    query = list(appids = id))
  info <- fromJSON(url)
  name <- info[[1]]$data$name
  desc <- info[[1]]$data$detailed_description
  metascore <- info[[1]]$data$metacritic$score
  
  steamdb <- read_html(paste("https://steamdb.info/app/", id, "/info/", sep = ''))
  rating <- steamdb %>%
              html_node('.header-thing') %>%
              html_text()
  rating <- trimws(str_split(rating, '\n')[[1]][2])
  logo <-  steamdb %>%
            html_node(xpath = "//td[text()='logo_small']/following-sibling::td[1]/a") %>%
            html_attr(name = "href")
  
  current_players <- steamdb %>%
    html_node("#js-graphs-button") %>%
    html_text()
  current_players <- str_extract_all(current_players, '\\d+,?\\d+,?\\d+')
  
  return(list(name = name, 
              logo_link = logo, 
              desc = desc,
              rating = rating,
              metascore = metascore,
              current_players = current_players))
}

ui <- dashboardPage(
  dashboardHeader(title = "Steam Game Analytics"),
  dashboardSidebar(
    numericInput(inputId = "game_id", label = 'Game ID', value = "730")
  ),
  dashboardBody(
    fluidRow(
      column(12, htmlOutput("game_info"))
    )
  )
)

server <- function(input, output) {
  general_info <- reactive({get_general_info(input$game_id)})
  
  output$game_info <- renderUI({
          tags$div(
            tags$h1(tags$img(src = general_info()$logo_link),
            tags$a(general_info()$name, 
                    href = paste("https://store.steampowered.com/app/", 
                                 input$game_id, sep = ''))),
            infoBox("Rating", general_info()$rating, width = 4, fill = T),
            infoBox("Metacritic Score", general_info()$metascore, width = 4, fill = T, color = "green"),
            infoBox("Players Online", general_info()$current_players, width = 4, fill = T, color = "red"),
            HTML(general_info()$desc)
          )
  })
}

shinyApp(ui, server)
