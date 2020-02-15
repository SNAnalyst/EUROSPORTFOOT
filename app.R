library(rvest)
library(tidyverse)


# Premier ligue 
link_eng <- "https://www.topscorersfootball.com/premier-league"

eng <- link_eng %>% read_html() %>% html_table() %>% pluck(1)

eng$ligue <- "Premier League"


# Spain la Liga 

link_liga <- "https://www.topscorersfootball.com/la-liga"


liga <- link_liga %>% read_html() %>% html_table() %>% pluck(1) 


liga$ligue <- "La Liga"

# Ligue 1

link_lig1 <- "https://www.topscorersfootball.com/ligue-1"

lig1 <- link_lig1 %>% read_html() %>% html_table() %>% pluck(1) 

lig1$ligue <- "Ligue 1"

# SerieA 

serieA_link <- "https://www.topscorersfootball.com/serie-a"

serieA <- serieA_link %>% read_html() %>% html_table() %>% pluck(1)

serieA$ligue <- "Serie A"

# Bundesliga

Bundesliga_link <- "https://www.topscorersfootball.com/bundesliga"

Bundesliga <- Bundesliga_link %>% read_html() %>% html_table() %>% pluck(1)

Bundesliga$ligue <- "Bundesliga"


# Final Data

Final <- rbind(eng, liga, lig1, serieA, Bundesliga)

Final <- Final[, -1]


################### Shiny App #####################################


ui <- shinyUI(fluidPage(
    
    
    titlePanel(strong("Soccer App")),
    
    tabsetPanel(
        type = "pills",
    
        
        tabPanel(
            "Ranking",
            titlePanel("Scorer Ranking"),
            sidebarLayout(
                
                sidebarPanel(
                    
                    selectInput(inputId = "ch_ligue", 
                        label = "Choose a league", 
                        choices = unique(Final$ligue))
                    
                    ), 
                    
                    mainPanel(
                        
                        tableOutput("scorer_ranking")
                        
                        )
                    
                    
                    
                    
                    )))))

                    
                    
                    
                    
                    
                    
                    

server <- function(input, output) {
    
    
    df <- reactive({
        
        data <- Final %>% filter(ligue == input$ch_ligue)
        
        data <- data[, -5]
        
        return(data)
        
    })
    
    

    output$scorer_ranking <- renderTable({
        
        
        df()
        
        
    })
        
        
        
    
    

}



shinyApp(ui = ui, server = server)
