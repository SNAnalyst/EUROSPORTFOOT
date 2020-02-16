library(rvest)
library(tidyverse)
library(shinythemes)
library(ggthemes)
library(maps)
library(ggdark)
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

# Liga Portugal 


LigaPortugal_link <- "https://www.topscorersfootball.com/primeira-liga"

LigaPortugal <- LigaPortugal_link %>% read_html() %>% html_table() %>% pluck(1)

LigaPortugal$ligue <- "Liga Portugal"



# Süper Lig


SuperLig_link <- "https://www.topscorersfootball.com/super-lig"

SuperLig <- SuperLig_link %>% read_html() %>% html_table() %>% pluck(1)

SuperLig$ligue <- "Süper Lig"


# Eredivisie 

Eredivisie_link <- "https://www.topscorersfootball.com/eredivisie"

Eredivisie <- Eredivisie_link %>% read_html() %>% html_table() %>% pluck(1)

Eredivisie$ligue <- "Eredivisie"


# Champions League

CL_link <- "https://www.topscorersfootball.com/champions-league"

CL <- CL_link %>% read_html() %>% html_table() %>% pluck(1)

CL$ligue <- "Champions League"


# Champions League

EL_link <- "https://www.topscorersfootball.com/europa-league"

EL <- EL_link %>% read_html() %>% html_table() %>% pluck(1)

EL$ligue <- "Europa Ligue"



# Super League Greece


SLG_link <- "https://www.topscorersfootball.com/superleague"

SLG <- SLG_link %>% read_html() %>% html_table() %>% pluck(1)

SLG$ligue <- "Super League Greece"


# Swiss Super League

SSL_link <- "https://www.topscorersfootball.com/super-league"

SSL <- SSL_link %>% read_html() %>% html_table() %>% pluck(1)

SSL$ligue <- "Swiss Super League"





# Final Data ##############################################

Final0 <- rbind(eng, 
    liga, 
    lig1, 
    serieA, 
    Bundesliga, 
    LigaPortugal, 
    SuperLig,
    Eredivisie, 
    CL, 
    EL, 
    SLG, 
    SSL)

Final <- Final0[, -1]


######## Map Data ###############################################

world <- map_data("world")

world <- world %>% rename(Nationality = region)


Total_Goals <- Final %>% group_by(Nationality) %>% 
    summarise(TotalG = sum(Goals)) %>% 
    arrange(desc(TotalG)) %>% 
    as.data.frame()




Final_map <- left_join(world, Total_Goals, by = "Nationality")



#### Ligue Scorer statistics ###################################



Stats_ligue <- Final0 %>% group_by(ligue) %>% 
  summarise(mean = round(mean(Goals), 2),
    median = median(Goals), 
    sum = sum(Goals)) %>% 
  arrange(desc(sum))


################### Shiny App #####################################


ui <- shinyUI(fluidPage(theme = shinytheme("cyborg"),
    
    

    titlePanel(strong("Soccer App")),
    
    tabsetPanel(
        type = "pills",
    
        
        tabPanel(
            "Ranking",
            titlePanel("Scorer Ranking"),
                
            fluidRow(
                    
                column(12, 
                    selectInput(inputId = "ch_ligue", 
                    label = "Choose a league", 
                    choices = sort(unique(Final$ligue)), width = "400px"
                        
                        )
                    
                    )
                    
                    
                    ), 
            
            fluidRow(
                
                column(5, tableOutput("scorer_ranking")), 
                column(7, plotOutput("scorer_plot"))
                
                
                
            )
        
        
        ), 
        
    
    tabPanel(
        
        
        "Map", 
        
        titlePanel("Scorer Map"), 
        
        fluidRow(
            
            column(12, plotOutput("scorer_map") )
            
            
        ), 
        
        fluidRow(
            
            
            column(12, plotOutput("scorer_map_plot"))
            
        )
        
        
        
        
    ),
      
      
      tabPanel(
        
        
        "Score Ligue", 
        
        titlePanel("Score Statistics"), 
        
        fluidRow(
          
          column(4, tableOutput("scorer_ligue")), 
          column(8, plotOutput("scorer_ligue_plot"))
          
          
      )
    
    
    ))))






server <- function(input, output) {
    
    
    df <- reactive({
        
        data <- Final %>% filter(ligue == input$ch_ligue)
        
        data <- data[, -5]
        

        data <- data %>% 
            mutate(rank = dense_rank(desc(Goals)))
        
        data <- data %>% select(rank, everything())
        
        return(data)
        
    })
    
    

    output$scorer_ranking <- renderTable({
        
        
        df()
        
        
    })
        
    
    output$scorer_plot <- renderPlot({
    
    ggplot(df(), aes(reorder(Player, Goals), Goals)) +
            geom_col(col = "red1", fill = "lavenderblush", 
                width=0.4, position = position_dodge(width=0.5))  +
            coord_flip() +
            xlab("") +
            theme_fivethirtyeight()

        
    })
    
    
    
    output$scorer_map <- renderPlot({
        
        
        ggplot(Final_map, aes(x=long, y=lat, group=group, fill = TotalG)) +
            geom_polygon(colour="black") +
            scale_fill_viridis_c(option = "viridis") +
            labs(fill = '') +
            theme_void()
        
        
        
        
    })
    
    
  
    
    output$scorer_map_plot <- renderPlot({
        
        
        ggplot(Total_Goals, aes(reorder(Nationality, TotalG), TotalG))+
            geom_col(col = "lightblue", fill = "darkred", 
                width=0.4, position = position_dodge(width=0.5))+
            coord_flip()+
            theme_replace()+
            xlab("")+
            ylab("") 

        
    })
    
    

    output$scorer_ligue <- renderTable({
      
      
      Stats_ligue
      
      
      
    })
    
    
    output$scorer_ligue_plot <- renderPlot({
      
      
      
      ggplot(Stats_ligue, aes(reorder(ligue, sum), sum)) +
        geom_col(fill = "pink", col = "darkred") +
        labs(title = "Total Best Scorers Goals per ligue",
          x ="", y = "") +
        coord_flip() +
        dark_theme_dark(base_size = 16)
      
     
      
      
      
    })

}



shinyApp(ui = ui, server = server)
