if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,dplyr, shiny)

ui <- fluidPage(
  
)
server <- function(input, output) {
}
shinyApp(ui, server)

df<-combined_risk_factors_categorical

ui <- fluidPage(
  titlePanel("Your Risk Factors"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "age_group",
        label = "Age Group",
        choices = c("All", sort(unique(df$age_group)))
      ),
      selectInput(
        inputId = "race_ethnicity",
        label = "Race or Ethnicity",
        choices = c("All", sort(unique(df$race_ethnicity)))
      ),
      selectInput(
        inputId = "first_degree_history",
        label = "First Degree Family History of Breast Cancer",
        choices = c("All", sort(unique(df$first_degree_history)))
      ),
      selectInput(
        inputId = "age_at_menarche",
        label = "Age at Menarche",
        choices = c("All", sort(unique(df$age_at_menarche)))
      )
    ),
    mainPanel(
      textOutput("n_similar"),
      textOutput("pct_similar")
    )
  )
)
grand_total<-sum(df$count)
server <-function(input,output) {
  output$n_similar <-renderText({
    filtered <-df
    if (input$age_group !="All") {
      filtered <-filtered %>% filter(age_group==input$age_group)
    }
    if (input$race_ethnicity !="All") {
      filtered <-filtered %>% filter(race_ethnicity==input$race_ethnicity)
    }
    if (input$first_degree_history !="All") {
      filtered <-filtered %>% filter(first_degree_history==input$first_degree_history)
    }
    if (input$age_at_menarche !="All") {
      filtered <-filtered %>% filter(age_at_menarche==input$age_at_menarche)
    }
    total <-sum(filtered$count)
    paste("Number of Women like you in the data:", format(total, big.mark = ","))
  })
  output$pct_similar <-renderText({
    filtered <-df
    if (input$age_group !="All") {
      filtered <-filtered %>% filter(age_group==input$age_group)
    }
    if (input$race_ethnicity !="All") {
      filtered <-filtered %>% filter(race_ethnicity==input$race_ethnicity)
    }
    if (input$first_degree_history !="All") {
      filtered <-filtered %>% filter(first_degree_history==input$first_degree_history)
    }
    if (input$age_at_menarche !="All") {
      filtered <-filtered %>% filter(age_at_menarche==input$age_at_menarche)
    }
    grand_total<-sum(df$count)
    total <-sum(filtered$count)
    pct <- ((total/grand_total)*100)
    paste("% of the Women like you in the data:", format(pct, big.mark = ","))
    
  }
    
  )
}


shinyApp(ui,server)