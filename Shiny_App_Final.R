if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,dplyr, shiny)

ui <- fluidPage(
  
)
server <- function(input, output) {
}

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
      ),
      selectInput(
        inputId = "age_at_first_birth",
        label = "Age at First Childbirth",
        choices = c("All", sort(unique(df$age_at_first_birth)))
      ),
      selectInput(
        inputId = "BI_RADS_breast_density",
        label = "BIRADS breast density",
        choices = c("All", sort(unique(df$BI_RADS_breast_density)))
      ),
      selectInput(
        inputId = "use_of_hormone_therapy",
        label = "Use of Hormone Therapy",
        choices = c("All", sort(unique(df$use_of_hormone_therapy)))
      ),
      selectInput(
        inputId = "menopaus_status",
        label = "Menopaus Status",
        choices = c("All", sort(unique(df$menopaus_status)))
      ),
      selectInput(
        inputId = "bmi_range",
        label = "BMI Range",
        choices = c("All", sort(unique(df$bmi_range)))
      ),
      selectInput(
        inputId = "previous_biopsy_or_aspiration",
        label = "Previous Biopsy or Aspiration",
        choices = c("All", sort(unique(df$previous_biopsy_or_aspiration)))
      ),
      selectInput(
        inputId = "history_of_breast_cancer",
        label = "Previous History of Breast Cancer",
        choices = c("All", sort(unique(df$history_of_breast_cancer)))
      )
    ),
    mainPanel(
      textOutput("n_similar"),
      textOutput("pct_similar"),
      plotOutput("pct_plot")
    )
  )
)
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
    if (input$age_at_first_birth !="All") {
      filtered <-filtered %>% filter(age_at_first_birth==input$age_at_first_birth)
    }
    if (input$BI_RADS_breast_density !="All") {
      filtered <-filtered %>% filter(BI_RADS_breast_density==input$BI_RADS_breast_density)
    }
    if (input$use_of_hormone_therapy !="All") {
      filtered <-filtered %>% filter(use_of_hormone_therapy==input$use_of_hormone_therapy)
    }
    if (input$menopaus_status !="All") {
      filtered <-filtered %>% filter(menopaus_status==input$menopaus_status)
    }
    if (input$bmi_range !="All") {
      filtered <-filtered %>% filter(bmi_range==input$bmi_range)
    }
    if (input$previous_biopsy_or_aspiration !="All") {
      filtered <-filtered %>% filter(previous_biopsy_or_aspiration==input$previous_biopsy_or_aspiration)
    }
    if (input$history_of_breast_cancer !="All") {
      filtered <-filtered %>% filter(history_of_breast_cancer==input$history_of_breast_cancer)
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
    if (input$age_at_first_birth !="All") {
      filtered <-filtered %>% filter(age_at_first_birth==input$age_at_first_birth)
    }
    if (input$BI_RADS_breast_density !="All") {
      filtered <-filtered %>% filter(BI_RADS_breast_density==input$BI_RADS_breast_density)
    }
    if (input$use_of_hormone_therapy !="All") {
      filtered <-filtered %>% filter(use_of_hormone_therapy==input$use_of_hormone_therapy)
    }
    if (input$menopaus_status !="All") {
      filtered <-filtered %>% filter(menopaus_status==input$menopaus_status)
    }
    if (input$bmi_range !="All") {
      filtered <-filtered %>% filter(bmi_range==input$bmi_range)
    }
    if (input$previous_biopsy_or_aspiration !="All") {
      filtered <-filtered %>% filter(previous_biopsy_or_aspiration==input$previous_biopsy_or_aspiration)
    }
    if (input$history_of_breast_cancer !="All") {
      filtered <-filtered %>% filter(history_of_breast_cancer==input$history_of_breast_cancer)
    }
    grand_total<-sum(df$count)
    total <-sum(filtered$count)
    pct <- ((total/grand_total)*100)
    paste("% of the Women like you in the data:", format(pct, big.mark = ","))
    
  }
    
  )
output$pct_plot <-renderPlot({
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
  if (input$age_at_first_birth !="All") {
    filtered <-filtered %>% filter(age_at_first_birth==input$age_at_first_birth)
  }
  if (input$BI_RADS_breast_density !="All") {
    filtered <-filtered %>% filter(BI_RADS_breast_density==input$BI_RADS_breast_density)
  }
  if (input$use_of_hormone_therapy !="All") {
    filtered <-filtered %>% filter(use_of_hormone_therapy==input$use_of_hormone_therapy)
  }
  if (input$menopaus_status !="All") {
    filtered <-filtered %>% filter(menopaus_status==input$menopaus_status)
  }
  if (input$bmi_range !="All") {
    filtered <-filtered %>% filter(bmi_range==input$bmi_range)
  }
  if (input$previous_biopsy_or_aspiration !="All") {
    filtered <-filtered %>% filter(previous_biopsy_or_aspiration==input$previous_biopsy_or_aspiration)
  }
  if (input$history_of_breast_cancer !="All") {
    filtered <-filtered %>% filter(history_of_breast_cancer==input$history_of_breast_cancer)
  }
  grand_total<-sum(df$count)
  total <-sum(filtered$count)
  pct <- ((total/grand_total)*100)
  barplot(pct,ylim=c(0,100),ylab="Percent of the data like you", names.arg="Women like you")
})
}


shinyApp(ui,server)