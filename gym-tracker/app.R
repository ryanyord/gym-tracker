## Author: Ryan Yordanoff
## For strong.app data

library(shiny)
library(tidyverse)
options(shiny.maxRequestSize=30*1024^2)

#++++++++++++++++++++++++++++++++UI++++++++++++++++++++++++++++++++++++++++++++
# Define UI
ui <- fluidPage(
  shinyUI(navbarPage("Do you even lift bro?",
                     mainPanel(
                       tabsetPanel(
                         #---------------------------Tab 1: Samples------------------------------------
                         # Tab 1: Samples 
                         tabPanel("Samples",
                                  
                                  # Sidebar layout
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      # Input: Select file
                                      fileInput(inputId='fileuploadt1',
                                                label= 'Load Sample Information:',
                                                placeholder = 'sample_info.csv'),
                                      
                                    ),
                                    
                                    
                                    mainPanel(
                                      # Tabs
                                      tabsetPanel(
                                        # Tab 1: Summary - Total weight lifted?
                                        tabPanel("Summary",
                                                 tableOutput('table_t1')),
                                        # Tab 2: Table
                                        tabPanel("Table",
                                                 tableOutput('table_t1_2')),
                                        # Tab 3: Plots
                                        tabPanel("Plots",
                                                 plotOutput('hist_plots',
                                                            width = "1200px",
                                                            height = "1200px")),
                                      )
                                    ),
                                    
                                    
                                  )
                         ),
                         
                        
)))))


#++++++++++++++++++++++++++++++++SERVER+++++++++++++++++++++++++++++++++++++++++
# Define server logic
server <- function(input, output, session) {
  
  
  
  #---------------------------Tab 1: Samples-------------------------------------
  load_data_t1 <- reactive({
    data <- read_delim(input$fileuploadt1$datapath, delim = ";")
    return(data)
  })
  
  
  
  
  summary_table <- function(data) {
    sample_info <- data
    df <- data.frame(sapply(sample_info, class)) %>%
      as_tibble(rownames='Column Name') %>%
      rename(Type = 2)
    
    df2 <- data.frame(sapply(sample_info, function(x) str_glue('{round(mean(x),2)} (+/- {round(sd(x), 2)}))'))) %>%
      as_tibble(rownames='Column Name') %>%
      rename(`Mean (sd)` = 2)
    
    df3 <- data.frame(sapply(sample_info, function(x) str_glue('{length(unique(x))} Distinct Values')))%>%
      as_tibble(rownames='Column Name') %>%
      rename(`Distinct Values` = 2)
    
    output_df <- cbind(df, (df2[2]), (df3[2])) %>%
      mutate(`Mean (sd)` = na_if(`Mean (sd)`, "NA (+/- NA))"), 
             `Mean (sd)` = coalesce(`Mean (sd)`, `Distinct Values`)) %>%
      select(-`Distinct Values`) %>%
      rename(`Mean (sd) or Distinct Values` = `Mean (sd)`)
    
    
    return(output_df)
  }
  
  multi_hist <- function(data) {
    return(data %>% select_if(is.numeric) %>%
             hist.data.frame())
  }
  
  
  
  
  
  output$table_t1 <- renderTable({req(input$fileuploadt1)
    summary_table(load_data_t1())
  })
  
  output$table_t1_2 <- renderTable({req(input$fileuploadt1)
    load_data_t1()
  })
  
  output$hist_plots <- renderPlot({req(input$fileuploadt1)
    multi_hist(load_data_t1())
  })
  

  
  
  #---------------------------------------------------------------------------------  
}






# Run the application
shinyApp(ui = ui, server = server)