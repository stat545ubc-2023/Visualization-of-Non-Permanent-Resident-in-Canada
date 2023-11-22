# lib --------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(DT)
library(maps)

# data pre-processing ----------------------------------------------------------

npr <- read.csv("dataset.csv") %>%
  # rename columns for convenience
  rename(region = Non.permanent.resident.type..10..4,
         total_record  = Total...Non.permanent.resident.type,
         NPR = Non.permanent.residents.6,
         Asylum_Claimants = Asylum.claimants.7,
         NPR_With_Work_Permit = Non.permanent.residents.with.a.work.permit.only.8,
         NPR_With_Study_Permit = Non.permanent.residents.with.a.study.permit.only.9,
         NPR_With_Work_and_Study_Permit = Non.permanent.residents.with.work.and.study.permits.10,
         Other_NPR_Type = Other.non.permanent.resident.types.11,
         PR = Not.a.non.permanent.resident,
         PR_Non_Immigrants = Non.immigrants.12,
         PR_Immigrants = Immigrants.13) %>%
  
  # delete commas and convert to numbers (e.g. 1,999,999 => 1999999)
  mutate(across(-region, ~as.numeric(gsub(",", "", .)))) %>% 
  
  # correct country names since some includes numbers (e.g. China 27 => China)
  mutate(across(region, ~gsub(" \\d+", "", .))) %>% 
  
  # correct country names that matches world map dataset
  mutate(
    region = case_when(
      region == "United States of America" ~ "USA",
      region == "Russian Federation" ~ "Russia",
      region == "Inside Canada" ~ "Canada",
      region == "Korea, North" ~ "North Korea",
      region == "Korea, South" ~ "South Korea",
      region == "Southern Africa" ~ "South Africa",
      region == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
      region == "Congo, Republic of the" ~ "Republic of Congo",
      region == "Côte d'Ivoire" ~ "Ivory Coast",
      region == "United Kingdom" ~ "UK",
      region == "Czechia" ~ "Czech Republic",
      region == "Viet Nam" ~ "Vietnam",
      TRUE ~ region
    )
  )

# load map data
world.map <- select(map_data("world"),-subregion)

# merge map with npr, and drop na
merged_data <- full_join(world.map, npr, by = 'region') %>%
  drop_na()

# delete Canadian provinces and total_record, which are not interested, and reorder the dataframe
npr_no_province <- npr[-c(1,4:16), ] %>% select(-total_record)
rownames(npr_no_province) <- NULL

# ui ---------------------------------------------------------------------------
ui <- fluidPage(
    
  titlePanel('Visualization of Non-Permanent Resident (NPR) in Canada'),
  fluidRow(column(3,offset = 0,'Shengyu Zuo # 23571953')),
    tabsetPanel(
    # first tab
    tabPanel("World View",
             h3("World Map Distribution"),
             sidebarLayout(
               sidebarPanel(
                 # description
                h4('This page offers a comprehensive view of the Canadian Permanent Resident (PR) 
                and Non-Permanent Resident (NPR) populations across the world.'),
                h4('Through an intuitive world map interface, you can clearly see the geographical distribution 
                of these categories, gaining insights into the concentration and spread of residents in 
                various regions.'),
                h4('You can also see the rank of different regions of the selected catergory.'),
                 
                 # selector input
                selectInput("fill_selector", "Select PR/NPR Catergories",
                             choices = c(
                               "NPR", "Asylum_Claimants",
                               "NPR_With_Work_Permit", "NPR_With_Study_Permit",
                               "NPR_With_Work_and_Study_Permit", "Other_NPR_Type",
                               "PR", "PR_Non_Immigrants", "PR_Immigrants"
                             ),
                             selected = "NPR_With_Work_Permit"),
                 # slider input
                sliderInput("rowSlider", "Select Number of Ranks:", 
                            min = 3, max = 10, value = 5, step = 1),
                 # output text
                textOutput("topx"),
                 # output table
                tableOutput("top10")
               ),
                 # output map plot
               mainPanel(
                 plotOutput("worldMap", height = "800px", width = "1440px")
               )
             )
    ),
    
    # second tab
    tabPanel("Country View",
             h3("Country Bar Plot"),
             sidebarLayout(
               sidebarPanel(
                 
                 h4('This page gives a bar plot that clearly shows the PR/NPR populations by regions.'),
                 h4('You can use the search bar below to select or type in the region you are interested in (including continents).'),
                 h4('You can also see the specific numerical data in the plot.'),
                 
                 # selectize input
                 selectizeInput(
                   'variable', label = 'Region', 
                   choices = unique(npr_no_province$region),
                   options = list(placeholder = 'Select a Region',
                                  onInitialize = I('function() { this.setValue(""); }'))
                 )
               ),
               mainPanel(
                 # plot bar plot
                 plotOutput("country", height = "800px", width = "1440px"),
                 
               )
             )
    ),
    
    # third tab
    tabPanel("Table View",
             h3("Interactive Table & Download"),
             sidebarLayout(
               sidebarPanel(
                 
                 h4('This page provides an interactive table that you can explore and download.'),
                 
               ),
               mainPanel(
                 # output table and download button
                 DT::dataTableOutput("select"),
                 downloadButton("downloadData", "Download as CSV")
                 
               )
             )
    )
    ),
  
  
  # dataset reference
  fluidRow(
    column(
      4, 
      tags$hr(), 
      tags$p(
        tags$strong("Resource:"),
        "Statistics Canada: Non-permanent resident type by place of birth: Canada, provinces and territories, census metropolitan areas and census agglomerations with parts.",
        "Link:",
        tags$a(
          href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810036101&geocode=A000011124",
          "Statistics Canada Link"
        )
      )
    )
  ),
  
  
    )

# server------------------------------------------------------------------------
server <- function(input, output) {
  # world map (tab1) plot
  output$worldMap <- renderPlot({
    ggplot(merged_data, aes(x = long, y = lat, fill = .data[[input$fill_selector]], group = group)) +
      geom_polygon(colour = "gray") +
      scale_fill_viridis_c(option = "plasma",alpha = 0.5)+
      theme(aspect.ratio = 9/16,panel.grid = element_blank(), 
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent"),
            legend.text = element_text(size = 12))+
            labs(x = "Longitude", y = "Latitude") +
            ggtitle('Map View Distribution')
  })
    
  # tab1 top10 table
  output$top10 <- renderTable({
      merged_data %>%
      group_by(region)%>%
      summarize(value = as.integer(mean(.data[[input$fill_selector]])))%>%
      arrange(desc(value)) %>%
      head(input$rowSlider)
  })
  
  # tab1 topx text display
  output$topx <- renderText({
    paste('Top', input$rowSlider, 'regions of selected category')
  })
  
  # tab2 data processing & bar plot
  output$country <- renderPlot({
    df_sub <- subset(npr_no_province, region == input$variable)
    df_long <- df_sub %>%
    pivot_longer(cols = -c(region), names_to = "Variable", values_to = "Value")
    
    ggplot(df_long) + 
      geom_bar(stat = "identity",aes(x = Variable, y = Value),fill = '#8D83C4') +
      geom_text(aes(x = Variable, y = Value, label = Value), vjust = -0.5)+
      theme_classic()+
      theme(aspect.ratio = 9/16)+
      labs(x = "Resident Status", y = "Count") +
      ggtitle('Bar plot of Resident Status by Region')
      
  })
  
  # tab3 interactive table
  output$select <- DT::renderDataTable({
    npr_no_province
  })
  
  # tab3 downloadable table
  output$downloadData <- downloadHandler(
    filename = function() {
      "NPR.csv"  # Set the filename for the downloaded CSV file
    },
    content = function(file) {
      # Write the data to a CSV file
      write.csv(npr_no_province, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)