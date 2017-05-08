# Simple header -----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plyr)
library(data.table)
library(DT)
load("Fire.year.stat.RData")
load("Fire.time.stat.RData")
load("death.month.week.stat.RData")
load("death.fire.RData")
load("fifty_states.RData")
load("state.df.RData")

header <- dashboardHeader(title="Fire Analysis")

# Sidebar --------------------------------------------------------------

sidebar <- ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Accident State Analysis",
        tabName = "State",
        icon = icon("dashboard")
      ),
      menuItem(
        "Fire Occurrence Analysis",
        tabName = "HappenTimes",
        icon = icon("th")
      ),
      menuItem(
        "Fire Reason Analysis",
        tabName = "FireType",
        icon = icon("dashboard")
      ),
      menuItem(
        "Fire Death Analysis",
        tabName = "Death",
        icon = icon("dashboard")
      )
    )
  )

# Compose dashboard body --------------------------------------------------

body <- dashboardBody(
  tabItems(
    
    # First tab content
    # Boxes need to be put in a row (or column)
    tabItem(
      tabName = "HappenTimes",
      fluidPage(
        selectInput("TopN", "The TopN of Fire Accident", choices = c(10, 20, 30, 40)),
        splitLayout(
          verticalLayout(selectInput("Year1", "Year", choices = c(2015:2006)),
                         plotlyOutput("p1")),
          verticalLayout(selectInput("Year2", "Year", choices = c(2006:2015)),
                         plotlyOutput("p2"))
        )
        ),
      fluidRow(
        splitLayout(
            verticalLayout(
                           tableOutput('Year1'))
          ,
          verticalLayout(
            tableOutput('Year2')
          )
        )) # End of fluidRow
      ), # End of tabItem
    
    # Second tab content
    # Boxes need to be put in a row (or column)
    tabItem(
      tabName = "FireType",
      fluidRow(
          selectInput("FireTopN", "The TopN of Death Fire", choices = c(10, 20, 30, 40)),
          h2("The Reason for Fire by Year"),
          plotlyOutput("p3"),
          # tableOutput('dt1')
          DT::dataTableOutput('dt1')
        # End of tabBox
      ) # End of fluidRow,
    ), # End of tabItem
    
    # Third tab content
    # Boxes need to be put in a row (or column)
    tabItem(
      tabName = "Death",
      fluidRow(h2("The Number of Death In Fire"), status = "info",
               plotlyOutput("p5_1")),
      fluidRow(h2("The Number of Happen Times In Death Fire"), status = "info",
               plotlyOutput("p5_2")),
      fluidRow(h2("The Number of Death year by weekends"), status = "info",
               plotlyOutput("p6")),
      fluidRow(
        box(
          h2("The Number of Death by Month"),
          plotlyOutput("p7")
        ),
        box(
          h2("The Number of Death by weekends"),
          plotlyOutput("p8")
        )
      ) # End of fluidRow
    ), # End of tabItem
    
    # Fourth tab content
    # Boxes need to be put in a row (or column)
    tabItem(
      tabName = "State",
      h2("The Fire Times by State"),
      DT::dataTableOutput('dt2'),
      selectInput("Year_map_choose", "Year", choices = c(2015:2006)),
      plotlyOutput("Year_map", height = 600, width = 1300)
    )
  ) ## End tabItems
) ## End dashboardBody




# Setup Shiny app UI components -------------------------------------------

ui <- dashboardPage(header, sidebar, body, skin = "purple")

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output) {
  formulaText1 <- reactive({
    paste0("The Reason for Fire in ", input$Year1)
  })
  
  formulaText2 <- reactive({
    paste0("The Reason for Fire in ", input$Year2)
  })

  # Return the formula text for printing as a caption
  output$caption1 <- renderText({
    formulaText1()
  })
  output$caption2 <- renderText({
    formulaText2()
  })
  
  output$Year1 <- renderTable({
    Fire.year.stat %>%
      filter(year == input$Year1) %>%
      arrange(desc(n)) %>%
      head(as.numeric(input$TopN)) %>%
      ungroup %>%
      select(year, code_descr, n) %>%
      mutate(Percent = paste0(round(n / sum(n)*100,2),"%")) %>%
      rename(c("code_descr" = "Fire Accident", "n" = "times"))
  })
  output$Year2 <- renderTable({
    Fire.year.stat %>%
      filter(year == input$Year2) %>%
      arrange(desc(n)) %>%
      head(as.numeric(input$TopN)) %>%
      ungroup %>%
      select(year, code_descr, n) %>%
      mutate(Percent = paste0(round(n / sum(n)*100,2),"%")) %>%
      rename(c("code_descr" = "Fire Accident", "n" = "times"))
  })
  output$Year_map <- renderPlotly({
    year.state.df <- state.df %>%
      filter(year == input$Year_map_choose) %>%
      mutate(sum = ifelse(is.na(sum), 0, sum)) %>%
      ungroup
    p <-  year.state.df %>%
      ggplot() + 
      geom_polygon(aes(x=long, y=lat, group=group), data=fifty_states, fill="beige", colour="grey60")
    p <- p + geom_point(data=year.state.df,aes(colour = sum, x = longitude, y= latitude),
                 size=30*year.state.df$sum/max(year.state.df$sum)) +
      geom_text(aes(label=state,x=longitude,y=latitude),size=3) +
      scale_colour_gradient("Fire Number",high="red",low="blue") +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
    ggplotly(p)
  })
  output$dt2 <- DT::renderDataTable({
    state.df %>%
      select(-code_descr, -id, -latitude, -longitude) %>%
      mutate(year = paste0("Y", year)) %>%
      spread(year, sum) %>%
      data.frame %>%
      arrange(desc(Y2015))
  })
  output$p1 <- renderPlotly({
    Fire.year.stat %>%
      filter(year == input$Year1) %>%
      arrange(desc(n)) %>%
      head(input$TopN) %>%
      plot_ly(type="pie", labels = ~code_descr,
              values = ~n, textinfo="percent",
              showlegend=F)
  })
  output$p2 <- renderPlotly({
    Fire.year.stat %>%
      filter(year == input$Year2) %>%
      arrange(desc(n)) %>%
      head(input$TopN) %>%
      plot_ly(type="pie", labels = ~code_descr,
              values = ~n, textinfo="percent",
              showlegend=F)
  })
  
  output$p3 <- renderPlotly({
    Fire.year.stat %>%
      group_by(year) %>%
      top_n(as.numeric(input$FireTopN)) %>%
      plot_ly(x= ~year, y = ~n,linetype = ~code_descr) %>%
        layout(mode = "markers", xaxis = list(title = "Year"), yaxis = list(title = "Happen Times"))
  })
  output$dt1 <- DT::renderDataTable({
    Fire.year.stat %>%
      group_by(year) %>%
      top_n(as.numeric(input$FireTopN)) %>%
      select(-inc_type) %>%
      rename(c("code_descr" = "Fire Accident")) %>%
      spread(year, n) 
  })
  output$p5_1 <- renderPlotly({
    Fire.year.stat %>%
      inner_join(death.fire) %>%
      plot_ly(x= ~year, y = ~death.num,linetype = ~code_descr) %>%
      layout(mode = "markers", xaxis = list(title = "Year"), yaxis = list(title = "Death Number In Fire"))
  })
  output$p5_2 <- renderPlotly({
    Fire.year.stat %>%
      inner_join(death.fire %>% ungroup %>%
                   select(inc_type) %>% unique) %>%
      plot_ly(x= ~year, y = ~n,linetype = ~code_descr) %>%
      layout(mode = "markers", xaxis = list(title = "Year"), yaxis = list(title = "Happen Times Death Fire"))
  })
  
  output$p6 <- renderPlotly({
    death.month.week.stat %>%
      plot_ly(x= ~month, y = ~death.num, color = ~weekday) %>%
      add_bars() %>%
      layout(barmode = "stack", xaxis = list(title = "Month"), yaxis = list(title = "Person Death Num"))
  })
  output$p7 <- renderPlotly({
    death.month.week.stat %>%
      plot_ly(x= ~month, y = ~death.num, color = ~month) %>%
      add_bars() %>%
      layout(barmode = "stack",showlegend = FALSE, xaxis = list(title = "Month"), yaxis = list(title = "Person Death Num"))
  })
  output$p8 <- renderPlotly({
    death.month.week.stat %>%
      plot_ly(x= ~weekday, y = ~death.num, color = ~weekday) %>%
      add_bars() %>%
      layout(barmode = "stack",showlegend = FALSE, xaxis = list(title = "Weekday"), yaxis = list(title = "Person Death Num"))
  })
}


