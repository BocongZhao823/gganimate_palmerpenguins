
# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Shiny Themes,
# https://rstudio.github.io/shinythemes/

# Shiny tutorial Video
# https://www.youtube.com/watch?v=9uFQECk30kA

# Layout guide
# https://shiny.rstudio.com/articles/layout-guide.html

# Load R packages
library(shiny)
library(shinythemes)
library(palmerpenguins)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)
data(package = 'palmerpenguins')
myPenguins = penguins %>% na.omit()

library(rsconnect)
#rsconnect::removeAccount("actuarialstudent")
#rsconnect::setAccountInfo(name='bocongzhao', 
#                          token='1E9AE38A0205D3253B6C136C04C2C225', 
#                          secret='+6i7VToc+eThw6SKUMuzXPxDRwAaig5oXZyUyP3y')
#rsconnect::deployApp('C:/Users/mreal/Desktop/R')


#################
# User Interface
#################

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                # Display Navigation Bar 
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  
                  # Name of the Navigation Bar 
                  "Animated penguins data",
                  
                  #################
                  # 1st page
                  #################
                  
                  tabPanel("Basic plot", 
                           
                           # App title
                           h4("Select the appropriate variable from the drop down menu:"),
                           br(),
                           
                           ##########
                           
                           
                           fluidRow(
                             
                             column(3, offset = 1,
                                    selectInput("x", "X variable:", names(penguins), selected="flipper_length_mm"),
                                    selectInput("y", "Y variable", names(penguins), selected="bill_depth_mm"),
                             ),
                             
                             column(3, 
                                    selectInput("colour", "Colour: ", names(penguins), selected="sex"),
                                    selectInput("size", "Size: ", names(penguins), selected="bill_length_mm"),
                                    selectInput("state", "State for transition_states: ", names(penguins), selected="island")
                             ), 
                             
                             column(3,
                                    selectInput("facetRows", "Facet rows: ", names(penguins), selected="species"),
                                    selectInput("facetCols", "Facet columns", names(penguins), selected="island")
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow( 
                             column(6, imageOutput('plot1', width = "30%", height = "30%")),
                             column(6, imageOutput('plot', width = "30%", height = "30%")),
                             column(6, imageOutput('plot3', width = "30%", height = "30%")),
                             column(6, imageOutput('plot2', width = "30%", height = "30%"))
                           )
                           
                           
                           
                           #########
                           
                           
                           
                  ) #,
                  
                  #################
                  # 2nd page
                  #################
                  # Component of tabPanel 
                  #tabPanel("Another plot",
                           
                  #         "Cool stuff pending..."
                  #)
                  
                ) # navbarPage
                
) # fluidPage


#############
# server
#############

# Define server function  
server <- function(input, output) {
  
  ##########################################
  output$plot <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p = myPenguins %>%
      ggplot(
        aes(x=!!sym(input$x), 
            y=!!sym(input$y), 
            colour=!!sym(input$colour)#,
            #size = !!sym(input$size)
            ) 
      ) + 
      geom_point(alpha = 0.8) + 
      #geom_line() +
      facet_grid(rows=vars(!!sym(input$facetRows)), cols=vars(!!sym(input$facetCols)))+
      theme_bw()+
      #theme_minimal() +
      transition_time(year)+
      labs(title = "Year: {frame_time}")+
      
      view_follow()#+
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif')
    
  }, deleteFile = TRUE)
  
  
  ################################################################
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p1 = myPenguins %>%
      ggplot(
        mapping = aes(
          x=!!sym(input$x), 
          y=!!sym(input$y),
          size = !!sym(input$size)
        )
      ) +
      geom_point(alpha = 0.8, aes(colour=!!sym(input$colour) ))+
      theme_bw()+
      #theme_minimal() +
      transition_time(year)+
      labs(title = "Year: {frame_time}")+
      view_follow()#+
      #shadow_wake(wake_length = 0.5)
    
    anim_save("outfile1.gif", animate(p1)) # New
    
    # Return a list containing the filename
    list(src = "outfile1.gif",
         contentType = 'image/gif')
    
  }, deleteFile = TRUE)
  #################################################################
  output$plot2 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p2 = myPenguins %>%
      ggplot(data = myPenguins, 
             mapping = aes(x=!!sym(input$x), y=!!sym(input$y))) +
      geom_boxplot(aes(colour=!!sym(input$colour)))+
      transition_time(year)+
      labs(title = "Year: {frame_time}")+
      view_follow()
    
    anim_save("outfile2.gif", animate(p2)) # New
    
    # Return a list containing the filename
    list(src = "outfile2.gif",
         contentType = 'image/gif')
    
  }, deleteFile = TRUE)
  ######################################################
  
  output$plot3 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p3 = myPenguins %>%
      ggplot(
        mapping = aes(
          x=!!sym(input$x), 
          y=!!sym(input$y),
          size = !!sym(input$size)
        )
      ) +
      geom_point(alpha = 0.8, aes(colour= !!sym(input$colour)))+
      theme_bw()+
      transition_states(!!sym(input$state), transition_length = 3, state_length = 1)+
      labs(title = "{closest_state}")
    
    anim_save("outfile3.gif", animate(p3)) # New
    
    # Return a list containing the filename
    list(src = "outfile3.gif",
         contentType = 'image/gif')
    
  }, deleteFile = TRUE)
  #################################################################
}

#############
# Shiny App
#############

# Create Shiny object
shinyApp(ui = ui, server = server)

