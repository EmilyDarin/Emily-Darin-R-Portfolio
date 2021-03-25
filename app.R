#
###SB Kelp Data Shiny App
###File created: December 1st, 2020
#
#
#
#
#This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#### Setup ####

# Anything that should only happen ONCE should be placed in this setup section, prior to the actual shiny structure.

# Load packages
library(tidyverse) #General everything
library(RColorBrewer)
library(ggplot2) #General plotting
library(ggrepel) #For adding text labels that repel away from data points
library(shiny) #Runs shiny
library(shinythemes) #Shiny theme for the page
library(shinyWidgets) #Widgets
library(scales) #SSD - Use the percent format
library(reshape2) #Overview tab - melts bars together
library(ssdtools) #SSD package
library(DT) #Build HTML data tables
library(plotly) #Make plots interactive
library(viridis) #Colors
library(scales) #To use "percent" function
library(shinyjs) #Exploration tab - reset button
library(calecopal) #color palettes 
library(viridis)#longer color palettes
library(plyr)
library(ggbeeswarm)
library(cowplot)

    # Load all finalized datasets.
sb_fish<- read_csv("sbc_lter_fish.csv", guess_max = 10000)
sbc_lter_all <- read_csv("sbc_lter_all.csv")
sb_stat_wide <- read_csv("sbc_lter_all_wide.csv")

#### Introduction Setup ####

# All text inputs below.


#### Species Exploration Setup ####

sb_setup<-sb_fish%>% # rename datset 
  mutate(org_f = factor(common_name))%>% #factor common names for organism widget/graph
  mutate(year_f = factor(year))%>% #factor common names for year widget
  mutate(site_f = factor(site)) #factor common names for site widget 
  
sb_new<-sbc_lter_all%>%
  mutate(fun_f = factor(case_when(
    funct_group == "fleshy.algae" ~ "Fleshy Algae",
    funct_group == "giant.kelp" ~ "Giant Kelp",
    funct_group == "hebivorous.fish" ~ "Herbivorous Fish",
    funct_group == "herbivorous.invert" ~ "Herbivorous Invert",
    funct_group == "lg.inv.eating.fish" ~ "Large Invertebrate-Eating Fish",
    funct_group == "mobile.sm.inv.eating.fish" ~ "Small Mobile Invertebrate-Eating Fish",
    funct_group == "omnivorous.invert" ~ "Omnivorous Invertebrate",
    funct_group == "piscivores" ~ "Piscivores",
    funct_group == "planktontivorous.fish" ~ "Plankton-Eating Fish",
    funct_group == "predatory.invert" ~ "Predatory Inverts",
    funct_group == "sessile.planktivorous.invert" ~ "Sessile Plankton-Eating Inverts",
    funct_group == "territor.sm.inv.eating.fish" ~ "Small Territorial Invertebrate-Eating Fish")))%>%
  mutate(site_s=factor(site))


#### Taxonomy Setup ####

sbc_lter_all%>%
  group_by(species)

text<-sbc_lter_all%>%
  filter(taxa=="FISH")


#### User Interface ####

ui <- fluidPage(theme = shinytheme("cosmo"),  
                
                # App title
                titlePanel(h1("Santa Barbara Kelp Forest Data")),
                
                # Title panel subtext
                tags$div("This website is only intended for use by BIO 455/555 Professor Dr. Allen Bengt and Emily Darin"),
                
                br(), # line break
                
                # Main panel for displaying outputs
                mainPanel(width = 12,
                          
                          # Output: set of 6 tabs
                          tabsetPanel(type = "tabs",
                                      
    #### Introduction UI ####        
                                      tabPanel("1: Introduction", 
                                               
                                               br(), 
                                               h3("What is the Santa Barbara Kelp Database?", align = "center"), #Section 1
                                               
                                               strong(p("This database is a collection of data visualizations representing different kelp forest data sets on the Santa Barbara Channel.")), 
                                               
                                               p("This web application allows users to explore the Santa Barba Channel Kelp Forest LTER data visually. The aim of this project is to explore different trends in the kelp forest population.  "),
                                               
                                               strong(p("Naviagating the app")),
                                      
                                               p("Feel free to navigate through different tabs and play with the data. If you are interested in a certain piece of the data, set your preferences and click download. There are multiple widgets, so click on as many things as possible to see what they do! "),
                                               
                                
                        
                                               p("Full datsets may be found at links provided in the resource tab. "),
                                               
                                              #Logos with links to organizations
                                              
                                              
                                              
                                               
                                               br(), 
                                               
                                               verbatimTextOutput(outputId = "Introduction1")),
                                      
  
                                      
#### Species Exploration UI ####
                                      tabPanel("2: Exploration: Santa Barbara Channel",
                                               shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                                               id = "Emily-tab", # adds ID for resetting Heili's tab's filters
                                               
                                               h3("Exploration of Kelp Forest Data from the Santa Barbara Channel", align = "center"),
                                               br(), # line break
                                               p("These figures display the average counts of fish by species or functional group over the 20 year time-series." ),
                                               br(),
                                               p("The data in these figures has been averaged, mean counts are reperesented by each species with the bar and sites can be read on the X-axis."),
                                               br(), # line break
                                               p("Filter the data: The data may be filtered using the drop-down menus located below. Then, click the 'Update Filters' button to refresh the data displayed according to your selections."),
                                               br(), # line break
                                               p("Download the data: Click the 'Download Data' button to retrieve the selected dataset as a '.csv' file."),
                                               br(), # line break
                                               
                                               
                                             
                                               
                                               # widgets
                                               column(width = 12,
                                                      
                                                      
                                                      column(width = 3,
                                                             pickerInput(inputId = "year_check", # year checklist
                                                                         label = "Year:", 
                                                                         choices = levels(sb_setup$year_f),
                                                                         selected = levels(sb_setup$year_f),
                                                                         options = list(`actions-box` = TRUE), # option to de/select all
                                                                         multiple = TRUE)), # allows for multiple inputs
                                                      
                                                      
                                                      
                                                      
                                                      column(width = 3,
                                                             pickerInput(inputId = "site_check", # year checklist
                                                                         label = "Site:", 
                                                                         choices = levels(sb_setup$site_f),
                                                                         selected = levels(sb_setup$site_f),
                                                                         options = list(`actions-box` = TRUE), # option to de/select all
                                                                         multiple = TRUE)), # allows for multiple inputs
                                                      
                                                      column(width = 3,
                                                             pickerInput(inputId = "organism_check", # organism checklist
                                                                         label = "Organism:", 
                                                                         choices = levels(sb_setup$org_f),
                                                                         selected = levels(sb_setup$org_f),
                                                                         options = list(`actions-box` = TRUE), # option to de/select all
                                                                         multiple = TRUE))), # allows for multiple inputs
                                                      
                                                      
                                               column(width=12,
                                                      column(width = 3,
                                                             actionButton("go", "Apply Changes", class = "btn-success"))),
                                               
                                               column(width = 12,
                                                      
                                                      column(width = 12,
                                                             plotOutput(outputId = "organism_plot_react"),
                                                             br())),
                                               
                                               column(width = 3,
                                                      downloadButton("downloadData", "Download Data", class = "btn-info")), # adds download button
                                               

                                               column(width = 12,
                                                      
                                                      
                                                      column(width = 3,
                                                             pickerInput(inputId = "fun_check", # year checklist
                                                                         label = "Functional Groups:", 
                                                                         choices = levels(sb_new$fun_f),
                                                                         selected = levels(sb_new$fun_f),
                                                                         options = list(`actions-box` = TRUE), # option to de/select all
                                                                         multiple = TRUE)), # allows for multiple inputs
                                                      
                                                      
                                                      
                                                      
                                                      column(width = 3,
                                                             pickerInput(inputId = "site_stat_check", # year checklist
                                                                         label = "Site:", 
                                                                         choices = levels(sb_new$site_s),
                                                                         selected = levels(sb_new$site_s),
                                                                         options = list(`actions-box` = TRUE), # option to de/select all
                                                                         multiple = TRUE))), # allows for multiple inputs\
                                               
                                              
                                               
                                               
                                               column(width = 12,
                                                      
                                                      column(width = 12,
                                                             plotOutput(outputId = "stat_plot_react"),
                                                             br()))),

                                             


                                              
    #### Taxonomy UI ####                                         
                                           
tabPanel("3: Taxonomy", 
         br(), 
         h3("SB Data Taxonomic Grouping"),
         br(),
         p("This figure shows the taxonimic levels by site and count. This was important to me because it shows overall species richness. I then chose to specifically look at one taxanomic group that I found interesting."), 
         br(),
         p("Detailed descriptions of data categories may be found in the links in the Resources tab."),
         br(),
         
         
         #selectInput(inputId = "plot.type", "Plot Type:", 
                     #list(boxplot = "boxplot", violin = "violin", beeswarm = "beeswarm") #need to fix, just comment out for now
         #),
         
         
         
         column(width = 12,
                
                column(width = 12,
                       plotOutput(outputId = "taxa_boxplot"),
                       br())),
         
         
         column(width = 12,
                
                column(width = 12,
                       plotOutput(outputId = "taxa_fish"),
                       br())),
         
         
         
         
         

         
         
         uiOutput(outputId= "Emily_plot")),


                                      
    #### Contact UI ####
                                      
                                      tabPanel("4: Resources",
                                               h3("The database for this graph can be found at the link https://sbclter.msi.ucsb.edu/data/catalog/ ", align = "center"),
                                               br(), # line break
                                      
                                               
                                               verbatimTextOutput(outputId = "Emily3"))

)))
                                      
                                      


#### Server ####

server <- function(input, output) {
    
    

    #### Species Exploration Server ####
    
    
    # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
    sbf_filter <- eventReactive(list(input$go),{
        # eventReactive explicitly delays activity until you press the button
        # use the inputs to create a new dataset that will be fed into the renderPlot calls below
        
        #every selection widget should be represented as a new variable below
        org_c <- input$organism_check
        site_c <- input$site_check
        year_c <- input$year_check
        # assign organism input values to "org_c"
        
        
        
        sb_setup %>% # take original dataset
            filter(org_f %in% org_c)%>%   # filter by organism inputs
            filter(site_f %in% site_c)%>% #filter by site
            filter(year_f %in% year_c)
            
    })
    
    
    
    #Organism plot
    
    output$organism_plot_react <- renderPlot({
        
      ggplot(sbf_filter(), aes(x = site, y = count, fill= org_f)) +
        stat_summary(fun.y=mean, geom = "bar", position="dodge")+
        ggtitle("Common Species") +
        xlab("Site") + 
        labs(fill = "Species")+
        ylab("Number of Individuals")+
        scale_fill_viridis(discrete = TRUE, option = "D")+
        theme_classic()
     
        
    })
    
    
    #### Taxonomy S ####
    
    # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
    sbf_stat_filter <- eventReactive(list(input$go),{
      # eventReactive explicitly delays activity until you press the button
      # use the inputs to create a new dataset that will be fed into the renderPlot calls below
      
      #every selection widget should be represented as a new variable below
      site_s_c <- input$site_stat_check
      fun_c <- input$fun_check
     
      
      #year_c <- input$year_check
      # assign organism input values to "org_c"
      
      
      
      sb_new %>% # take original dataset
        filter(fun_f %in% fun_c)%>%   # filter by organism inputs
        filter(site_s %in% site_s_c)#%>% #filter by site
        #filter(year_f %in% year_c)
      
    })
    
    
    
    #functional group plot
    
    output$stat_plot_react <- renderPlot({
      
      ggplot(sbf_stat_filter(), aes(x = count, y = site_s, fill= fun_f)) +
      geom_boxplot(alpha = 0.7, aes(color= fun_f, fill = fun_f )) +
        scale_color_viridis( discrete = TRUE, option = "A")+
        ggtitle("Functional Groups") +
        xlab("Number of Individuals") + 
        ylab("Functional Groups")+
      scale_fill_viridis( discrete = TRUE, option = "A")+
      theme_classic()
      
      
      
      
      
      
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(sbf_stat_filter() %>%
                    select(-c(fun_f, site_s)), 
                  file, row.names = FALSE)
      }
    )
    
   output$taxa_boxplot <- renderPlot({
      
      ggplot(data = sbc_lter_all, aes(x=taxa, y=count, fill = site))+
        geom_boxplot(alpha = 0.7, aes(color= site, fill = site ))+
        ylim(0,500)+
        scale_fill_viridis(discrete = TRUE, option = "D")+
        scale_color_viridis(discrete = TRUE, option = "D")+
        xlab("Taxanomic Group")+
        ylab("Number of Individuals")+
        ggtitle("Taxonomic Groups")+
       theme_cowplot()
      
    })
   
   output$taxa_fish <- renderPlot({
     
     
     sbc_lter_all%>%
       filter(taxa=="FISH")%>%
     ggplot(aes(x=taxa, y=count, fill = species))+
       geom_boxplot(alpha = 0.7, aes(color= species, fill = species ))+
       scale_color_viridis(discrete = TRUE, option = "A")+
       scale_fill_viridis(discrete = TRUE, option = "A")+
       ylim(0,200)+
       xlab("Taxanomic Group")+
       ylab("Number of Individuals")+
       ggtitle("Fish Taxonomy")+
       theme_cowplot()
    
     
   })
    
   
   
} #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.
