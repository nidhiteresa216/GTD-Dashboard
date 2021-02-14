library(bs4Dash)
library(fresh)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(shiny)
library(ECharts2Shiny)
library(leaflet)
library(threejs)
library(png)
library(dplyr)
library(treemap)
library(d3treeR)
library(datasets)
library(rstatix)
library(dslabs)
library(tidyverse)
library(gplots)
earth_dark <- list(img=system.file("images/world.jpg", package="threejs"),
                   bodycolor="#ffffff",
                   emissive="#000000",
                   lightcolor="#ffffff")
df <- RGP3
india_df<-data.frame()
for (i in 1:nrow(df)){
 if (df$country[i]==92 & df$provstate[i]!="Unknown"){
 india_df <- rbind(india_df,df[i,])
}
}


Dataset <- data.frame(
  Type = c(df$attacktype1),
  Value = c(df$iyear),
  Lat = c(df$latitude), 
  Long = c(df$longitude)
)
Dataset2 <- data.frame(
  Type1 = c(india_df$attacktype1),
  Value1 = c(india_df$iyear),
  Lat = c(india_df$latitude), 
  Long = c(india_df$longitude)
)




# Theme -------------------------------------------------------------------

bs4DashTheme <- create_theme(
  bs4dash_vars(
    navbar_dark_color = "#bec5cb",
    navbar_dark_active_color = "#FFF",
    navbar_dark_hover_color = "#FFF"
  ),
  bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30"),
  bs4dash_layout(main_bg = "#353c42"),
  bs4dash_sidebar_dark(
    bg = "#272c30", color = "#bec5cb", hover_color = "#FFF",
    submenu_bg = "#272c30", submenu_color = "#FFF", submenu_hover_color = "#FFF"
  ),
  bs4dash_status(dark = "#272c30"),
  bs4dash_color(gray_900 = "#FFF", white = "#272c30")
)

rock ="green"

# App ---------------------------------------------------------------------


ui <- bs4DashPage(
  title = "GTD DASHBOARD",
  # sidebar_collapsed = FALSE,
  navbar = bs4DashNavbar(skin = "dark"),
  controlbar = bs4DashControlbar(
    skin = "dark",
    "This is the control bar"
  ),
  sidebar = bs4DashSidebar(
    title = "GTD DASHBOARD",
    skin = "dark",
    bs4SidebarMenu(
      bs4SidebarHeader("Menu:"),
      bs4SidebarMenuItem(
        tabName = "home",
        text = "DESCRIPTION",
        icon = "info-circle"
      ),
      bs4SidebarMenuItem(
        tabName = "data",
        text = "WORLD-DATA SET",
        icon = "database"
      ),
      bs4SidebarMenuItem(
        tabName = "tab1",
        text = "WORLD DATA",
        icon = "globe"
      ),
      bs4SidebarMenuItem(
        tabName = "tab3",
        text = " 3D GLOBE",
        icon = "cube"
      ),
      bs4SidebarMenuItem(
        tabName = "tab2",
        text = "INDIA DATA",
        icon = "flag"
      ),
  
      bs4SidebarMenuItem(
        tabName = "tab4",
        text = "CONCLUSION",
        icon = "stop"
      ),
      bs4SidebarMenuItem(
        "Success/Failure", tabName="Success/Failure", icon="list-ul",
        radioButtons("success", label = h4(""),
                     choices = list("Failed attacks" = 0, "Successful Attacks" = 1, "All Attacks" = 2), 
                     selected = 2)
      ),
      bs4SidebarMenuItem(
        "Select the time period", tabName="Select the time period", icon="sliders-h",
        sliderInput(inputId = "years",
                    label = "Years:",
                    min = 1970,
                    max = 2017,
                    value =  c(1970,1990),
                    width = 200)
      ),
      
      
      bs4SidebarMenuItem(
        "World", tabName="World", icon="globe-asia",
        radioButtons("regions", label = h3("Regions"),
                     choices = list("North America" = 1, "Central America & Caribbean" = 2, 
                                    "South America" = 3, "East Asia" = 4, "Southeast Asia" = 5, 
                                    "South Asia" = 6, "Central Asia"=7, "Western Europe" = 8, 
                                    "Eastern Europe" = 9, "Middle East & North Africa" = 10, 
                                    "Sub-Saharan Africa"= 11, "Australasia & Oceania"= 12, 
                                    "All Regions" = 13 ), 
                     selected = 1)
      ),
      
      bs4SidebarMenuItem(
        "Attack Types", tabName="Attack Types", icon="jedi",
        radioButtons( "Type", "Type", choices = c("Assassination" = 1,
                                                  "Armed Assault" = 2,
                                                  "Bombing/Explosion" = 3,
                                                  "Hijacking" = 4,
                                                  "Hostage Taking (Barricade Incident)" = 5,
                                                  "Hostage Taking (Kidnapping)" = 6,
                                                  "Facility/Infrastructure Attack" = 7,
                                                  "Unarmed Assault" = 8,
                                                  "Unknown" = 9 
        ), selected = 1 )
      )
    )
  ),
  
  body = bs4DashBody(
    
    use_theme(bs4DashTheme),
    
    bs4TabItems(
      bs4TabItem(
        tabName = "home",
        mainPanel(absolutePanel(width = 20,bottom = 1, right = -150, top = -100,
                                img(src = "https://i.ibb.co/TPJkFXT/GTD.png" , height = 1000, width = 400)
        ),
        h1(span(strong("The Global Terrorism Database", style = "color:red"))),
        h3(p("The Global Terrorism Database (GTD) is the most comprehensive unclassified database of terrorist attacks in the world. 
            The National Consortium for the Study of Terrorism and Responses to Terrorism (START) makes the GTD available via", 
             strong(em("https://www.start.umd.edu/gtd/ ")),
             "in an effort to improve understanding of terrorist violence, so that it can be more readily studied and defeated. 
            The GTD is produced by a dedicated team of researchers and technical staff."),align ="justify"),
        br(),
        h3("The GTD is an open-source database, which provides information on domestic and international terrorist attacks around the 
            world since 1970, and includes more than 180,000 events. For each event, a wide range of information is available, 
            including the date and location of the incident, the weapons used, nature of the target, the number of casualties, 
            and - when identifiable - the group or individual responsible. ", align ="justify"), 
        
        br(),
        h2(span(strong("Characteristics of the GTD ", style = 'color:red'))),
        tags$ul(
          tags$li(h3("Contains information on over 180,000 terrorist attacks.")),
          p(""),
          tags$li(h3("Currently the most comprehensive unclassified database on terrorist attacks in the world.",align ="justify")),
          p(""),
          tags$li(h3("Includes information on more than 95,000 bombings, 20,000 assassinations, and 15,000 kidnappings and hostage 
            events since 1970.")),
          p(""),
          tags$li(h3("Includes information on at least 45 variables for each case, with more recent incidents including information on 
            more than 120 variables.",align ="justify")),
          p(""),
          tags$li(h3("More than 4,000,000 news articles and 25,000 news sources were reviewed to collect incident data from 1998 to 2017 alone.",align ="justify")),)
        )#mainPanel
      ),
      
      #tabitem
      bs4TabItem(
        tabName = "data",
        tags$h4("DATA-SET"),
        fluidRow(
          bs4Card(
            title = "",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            gradientColor = "warning",
            collapsible = TRUE,
            div(DT::dataTableOutput("tbl"), style = 'overflow-x: auto'))
          
        )
        
      ),
      bs4TabItem(
        tabName = "tab1",
        bs4TabSetPanel(id = "tab", side = "center",
                       bs4TabPanel(tabName = "WORLD PANEL-1",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("WorldKilledBox",width = 4),
                                            bs4ValueBoxOutput("WorldInjuredBox",width = 4),
                                            bs4ValueBoxOutput("countryaffected",width = 4)
                                   ),
                                   tags$h4("MAP PLOT"),
                                   fluidRow(
                                     bs4Card(
                                       title = "WORLD MAP : ATTACKS TYPES",
                                       closable = FALSE,
                                       width = 12,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       leafletOutput(outputId = "map",width ="100%")
                                     )
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "TOTAL ATTACKS : REGION WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "piecountry")
                                     ),
                                     bs4Card(
                                       title = "TOP 10 TERRORIST GROUPS : NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       tableOutput(outputId = "terr_gp_data_by_region")
                                     )
                                   )) ,   
                       
                       bs4TabPanel(tabName = "WORLD PANEL-2",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("WorldKilledBox2",width = 4),
                                            bs4ValueBoxOutput("WorldInjuredBox2",width = 4),
                                            bs4ValueBoxOutput("countryaffected2",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "FATALITIES : REGION WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "piecountry2")
                                     ),
                                     bs4Card(
                                       title = "REGION WISE COUNT : NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "region")
                                     ),
                                     bs4Card(
                                       title = "TYPES OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "attacktype")
                                     ),
                                     bs4Card(
                                       title = "WEAPONS/METHODS USED",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       plotlyOutput("weapon")
                                     )
                                   )
                                   
                       ),
                       
                       bs4TabPanel(tabName = "WORLD PANEL-3",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("WorldKilledBox3",width = 4),
                                            bs4ValueBoxOutput("WorldInjuredBox3",width = 4),
                                            bs4ValueBoxOutput("countryaffected3",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "WORLD : TREE PLOT",
                                       closable = FALSE,
                                       width = 8,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       d3tree2Output("tree")
                                     )
                                   )))),
      

      bs4TabItem(
        tabName = "tab2",
        bs4TabSetPanel(id = "tabs", side = "center",
                       bs4TabPanel(tabName = "INDIA PANEL-1",
                                   tags$h2("THE INDIA TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("KilledBox1",width = 4),
                                            bs4ValueBoxOutput("InjuredBox1",width = 4),
                                            bs4ValueBoxOutput("Indiaaffected",width = 4)
                                   ),
                                   tags$h4("MAP PLOT"),
                                   fluidRow(
                                     bs4Card(
                                       title = "MAP : ATTACK TYPES",
                                       closable = FALSE,
                                       width = 12,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       leafletOutput(outputId = "indiamap",width ="100%")
                                     )
                                   ),
                                   
                                   tags$h4(""),
                                   fluidRow(
                                     
                                     bs4Card(
                                       title = "TOTAL ATTACKS : STATE WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "pieindia")
                                     ),
                                     bs4Card(
                                       title = "TOP 10 TERRORIST GROUPS: NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       tableOutput(outputId = "India_terr_gp")
                                     )
                                   )) ,   
                       
                       bs4TabPanel(tabName = "INDIA PANEL-2",
                                   tags$h2("THE INDIA TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("KilledBox2",width = 4),
                                            bs4ValueBoxOutput("InjuredBox2",width = 4),
                                            bs4ValueBoxOutput("Indiaaffected2",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "FATALITIES : STATE WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "pieindia2")
                                     ),
                                     bs4Card(
                                       title = "INDIAN STATEWISE COUNT : NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "India_state_wise")
                                     ),
                                     bs4Card(
                                       title = "TYPES OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "India_attacktype")
                                     ),
                                     bs4Card(
                                       title = "WEAPONS/METHODS USED ",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       plotlyOutput("India_weapon")
                                     )
                                   )
                       ),
                       
                       bs4TabPanel(tabName = "INDIA PANEL-3",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("KilledBox3",width = 4),
                                            bs4ValueBoxOutput("InjuredBox3",width = 4),
                                            bs4ValueBoxOutput("Indiaaffected3",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "INDIA:TREE PLOT",
                                       closable = FALSE,
                                       width = 8,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       d3tree2Output("indiatree")
                                     )
                                   )
                                   
                       ))),
      bs4TabItem(
        tabName = "tab3",
        fluidRow(
          bs4Card(
            title = "3D GLOBE",
            closable = FALSE,
            width = 12,
            height = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            globeOutput("globe")
          )
        )
        
      ),
      bs4TabItem(
        tabName = "tab4",
        fluidRow( 
        bs4Card(
          title = "",
          closable = FALSE,
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          h2(span("CHI-SQUARE TEST OF INDEPENDENCE", style = 'color:red')),
          tags$ul(
            h3("The" ,strong ("Chi-square test of independence ")," compares two variables in a contingency table to see if they are related. In a more general sense, it tests to see whether distributions of categorical variables differ from each another. ", align ="justify"),
            p(""),
            h3("The Chi-square test of independence is used to analyse the frequency table (i.e. contingency table) formed by two categorical variables.",align ="justify"),
            br(),
            h3("The chi-square statistic is represented by the expression - ."),
            img(src = "https://i.ibb.co/17zh7ZJ/Picture9.png" , height = 100, width = 400),
            br(),
            br(),
            h3("C is the Degrees of Freedom, Oi s are the Observed Frequencies and Ei s are the Expected Frequencies [Degree of freedom = (No of rows - 1)*(no of columns -1)].",align ="justify"),
            br(),
            h3(strong ("Null hypothesis:"),"There is no relationship between the categorical variables.  "),
            p(""),
            h3(strong ("Alternative hypothesis:")," There is a relationship between the categorical variables."),
            br(),
            h3("Here, the Level of Significance (alpha) is important. "),
            br(),
            h3("If p-value <= alpha : Significant result -> reject null hypothesis (H0)"),
            p(""),
            h3("If p-value > alpha : Not a significant result -> fail to reject null hypothesis (H0)"),
           br(),
           br(),
            h2(span("(I) Chi-Square test of Independence for Attack Types and Success" , style = "color:red")),
             p(""),
            h3(strong("H0:"), "Attack types and success are independent."),
            p(""),
            h3(strong("H1:"), "Attack types and success are not independent."),
           br(),
           br(),
            h2(span("Contingency Table for Attack Type and Success" , style = "color:red")),
            img(src = "https://i.ibb.co/NrvmFCT/Picture2.png" , height = 400, width = 700),
           br(),
           br(),
             h2(span("Chi-Square test", style = "color:red")),
            img(src = "https://i.ibb.co/2jBnhdk/Picture3.png" , height = 200, width = 800),
            h3(strong("The P-value (2.2e-16) is less than the significance level (0.05). Therefore, we reject the null hypothesis. ")),
           br(),
           br(),
            h2(span("(II) Chi-Square test of Independence for Weapon types and success", style = "color:red" )),
            p(""),
            h3(strong("H0:"), "Weapon types and success are independent. "),
            p(""),
            h3(strong("H1:"), " Weapon types and success are not independent. "),
           br(),
           br(),
            h2(span("Contingency Table for Weapon type and Success", style = "color:red")),
            img(src = "https://i.ibb.co/jWdQD4G/Picture4.png" , height = 400, width = 700),
           br(),
           br(),
            h2(span("Chi-Square test" , style = "color:red")),
            img(src = "https://i.ibb.co/H4W2tKG/Picture5.png" , height = 200, width = 800),
            p(""),
            h3(strong("The P-value (2.2e-16) is less than the significance level (0.05). Therefore, we reject the null hypothesis. ")),
            p(""),
           h3(strong("Contingency table can be visualized using the function balloonplot() in gplots package. This function draws a graphical matrix where each cell contains a dot whose size reflects the relative magnitude of the corresponding components. ", align ="justify")),
        )),
        bs4Card(
          title = "ATTACK  TYPES  VS  SUCCESS",
          closable = FALSE,
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          plotOutput(outputId = "attacks_chi")
        ),
        bs4Card(
          title = "WEAPON  TYPES  VS  SUCCESS",
          closable = FALSE,
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          plotOutput(outputId = "weapon_chi")
        )
          
          
          ))
      
    )
    
  )
)


server <- function(input, output, session) {
  
  
  h <- 150
  # height of the bar
  
  cull <- reactive({
    firstData <- 
      filter(df[order(df$attacktype1 == input$Type, decreasing=TRUE)[df$iyear >= input$years[1] & df$iyear <= input$years[2]],])
    if(!is.null(input$regions!=13)){
      
      firstData <- filter(
        firstData,region %in% input$regions
      )
      return(firstData)
    }
    
  })
  
  
  values <- reactive({
    attack <- cull()
    value <- h * attack$attacktype1 /max(attack$attacktype1)
    col <- rainbow(10, start=2.8/5, end=3.4/25)
    names(col) <- c()
    # Extend palette to data values
    col <- col[floor(length(col) * (h - value)/ h)+1]
    list(value=value, color=col ,attack=attack)
  })
  
  filteredData <- reactive({
    Dataset %>% 
      filter(Type %in% input$Type) %>%
      filter(Value >= input$years[1]) %>% 
      filter(Value <= input$years[2]) 
  })
  filteredData2 <- reactive({
    Dataset2 %>% 
      filter(Type1 %in% input$Type) %>%
      filter(Value1 >= input$years[1]) %>% 
      filter(Value1 <= input$years[2]) 
  })
  
  df %>%
    dplyr::group_by(
      country_txt = df$country_txt,
      iyear = df$iyear,
    )%>%
    tally() 
  
  india_df %>%
    dplyr::group_by(
      provstate = india_df$provstate,
      iyear = india_df$iyear,
    )%>%
    tally() 
  
  output$tbl <- DT::renderDataTable(datatable(df[, rep(1:65)]))
  
  output$showData2 <- renderDataTable(
    india_df,
    options = list(
      pageLength = 10,
      scrollY="400p"
      
    )
  )
  
  output$WorldKilledBox <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$WorldInjuredBox <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$countryaffected <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$attacktype1[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(df$attacktype1[ df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  
  output$WorldKilledBox2 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$WorldInjuredBox2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$countryaffected2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$attacktype1[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(df$attacktype1[ df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  
  output$WorldKilledBox3 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$WorldInjuredBox3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$countryaffected3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$attacktype1[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(df$attacktype1[ df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  output$KilledBox1 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$InjuredBox1 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$Indiaaffected <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$attacktype1[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(india_df$attacktype1[ india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  output$KilledBox2 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$InjuredBox2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$Indiaaffected2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$attacktype1[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(india_df$attacktype1[ india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  
  output$KilledBox3 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$InjuredBox3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$Indiaaffected3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$attacktype1[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(india_df$attacktype1[ india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  ###Region wise count of attacks
  output$region <- renderPlotly({
    if (input$success==2) {
      y<-data.frame(table(df$region_txt[df$iyear>=input$years[1] & df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(df$region_txt[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("blue"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig
  })
  output$map <- renderLeaflet({
    leaflet(Dataset) %>% addTiles() %>%
      addMarkers(data = filteredData(), clusterOptions = markerClusterOptions())
    
    leaflet(Dataset)%>% addTiles() %>% 
      addMarkers(data = filteredData(), clusterOptions = markerClusterOptions(freezeAtZoom = 6))
    leaflet(Dataset) %>%  addTiles() %>% 
      addLabelOnlyMarkers(data = filteredData(),
                          lng = ~Long, lat = ~Lat,
                          label = ~as.character(filteredData()),
                          clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(multiple = T,
                                                      direction = "auto"))
  })   
  
  output$piecountry <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$region==input$regions)
      }
      else {#All regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$success==input$success, df$region==input$regions)
      }
      else {
        jk <- df %>% filter(df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    
    a <- data.frame(table(jk$country_txt))
    colnames(a) <- c("COUNTRY","FREQ")
    a <- a[order(a$FREQ, decreasing = TRUE),]
    attack_country <- a[1:10,]
    attack_country
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p1 <- plot_ly(attack_country, labels = ~COUNTRY, values = ~FREQ, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Attacks:', FREQ),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = F)
    
    p1  
  })
  
  
  
  output$piecountry2 <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$region==input$regions)
      }
      else {#All regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$success==input$success, df$region==input$regions)
      }
      else {
        jk <- df %>% filter(df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    b <- aggregate(nkill ~ country_txt ,data = jk , FUN = sum)
    colnames(b) <- c("COUNTRY","DEATH")
    b <- b[order(b$DEATH, decreasing = TRUE),]
    death_country <- b[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p3 <- plot_ly(death_country, labels = ~COUNTRY, values = ~DEATH, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Fatalities:', DEATH),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p3  
  })
  
  output$pieindia <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      jk <- india_df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
    }
    else {#If input$success are 0 or 1
      jk <- india_df %>% filter(india_df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
    }
    a <- data.frame(table(jk$provstate))
    colnames(a) <- c("STATE","FREQ")
    a <- a[order(a$FREQ, decreasing = TRUE),]
    attack_State <- a[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p1 <- plot_ly(attack_State, labels = ~STATE, values = ~FREQ, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Attacks:', FREQ),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = F)
    
    p1  
  })
  
  output$pieindia2 <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      jk <- india_df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
    }
    else {#If input$success are 0 or 1
      jk <- india_df %>% filter(india_df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
    }
    
    b <- aggregate(nkill ~ provstate, data = jk, FUN = sum)
    colnames(b) <- c("STATE","DEATH")
    b <- b[order(b$DEATH, decreasing = TRUE),]
    death_state <- b[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    p3 <- plot_ly(death_state, labels = ~STATE, values = ~DEATH, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Fatalities:', DEATH),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p3 
  })
  
  output$indiamap <- renderLeaflet({
    leaflet(Dataset2) %>% addTiles() %>%
      addMarkers(data = filteredData2(), clusterOptions = markerClusterOptions())
    
    leaflet(Dataset2)%>% addTiles() %>% 
      addMarkers(data = filteredData2(), clusterOptions = markerClusterOptions(freezeAtZoom = 6))
    leaflet(Dataset2) %>%  addTiles() %>% 
      addLabelOnlyMarkers(data = filteredData2(),
                          lng = ~Long, lat = ~Lat,
                          label = ~as.character(filteredData2()),
                          clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(multiple = T,
                                                      direction = "auto"))
  })   
  
  output$terr_gp_data_by_region <- renderTable({
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        z    <- data.frame(table(df$gname[(df$region==input$regions &
                                             df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
      else {#All regions
        z    <- data.frame(table(df$gname[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        z    <- data.frame(table(df$gname[(df$success==input$success & df$region==input$regions &
                                             df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
      else {
        z    <- data.frame(table(df$gname[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
    }
    if (nrow(z)!=0) {
      y<-z[!z$Var1=="Unknown",]
      names(y) <- c("Terrorist Groups","Number of Attacks")
      head(y[order(-y$"Number of Attacks"),],10)
    }
    else {
      paste("No data in the selected time period")
    }
  })
  ###Weapons used in the attacks worldwide
  output$weapon <- renderPlotly({
    #par(mar=c(14,3,2,2))
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        y<-data.frame(table(df$weaptype1_txt[(df$region==input$regions &
                                                df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", 'All attacks\n', sep='')
      }
      else {#All regions
        y<-data.frame(table(df$weaptype1_txt[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n", sep="")
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        y<-data.frame(table(df$weaptype1_txt[(df$success==input$success & df$region==input$regions &
                                                df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
      else {
        y<-data.frame(table(df$weaptype1_txt[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
    }
    fig1 <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("green"), cliponaxis='FALSE')
    fig1 <- fig1 %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig1
  })
  
  ### Types of Attacks : worldwide
  output$attacktype <- renderPlotly({
    # par(mar=c(15,3,4,2))
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        y<-data.frame(table(df$attacktype1_txt[(df$region==input$regions & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", 'All attacks\n', sep='')
      }
      else {#All regions
        y<-data.frame(table(df$attacktype1_txt[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n", sep="")
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        y<-data.frame(table(df$attacktype1_txt[(df$success==input$success & df$region==input$regions &
                                                  df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
      else {
        y<-data.frame(table(df$attacktype1_txt[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
    }
    fig1 <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("red"), cliponaxis='FALSE')
    fig1 <- fig1 %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig1
  })
  
  ##### INDIA VISUALISATIONS
  
  ###State wise details for India
  output$India_state_wise <- renderPlotly({
    #par(mar=c(9,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$provstate[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$provstate[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("blue"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    ggplotly(fig)
  })
  
  
  ### Types of Attacks : India
  output$India_attacktype <- renderPlotly({
    #par(mar=c(15,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$attacktype1_txt[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$attacktype1_txt[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("red"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 180), xaxis = list(tickangle = 45))
    fig
  })
  
  ###Top 10 terror groups in the India
  output$India_terr_gp <- renderTable({
    if (input$success==2) {
      z    <- data.frame(table(india_df$gname[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
    }
    else {#If input$success are 0 or 1
      z    <- data.frame(table(india_df$gname[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
    }
    if (nrow(z)!=0) {
      y<-z[!z$Var1=="Unknown",]
      names(y) <- c("Terrorist Groups","Number of Attacks")
      head(y[order(-y$"Number of Attacks"),],10)
    }
    else {
      paste("No data in the selected time period")
    }
  })
  
  ###Weapons used in the attacks in India
  output$India_weapon <- renderPlotly({
    #par(mar=c(14,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$weaptype1_txt[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$weaptype1_txt[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("green"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig
  })
  
  output$globe <- renderGlobe({
    v <- values()
    args <- c(earth_dark, list(lat=v$attack$latitude, long=v$attack$longitude, value=v$value, color=v$color,pointsize = 1,bg ="black",atmosphere=TRUE))
    do.call(globejs, args=args)
    
  })
  
  output$attacks_chi <- renderPlot({ 
    t1 <- table(df2$attacktype1_txt,df2$success)
    t1
    balloonplot(t(t1), main ="Balloon Plot", xlab ="Success", ylab="Attack Types" ,text.size=0.55,
                label = FALSE, show.margins =FALSE,dotsize=4)
  })
  output$weapon_chi <- renderPlot({ 
    t1 <- table(df2$weaptype1_txt,df2$success)
    t1
    balloonplot(t(t1), main ="Balloon Plot", xlab ="Success", ylab="Weapon Types" ,text.size=0.55,
                label = FALSE, show.margins =FALSE,dotsize=4)
  })
  
  
  
  
  
  output$tree <- renderD3tree2({fruitTree <- d3tree(treemap(df,
                                                            index = c("country_txt","iyear"), type = "value",
                                                            vSize = "nkill",vColor="nwound" ,palette = c("#FFFFFF","#FFFFFF","#CC0000")
  ), rootname = "World")
  })
  
  output$indiatree=renderD3tree2({
    
    fruitTree <- d3tree(treemap(india_df,
                                index = c("provstate","iyear"), type = "value",
                                vSize = "nkill",vColor="nwound" ,palette = c("#FFFFFF","#FFFFFF","#CC0000")
    ), rootname = "india")
  })
}


shinyApp(ui, server)