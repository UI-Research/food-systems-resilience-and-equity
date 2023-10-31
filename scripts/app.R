library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)
library(leaflet)
library(lubridate)
library(urbnthemes)
library(ggtext)
library(cowplot)

# set urbnthemes
set_urbn_defaults(style = "print")

# link to helper script
source("04_shiny-app-helper.R")

# User interface ----
ui <- fluidPage(
  useShinyjs(), 
  
  # add css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
  ),
  # specify the selection variables on the left of the app
  sidebarLayout(
    
    sidebarPanel(width = 3, 
                 # use this id tag later to hide sidebar on landing page
                 id="sidebar",
                 # have options shown be conditional on which tab user is on 
                 conditionalPanel(condition = "input.tabselected==2",
                                  # drop down menu for weighting by HH FI or Ag Production
                                  selectInput(inputId = "weight", 
                                              label = "See climate hazards by:", 
                                              choices = list("Household Food Insecurity" = "fi_sheldus", 
                                                             "Black and Hispanic Food Insecurity" = "fi_bh_sheldus",
                                                             "Agricultural Production" = "edible_sheldus", 
                                                             "Tribal Climate Risk" = "tribal_sheldus"))
                 ),
                 # have options shown be conditional on which tab user is on 
                 conditionalPanel(condition = "input.tabselected==3", 
                                  # drop down for state 
                                  selectInput(inputId = "state1", 
                                              label = "State", 
                                              choices = state_list, 
                                              selected = "Illinois"),
                                  # drop down for county - dependent on state selection
                                  selectInput(inputId = "county1",
                                              label = "County", 
                                              # We can initialize this as NULL because
                                              # we will update in the server function.
                                              choices = NULL), 
                                  radioButtons(inputId = "comparison", 
                                               label = "Pick a value to display",
                                               choices = list("Raw Values", 
                                                              "County Comparison")),
                                  tags$small("Filter between raw values and percentile ranks to show where your county
                                             falls relative to other counties in the U.S"),
                                  selectInput(inputId = "measure1", 
                                              label = "Select a measure to display", 
                                              choices = list(
                                                "County demographic details",
                                                "Food security and access", 
                                                "Agricultural production",
                                                "Climate hazards"))
                 ),
                 # have options shown be conditional on which tab user is on 
                 conditionalPanel(condition = "input.tabselected==4", 
                                  # drop down for state 
                                  selectInput(inputId = "state2", 
                                              label = "State", 
                                              choices = state_list, 
                                              selected = "Illinois"),
                                  # drop down for county - dependent on state selection
                                  selectInput(inputId = "county2",
                                              label = "County", 
                                              # We can initialize this as NULL because
                                              # we will update in the server function.
                                              choices = NULL)
                 )),
    
    # This is the section where we specify what is displayed in the main panel
    mainPanel(
      h3("Understanding the Intersection of Climate Risk,\nFood System Resilience, and Racial Equity: A County-Level Data Tool", align = "center"),
      # Output: Tabset w/ plots ----
      tabsetPanel(type = "tabs", 
                  # First tab - landing page describing the tool 
                  tabPanel("About", 
                           # this is the value that we use for the conditional panels above
                           value = 1, 
                           br(),
                           p(em("An Urban Insitute pilot data tool, updated ", format(Sys.Date(),"%m/%d/%Y"))),
                           p(em("Authors: Kassandra Martinchek, Judah Axelrod, Sonia Torres Rodríguez, Amy Rogin, Noah Johnson, and Elaine Waxman")), 
                           p("While food system resilience is crucial to ensuring that communities have the local resources needed to meet their 
                             food needs in a world that is increasingly vulnerable to shocks, few tools exist to help localities understand food 
                             system resilience to climate hazards, especially among communities of color. Food system resilience is especially important 
                             in communities of color as they are more vulnerable to climate-related risks, experience persistently high rates of food 
                             insecurity, and face starker barriers to accessing food due to centuries-long discriminatory policies and practices."),
                           br(),
                           p("This pilot data tool is designed to give changemakers key data they need to demonstrate the intersections between climate risk, 
                             food systems, and racial equity and effectively target resources to communities that face the starkest challenges in climate risk 
                             and recovery. This tool is a pilot effort to understand these intersections and our team hopes to collaborate with 
                             communities, changemakers, and policymakers to empower localities with data to better support food system resilience."),
                           br(),
                           h3("How to use this tool"),
                           br(),
                           p("This pilot tool provides key county-level data on climate hazards, food systems, and racial equity for the United States. 
                             There are several tabs that enable users to filter and select data to explore their counties’ food system resilience. "),
                           p("-", strong(" The “national map” tab"), " shows areas with elevated climate hazards and food systems risks. These maps look at the 
                             intersection of climate hazards (such as wildfires, droughts, and floods) that have strong impacts on local food systems 
                             (as measured by food security and agricultural production). This tab is useful for exploring where climate and food systems risks
                             intersect and which counties face cumulative disadvantages in food system resilience."),
                           p("-", strong(" The “county level data” tab"), " shows data on food security and access, demographics, agricultural production, and 
                             climate hazards at the county level. On the left-hand side, users can select a county to view, then toggle between raw values and
                             the county’s percentile rank relative to other counties for each data topic. Users can also see five counties that have similar
                             agricultural production, food security, and climate hazard profiles. This tab is useful for exploring county-level data on factors 
                             that shape food system resilience to climate hazards, seeing how a county compares with all other US counties, and identifying 
                             counties with similar risks to food system resilience."),
                           p("-", strong("The “race-disaggregated data” tab"), " shows county-level data on factors that shape food system resilience, disaggregated 
                             by race and ethnicity. Specifically, this tab includes data on counties’ racial/ethnic demographics and residential segregation, 
                             home values, homeownership, poverty, employment, and income by race and ethnicity. This tab is valuable for exploring differences
                             between populations within counties to illustrate the landscape of varied risks by race and ethnicity."),
                           br(),
                           p("If you have any questions or feedback as you use this data tool, please email foodandclimate@urban.org 
                             We would also love to hear how you have used this data tool as well as any other research and engagement you would find helpful to your work."),
                           p(paste0("Recommended citation: Martinchek, Kassandra, Judah Axelrod, Sonia Torres Rodríguez, Amy Rogin, Noah Johnson, and Elaine Waxman. Understanding the Intersection of Climate Risk, Food System Resilience and Racial Equity: 
                             A County-Level Data Tool (Urban Institute; last updated ", format(Sys.Date(),"%m/%d/%Y"), ")."),
                             tags$a(href = "https://urban-institute.shinyapps.io/food-systems-clustering/.", "https://urban-institute.shinyapps.io/food-systems-clustering/.")),
                           p("Download the data and read the technical documentation: ", 
                             tags$a(href = "https://datacatalog.urban.org/dataset/food-systems-resilience-and-equity", "https://datacatalog.urban.org/dataset/food-systems-resilience-and-equity")),
                           p("See the code that generated the data and analyses presented in this data tool: ", 
                             tags$a(href = "https://github.com/UI-Research/food-systems-resilience-and-equity", "https://github.com/UI-Research/food-systems-resilience-and-equity")), 
                           br(),
                           h3("Related Publications"), 
                           br(),
                           # p("- ", tags$a(href = "  https://datacatalog.urban.org/dataset/food-systems-resilience-and-equity", "Food Systems Resilience and Equity Data@Urban blog")),
                           p("- ", tags$a(href = "https://www.urban.org/growing-climate-hazards-pose-risks-food-system-resiliency-nationwide-communities-color-stand-bear", "Growing Climate Hazards Pose Risks to Food System Resiliency Nationwide. Communities of Color
Stand to Bear the Brunt.")),
                           p("- For more historical local-level data on food insecurity and discussions of its drivers, please see:",
                             tags$a(href = "https://apps.urban.org/features/disrupting-food-insecurity/", "Disrupting Food Insecurity")), 
                           br(), 
                           p(em(strong("Details about the author’s contributions: "), "Kassandra Martinchek led the overall project, including designing analyses, identifying data sources, and leading synthesis and writing of deliverables. 
                             Judah Axelrod designed and led our team’s collaboration on the programming and data analysis, led on the K-Nearest Neighbors analysis, 
                             and contributed to decision-making on incorporation of racial equity considerations. Sonia Torres Rodriguez contributed to data 
                             wrangling and analysis at all phases of the project, including the data aggregation and the percentile, quintile, and K-Nearest 
                             Neighbors analysis; led the drafting of the technical appendix; and supported the creation of the R Shiny app and publications. 
                             Amy Rogin led the development of R Shiny app used to visualize the data and analyses for the project, advised on climate data and 
                             limitations, and supported writing of publications. Noah Johnson contributed to data wrangling and analysis at all phases of the 
                             project and supported the creation of the R Shiny app and publications, including the technical appendix. Elaine Waxman led community 
                             engagement and advised on data sources and frameworks, analysis approach, and co-authored publications."))),
                  # Second tab - will display as "National Map"
                  tabPanel("National Map", 
                           # this is the value that we use for the conditional panels above
                           value = 2, 
                           br(),
                           p("Counties that face intersecting climate and food systems risks may be less equipped to respond to and 
                             rebound from climate-related shocks. Below, we present national-level data on counties’ climate and food 
                             system resilience in three different views. Click through the variables on the left to view each. You can download the data 
                             and read the technical documentation ", tags$a(href = "https://datacatalog.urban.org/dataset/food-systems-resilience-and-equity", "here.")),
                           # explainer text for each map selected
                           uiOutput("map_text"),
                           # specify the plot type - will use name within to call as tag in server
                           # leafletOutput means we're displaying a leaflet map
                           leafletOutput("nat_map"), 
                           br(), 
                           br()),
                  # Third tab - will display as "County Level"
                  tabPanel("County-Level Data", 
                           # this is the value that we use for the conditional panels above
                           value = 3, 
                           fluidRow(
                             br(),
                             # add intro text 
                             column(12, HTML(paste0(p("Local-level data are helpful for seeing where your county stands in terms of food access, agricultural production,
                               and climate risk. In this tab, you can view county-level data on these indicators by selecting a state and county 
                               on the left-hand panel. There, you can select either raw values or percentile ranks, which show where your county 
                               falls relative to other counties in the US. Details about each indicator—what it means in terms of food system 
                               resilience at the local level—are provided when users select their topic of interest (food access, agricultural 
                               production, or climate risk)."), 
                                                    p("Data may be missing due to suppression for small sample sizes or race/ethnicity groups or incomplete 
                                                      coverage for certain geographies. When this is the case, the corresponding chart value will not appear.
                                                      For more details see the technical appendix."),
                                                    p("Full county-level data across all indicators are available on ",
                                                      tags$a(href = "https://datacatalog.urban.org/dataset/food-systems-resilience-and-equity", "Urban’s Data Catalog"))))),
                             column(12, htmlOutput("header")), 
                             # specify the plot type - will use name within to call as tag in server
                             # plotOutput means we're displaying a general ggplot figure
                             column(12, uiOutput("slider", height = 300, width = "100%")), 
                             # add subheader under lollipop chart 
                             column(12, htmlOutput("subheader")),
                             # second plot or KNN table 
                             column(12, uiOutput("bar", height = 300, width = "100%")),
                             br()
                           )),
                  # Fourth tab - will display as "Racial Disparities"
                  tabPanel("Race-Disaggregated Data", 
                           # this is the value that we use for the conditional panels above
                           value = 4, 
                           fluidRow(
                             br(), 
                             p("Racial disparities in food systems reflect legacies of policies contributing to inequitable systems, such as housing, employment,
                               heath care, and criminal justice policies (Odoms-Young and Bruce 2018). In this tab, we show county-level data disaggregated by 
                               race and ethnicity on key factors that contribute to food insecurity and food systems."),
                             p("For more local-level data on localities’ opportunities for mobility, please see:",
                               tags$a(href = "https://upward-mobility.urban.org/measuring-upward-mobility-counties-and-cities-across-us.", "https://upward-mobility.urban.org/measuring-upward-mobility-counties-and-cities-across-us.")),
                             # demographics plot
                             h3("Racial composition and segregation"),
                             br(),
                             p("The demographic breakdown of a county’s population by race and ethnicity shows its racial and ethnic diversity. But counties may be racially and ethnically diverse 
                               while also having high levels of residential segregation. To capture this, we show data on whether members of a particular race or ethnicity have neighbors of other
                               races and ethnicities. High values suggest that members of that particular race or ethnicity live near those of other races and ethnicities, whereas low values may 
                               indicate residential segregation, where one group is geographically separated from other races and ethnicities. In areas with high degrees of residential segregation, 
                               residents may have markedly different levels of access to local resources and economic opportunities. For example, although not measured, in residentially segregated areas, 
                               residents of color are often farther from retail and charitable food resources where families get the food they need to live active and healthy lives."),
                             splitLayout(cellWidths = c("50%", "50%"),
                                         plotOutput("demog"),plotOutput("racial_seg")), 
                             # housing plots
                             # use split layout to display two graphs next to each other to minimize scrolling
                             br(),
                             h3("Home values and homeownership"), 
                             br(),
                             p("Homeownership rates can capture the extent to which residents have access to and are invested in asset-building. Assets can buffer food insecurity, and homeowners are
                               less likely to be food-insecure. Research has consistently documented differences in homeownership by race and ethnicity, with additional evidence that homeowners of 
                               different races and ethnicities may not equally share in housing price appreciation. To capture this, we show data on the ratio of home values to the share of the population 
                               that owns their homes. Low values suggest that homeowners of that race or ethnicity may not equally share in housing wealth relative to their homeownership rate. Lower levels
                               of homeownership and housing wealth can both indicate that residents have fewer buffers against food insecurity and with which to rebound from climate-related financial emergencies."),
                             p("For more local-level data on wealth and housing, please see:",
                               tags$a(href = "https://apps.urban.org/features/financial-health-wealth-dashboard/.", "https://apps.urban.org/features/financial-health-wealth-dashboard/.")),
                             splitLayout(cellWidths = c("50%", "50%"), 
                                         plotOutput("housing1"), plotOutput("housing2")), 
                             # income plots
                             br(),
                             h3("Poverty and income"), 
                             br(),
                             p("Income is a key resource for families to rebound from financial shocks and meet their financial and food needs. Local-level data on poverty rates can reveal counties where many residents 
                               have limited incomes and consequently may experience trouble meeting basic needs. Other counties may have areas of concentrated poverty, which could indicate that some populations 
                               experience economic exclusion and are isolated from better-resourced neighbors and local social and economic resources. Although poverty and concentrated poverty provide a window 
                               into areas of disadvantage, median incomes can reveal whether counties have areas with affluence (high median incomes) alongside concentrated disadvantage, which could signal that 
                               any recoveries from climate shocks could become inequitable between different groups."),
                             splitLayout(cellWidths = c("50%", "50%"), 
                                         plotOutput("income1"), plotOutput("income2")),
                             plotOutput("income3"),
                             # employment and med income plots
                             br(),
                             h3("Employment"), 
                             br(),
                             p("Employment is a key predictor of food security, while higher rates of unemployment are correlated with higher food insecurity rates. Localities and populations with higher rates 
                               of unemployment may struggle to get back on their feet and may have fewer resources available to respond to climate-related financial emergencies."),
                             plotOutput("employ"),
                             # food insecurity plot
                             br(),
                             h3("Food insecurity"),
                             br(),
                             p("Food insecurity is typically higher among Black, Latinx, and Native populations, which reflects differential access to resources for purchasing food and other basic needs, as well 
                               as to asset-building opportunities that reduce the ability of households of color to rebound from economic shocks (Odoms-Young and Bruce 2018). Below, we show the breakdown of food 
                               insecurity rates by race and ethnicity for families in the selected county."),
                             plotOutput("food_insec"))),
                  id = "tabselected")
    )
  )
)

# Server logic ----
server <- function(input, output, session) {
  
  # code to hide the sidebar panel on the landing page
  observeEvent(input[["tabselected"]], {
    if(input$tabselected == 1){
      hideElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-8")
      addCssClass("main", "col-sm-12")
    }else{
      showElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
    }
  })
  
  output$map_text <- renderUI({
    if(input$weight == "fi_sheldus"){
      HTML(paste(h3("Food Insecurity"), 
                 p("This map shows three measures: (1) counties in the top quintile (20 percent) 
    of food insecurity rates (in blue), (2) counties in the top quintile (20 percent) in terms of total damages 
    to property and crops over a 10-year period from floods, wildfires, and droughts (in yellow), and (3) counties
    in the top quintile (20 percent) for both climate hazards and food insecurity rates (in magenta). 
    Counties with elevated climate risks from wildfires, droughts, and floods and high rates of food insecurity may
    face outsized challenges building and restoring resilient food systems and could be prioritized by policies 
    designed to help local communities manage climate risks and meet local food needs."),
                 sep = '<br/>'))}
    
    else if(input$weight == "fi_bh_sheldus"){
      HTML(paste(
        h3("Food insecurity among Black and Latinx populations"),
        p("This map shows three measures: (1) counties
                             in the top quintile (20 percent) of food insecurity rates for Black and Latinx families combined (in blue), 
                             (2) counties in the top quintile (20 percent) in terms of total damages to property and crops over a 10-year 
                             period from floods, wildfires, and droughts (in yellow), and (3) counties in the top quintile (20 percent)
                             for both climate hazards and food insecurity rates for Black and Latinx families combined (in magenta). 
                             For this map, we use the combined food insecurity rate for Black and Latinx families, who disproportionately 
                             experience food insecurity. This map highlights unique climate risks to communities of color, where the unique 
                             intersections between climate hazards, food systems, and racial equity converge."),
        sep = '<br/>'))
    }
    else if(input$weight == "edible_sheldus"){
      HTML(paste(
        h3("Agricultural production"),
        p("This map shows three measures: (1) counties in the top quintile (20 percent)
                             of agricultural production of edible crops (in blue), (2) counties in the top quintile (20 percent) 
                             in terms of total damages to property and crops over a 10-year period from floods, wildfires, and droughts 
                             (in yellow), and (3) counties in the top quintile (20 percent) for both climate hazards and agricultural 
                             production of edible crops (in magenta). Areas with high agricultural food production and elevated climate
                             risks from wildfires, droughts, and floods may be less able to produce food to meet community needs into the
                             future, so policy protections for local producers may be supportive."),
        sep = '<br/>'))
    }
    else if (input$weight == "tribal_sheldus"){
      HTML(paste(
        h3("Tribal lands"),
        p("This map shows two measures: (1) counties that overlap with tribal lands,
                   including federally recognized American Indian reservations, off-reservation trust land areas,
                   state-recognized American Indian reservations, and Hawaiian home lands (in light blue),and (2) 
                   counties that overlap with tribal lands and are in the top quintile (20 percent) for climate hazards (in blue). Counties
                   with elevated climate risks from wildfires, droughts, and floods and contain tribal lands could be
                   prioritized by policies designed to help Native communities manage and remain resilient to climate
                   risks in a complex governance structure."), 
        sep = '<br/>'))
    }
  })
  
  # Leaflet map 
  output$nat_map <- renderLeaflet({
    
    # add percent tribal land to label -NOTE: This if else isn't currently working
    if (input$weight == "tribal_sheldus"){
      
      # filter data to input into map
      leaflet_data <- map_data %>%
        select(geoid, input$weight, county_name, state_name, tribal_percent) %>%
        left_join(county_sf, by = c("geoid" = "GEOID")) %>%
        # add text to be used in tooltip
        mutate(lab = paste0('<strong>County: </strong>',
                            county_name,
                            '<br><strong>Percent Tribal Land</strong>: ',
                            # NOTE: Change when input variable is decided
                            round(tribal_percent, 2))) %>%
        st_as_sf() %>%
        st_set_crs("4326")
      
      # create labels
      labs <- as.list(leaflet_data$lab)
      
      # leaflet map
      leaflet(leaflet_data) %>%
        # basemap layer
        addProviderTiles(providers$CartoDB.Positron)  %>%
        # choropleth layer
        addPolygons(color = "#444444",
                    weight = .2,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = .9,
                    fillColor = ~colorFactor(c("#1696d2", "#cfe8f3", "#d2d2d2"), leaflet_data[[input$weight]])(leaflet_data[[input$weight]]),
                    # adds tooltip
                    label = lapply(labs, HTML), 
                    labelOptions = labelOptions(textsize = "12px")
        ) %>% 
        addLegend("bottomright", 
                  pal = colorFactor(c("#1696d2", "#cfe8f3", "#d2d2d2"), leaflet_data[[input$weight]]), 
                  values = ~leaflet_data[[input$weight]], 
                  title = NULL)  %>% 
        setView(lng = -95,  lat = 40, zoom = 3)
    }
    
    else if(input$weight != "tribal_sheldus"){
      # filter data to input into map
      leaflet_data <- map_data %>%
        select(geoid, input$weight, county_name, state_name) %>%
        left_join(county_sf, by = c("geoid" = "GEOID")) %>%
        # add text to be used in tooltip
        mutate(lab = paste0('<strong>County: </strong>',
                            county_name,
                            '<br><strong>Percentile</strong>: ',
                            # NOTE: Change when input variable is decided
                            !!sym(input$weight))) %>%
        st_as_sf() %>%
        st_set_crs("4326")
      
      # create labels
      labs <- as.list(leaflet_data$lab)
      
      # leaflet map
      leaflet(leaflet_data) %>%
        # basemap layer
        addProviderTiles(providers$CartoDB.Positron)  %>%
        # choropleth layer
        addPolygons(color = "#444444",
                    weight = .2,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = .9,
                    fillColor = ~colorFactor(c("#1696d2", "#fdbf11", "#ec008b", "#d2d2d2"), leaflet_data[[input$weight]])(leaflet_data[[input$weight]]),
                    # adds tooltip
                    label = lapply(labs, HTML), 
                    labelOptions = labelOptions(textsize = "12px")
        ) %>% 
        addLegend("bottomright", 
                  pal = colorFactor(c("#1696d2", "#fdbf11", "#ec008b", "#d2d2d2"), leaflet_data[[input$weight]]), 
                  values = ~leaflet_data[[input$weight]], 
                  title = NULL) %>% 
        setView(lng = -95,  lat = 40, zoom = 3)
    }
    
    
    
  })
  
  # Whatever is inside `observeEvent()` will be triggered each time the first
  # argument undergoes a change in value. In this case, that means whenever
  # the user changes the state input.
  observeEvent(input$state1,
               { # Use this function to update the choices for the user.
                 # First argument is session, next the input to update,
                 # and third the new choices. Here, I'm filtering the
                 # previously made data.frame to based on the series column,
                 # and returning the choices column. 
                 # `drop=TRUE` makes it explicit that I want a vector returned.
                 updateSelectInput(session, "county1",
                                   choices = county_list %>% filter(state_name == input$state1) %>% 
                                     pull(county_name))
               })
  # get county list for second tab also 
  observeEvent(input$state2,
               { # Use this function to update the choices for the user.
                 # First argument is session, next the input to update,
                 # and third the new choices. Here, I'm filtering the
                 # previously made data.frame to based on the series column,
                 # and returning the choices column. 
                 # `drop=TRUE` makes it explicit that I want a vector returned.
                 updateSelectInput(session, "county2",
                                   choices = county_list %>% filter(state_name == input$state2) %>% 
                                     pull(county_name))
               })
  
  # HELPER FUNCTION ---------------------------------------------------------
  
  # This function is to create lollipop charts based on the columns given
  # and rename the y axis to be legible 
  # I'm making this it's own helper function so I can rename the values
  # once instead of for each variation of the plot
  lollipop_chart <- function(...){
    
    col_names <- c(...)
    
    lollipop <- raw_perc %>% 
      select(col_names, county_name, state_name) %>% 
      filter(state_name == input$state1, county_name == input$county1) %>% 
      pivot_longer(cols = -c(county_name, state_name), names_to = "Measure", values_to = "Percent") %>%
      mutate(Measure = recode(Measure,
                              ratio_average_to_living_wage = "Ratio of average wage to living wage",
                              ratio_average_to_living_wage_percentile = "Ratio of average wage to living wage",
                              median_hh_income = "Median household income",
                              median_hh_income_percentile = "Median household income",
                              per_disability = "Percent of population with a disability",
                              per_disability_percentile = "Percent of population with a disability",
                              per_h_owner = "Share of residents who own their home",
                              per_h_owner_percentile = "Share of residents who own their home",
                              unemp_rate = "Unemployment rate",
                              unemp_rate_percentile = "Unemployment rate",
                              fi_rate_overall = "Household food insecurity rate",
                              fi_rate_overall_percentile = "Household food insecurity rate",
                              cost_per_meal = "Average cost per meal",
                              cost_per_meal_percentile = "Average cost per meal",
                              share_hh_receive_snap = "Share of households receiving SNAP",
                              share_hh_receive_snap_percentile = "Share of households receiving SNAP",
                              share_production_edible_crops = "Share of production that is edible crops",
                              share_production_edible_crops_percentile = "Share of production that is edible crops",
                              share_production_edible_animals = "Share of production that is edible animal products",
                              share_production_edible_animals_percentile = "Share of production that is edible animal products",
                              share_commodity_direct = "Share of food sold direct to consumer",
                              share_commodity_direct_percentile = "Share of food sold direct to consumer",
                              share_ex_heat_days = "Share of extreme heat days",
                              ex_heat_days_percentile = "Share of extreme heat days",
                              share_ex_precip_days = "Share of extreme precipitation days",
                              ex_precip_days_percentile = "Share of extreme precipitation days",
                              tribal_percent = "% of county that contains tribal lands",
                              tribal_percent_percentile = "% of county that contains tribal lands",
                              per_non_hisp_black = "% of population identifying as Black",
                              per_non_hisp_black_percentile = "% of population identifying as Black",
                              per_non_hisp_white = "% of population identifying as Non-Hispanic White",
                              per_non_hisp_white_percentile = "% of population identifying as Non-Hispanic White",
                              per_hisp = "% of population identifying as Hispanic",
                              per_hisp_percentile = "% of population identifying as Hispanic",
                              per_non_hisp_native = "% of population identifying as Native",
                              per_non_hisp_native_percentile = "% of population identifying as Native",
                              per_non_hisp_asian = "% of population identifying as Asian",
                              per_non_hisp_asian_percentile = "% of population identifying as Asian",
                              pct_laccess_hhnv15 = "Share of households with low access to a\ngrocery store and no vehicle",
                              pct_laccess_hhnv15_percentile = "Share of households with low access to a\ngrocery store and no vehicle", 
                              sheldus_average_p = "Historical disaster damage")) %>%
      ggplot(aes(x = Percent, y = factor(Measure)))+
      geom_segment(aes(x = 0, xend = Percent, y = Measure, yend = Measure), linewidth = 1) +
      scale_y_discrete(labels =  function(x) str_wrap(x, width = 10))+
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))+
      guides(size = "none", linewidth = "none")+
      geom_point(size = 11.5, color = "black")+
      # add value to plot
      geom_text(aes(label = paste0(round(Percent*100), "%")), size = 4, color = "white")+
      labs(x = NULL,
           y = NULL)+
      scale_x_continuous(label = scales::percent, limits = c(0,1))
    
    lollipop
    
  }
  
  # intro text depending on which measure is selected on the county tab
  output$header <- renderUI({
    if (input$measure1 == "Food security and access"){
      # get the average cost per meal for the selected county
      meal_cost <- raw_perc %>% 
        filter(state_name == input$state1, county_name == input$county1) %>% 
        pull(cost_per_meal)
      
      HTML(paste(h3("Food security and access"),
                 p("Food insecurity measures the share of county residents who do not have consistent access 
                     to the food they need to live active and healthy lives. Counties with higher rates of food
                     insecurity have many residents who struggle to meet their day-to-day food needs and this can 
                     suggest that such areas have more residents who could face additional hardship if local food
                     systems are affected by climate hazards.") ,
                 p(paste0("In addition to food security, it is important to consider residents’ access to food resources 
                     and the costs they pay for food. Both of these factors are deeply intertwined with food insecurity 
                     and shape families’ ability to secure the food they need.  In areas where meal costs are high, 
                     purchasing power from family income and federal nutrition benefits may be eroded. In addition, households 
                     living in areas with low access to retail food stores may incur higher time and transportation costs to 
                     purchase food, which could limit the amount, variety, and healthfulness of the food families can secure 
                     on a consistent basis. In ", input$county1, ", the average cost per meal is ", scales::dollar(meal_cost))),
                 sep = '<br/>'))
    }
    else if (input$measure1 == "Agricultural production"){
      HTML(paste(h3("Agricultural production"),
                 p("Agricultural production is an important asset for communities because it can increase options for meeting
                     residents’ food needs and reduce costs associated with transporting food. Local agricultural production may
                     be focused on crop production for export, animal feed, or uses other than food for household consumption. 
                     For the purposes of examining the ability of local communities to generate their own food resources, we measure
                     the share of agricultural production devoted to edible crops and animal products. Then, we include an indicator
                     for whether agricultural production is sold locally (direct to consumers) to capture the degree to which agricultural 
                     production remains local or is distributed more widely."),
                 sep = '<br/>'))
    }
    else if (input$measure1 == "Climate hazards"){
      HTML(paste(h3("Climate hazards"),
                 p("Climate hazards can undermine local areas’ ability to produce edible food, maintain consistent access to retail food outlets,
                     and support family stability, especially if such hazards result in significant property losses, displacement, financial distress,
                     or lower quality of life. Data on how often counties experience extreme heat and precipitation days provides a measure for climate
                     risks at the local level and how vulnerable localities may be to future climate shocks."), 
                 p("Further, we can examine counties’ relative risks in terms of their past experiences with climate hazards that impact food systems. 
                   Specifically, we measure the magnitude of property and crop damages each county has experienced from wildfires, droughts, and floods 
                   over the past 10 years, relative to other counties. We do not provide raw values on this metric at the county level due to data disclosure 
                   constraints. These metrics can help us understand which areas have faced significant or consistent climate hazards and as such could experience 
                   challenges recovering from them, both now and in the future.")))
    }
    else if (input$measure1 == "County demographic details"){
      HTML(paste(h3("County demographic details"), 
                 p("Localities differ in their access to resources with which to respond to and rebound from climate hazards and remain food secure.
                     Local-level differences in factors that contribute to food insecurity, such as unemployment, cost of living, disability status,
                     and homeownership, may provide valuable context for local-level vulnerabilities to a lack of food system resilience.")))
    }
  })
  
  # text under slider plot and before KNN tables
  output$subheader <- renderUI({
    if(input$measure1 == "Food security and access" & input$comparison == "County Comparison"){
      HTML(paste(
        br(),
        p("In addition to seeing a county’s ranking on food security and access relative to other counties, 
                     it can be informative to see which counties in the US share similar food insecurity risk profiles. 
                     Below, we show five counties that are similar to this county in terms of their overall food insecurity 
                     rates and their combined food insecurity rates for Black and Latinx families. These counties may share
                     similar challenges in reaching families facing food insecurity and ensuring that programs and policies 
                     equitably reach families in need. Counties that do not have estimated food insecurity rates for Black 
                     and Latinx households because of small population numbers are not included in this analysis.")))
    }
    else if(input$measure1 == "Agricultural production"){
      # text before agricultural producer stacked bar chart
      if(input$comparison == "Raw Values"){
        HTML(paste(
          br(),
          p("Participation in local food production by producers from diverse communities is one indication of equitable
                       opportunities in the food economy. Below, we show the share of producers at the county level   by race and ethnicity
                       to examine racial and ethnic diversity in local food production.")))
      }
      # text before KNN table 
      else{
        HTML(paste(
          br(),
          p("In addition to seeing a county’s ranking on agricultural production relative to other counties, it can be informative 
                       to see which counties in the US share similar agricultural production profiles. Below, we show five counties that are 
                       similar to this county in terms of their levels of agricultural production and diversity of agricultural production. 
                       These counties may share similar strengths and challenges in producing foods that can be consumed by local residents in 
                       the face of shocks.")))
      }
    }
    else if(input$measure1 == "Climate hazards" & input$comparison == "County Comparison"){
      HTML(paste(
        br(),
        p("In addition to seeing a county’s ranking on climate hazards relative to other counties, it can be informative to see which counties 
                     in the US share similar climate hazard risk profiles. Below, we show five counties that are similar to this county in terms of total 
                     property and crop damages owing to wildfires, floods, and droughts over the past 10 years. These counties may share similar challenges
                     in responding to climate hazards and rebuilding and remaining resilient to future climate challenges. Counties that have not had any 
                     damages owing to wildfires, floods, or droughts over the past 10 years are not included in this analysis.")))
    }
  })
  
  # create lollipop charts for raw values
  output$slider_raw <- renderPlot({
    
    # based on selected measure create lollipop plot
    if(input$measure1 == "Agricultural production"){
      # % of production that is edible crops
      # % of production that is edible animals/animal products
      # % of commodities (food) sold direct to consumers
      lollipop_chart("share_production_edible_crops","share_production_edible_animals", "share_commodity_direct")
    }
    else if(input$measure1 == "County demographic details"){
      # NEED TO FIGURE OUT WHAT TO DO WITH MED HH INCOME - median_hh_income
      lollipop_chart("ratio_average_to_living_wage", "per_disability", "per_h_owner","unemp_rate")
    }
    else if(input$measure1 == "Climate hazards"){
      lollipop_chart("share_ex_precip_days", "share_ex_heat_days")
    }
    else if(input$measure1 == "Food security and access"){
      
      lollipop_chart("fi_rate_overall", "share_hh_receive_snap", "pct_laccess_hhnv15")
      
    }
  })
  
  # create lollipop charts for percentile values
  output$slider_perc <- renderPlot({
    # based on selected measure create lollipop plot
    if(input$measure1 == "Agricultural production"){
      # % of production that is edible crops
      # % of production that is edible animals/animal products
      # % of commodities (food) sold direct to consumers
      lollipop_chart("share_production_edible_crops_percentile","share_production_edible_animals_percentile", "share_commodity_direct_percentile")
    }
    else if(input$measure1 == "County demographic details"){
      # NEED TO FIGURE OUT WHAT TO DO WITH MED HH INCOME - median_hh_income
      lollipop_chart("ratio_average_to_living_wage_percentile", "per_disability_percentile", "per_h_owner_percentile","unemp_rate_percentile")
    }
    else if(input$measure1 == "Climate hazards"){
      lollipop_chart("ex_precip_days_percentile", "ex_heat_days_percentile", "sheldus_average_p")
    }
    else if(input$measure1 == "Food security and access"){
      lollipop_chart("fi_rate_overall_percentile", "share_hh_receive_snap_percentile", "pct_laccess_hhnv15_percentile", "cost_per_meal_percentile")
    }
    
  })
  
  # switch what is displayed to the main panel depending on if 
  # we're showing raw or percentile values
  output$slider <- renderUI({
    switch(input$comparison, 
           "Raw Values" = plotOutput("slider_raw"),
           "County Comparison" = plotOutput("slider_perc"),
    )
  })
  
  # create racial disparity bar charts
  output$bar_raw <- renderPlot({
    
    if(input$measure1 == "Agricultural production"){
      
      # create factor levels, clean legend names, and calculate position for labels
      temp <- raw_perc %>% 
        select(share_producers_white, share_producers_hispanic, share_producers_black, share_producers_other_race, share_producers_aapi, county_name, state_name) %>% 
        filter(state_name == input$state1, county_name == input$county1) %>% 
        pivot_longer(cols = -c(county_name, state_name), names_to = "Measure", values_to = "Percent") %>% 
        mutate(Measure = recode(Measure, 
                                share_producers_white = "Producers who are White", 
                                share_producers_black = "Producers who are Black",
                                share_producers_hispanic = "Producers who are Hispanic",
                                share_producers_other_race = "Producers who are another race" , 
                                share_producers_aapi = "Producers who are AAPI") %>%  factor(), 
               Percent = Percent*100, 
               year = "2010") %>% 
        mutate(pos=cumsum(Percent)-0.5*Percent) 
      
      # plot stacked bar chart
      ggplot(temp, aes(x = year, y = Percent, color = Measure, fill = Measure))+
        geom_bar(stat = "identity") +
        geom_text(data=subset(temp, Percent>.5),
                  aes(y = pos,
                      label = paste0(round(Percent), "%"), 
                      color = Measure),
                  size = 5, 
                  check_overlap = TRUE, 
                  nudge_x = .43) +
        coord_flip() +
        theme(axis.line = element_blank(), 
              axis.ticks = element_blank(), 
              axis.title = element_blank(), 
              panel.grid = element_blank(), 
              axis.text = element_blank(), 
              legend.text = element_text(size =15))+
        guides(fill = guide_legend(ncol = 2), 
               size = 4)+
        scale_fill_manual(values = c("#ec008b","#d2d2d2", "#fdbf11", "#55b748", "#1696d2" ))+
        scale_color_manual(values = c("#ec008b","#d2d2d2", "#fdbf11", "#55b748", "#1696d2"))    
    }
    
  })
  
  # create KNN table
  output$bar_knn <- renderDataTable({
    if(input$measure1 == "Climate hazards"){
      knn %>% 
        filter(risk_type == "Climate", county_name_og== input$county1, state_name_og == input$state1) %>% 
        select(county_name_neighbor, state_name_neighbor) %>% 
        DT::datatable(options = list(dom = 't'), colnames = c("County" = "county_name_neighbor", "State" = "state_name_neighbor"))
    }
    else if(input$measure1 == "Agricultural production"){
      knn %>% 
        filter(risk_type == "Food Production", county_name_og== input$county1, state_name_og == input$state1) %>% 
        select(county_name_neighbor, state_name_neighbor) %>% 
        DT::datatable(options = list(dom = 't'), colnames = c("County" = "county_name_neighbor", "State" = "state_name_neighbor"))
    }
    else if(input$measure1 == "Food security and access"){
      knn %>% 
        filter(risk_type == "Food Access", county_name_og== input$county1, state_name_og == input$state1) %>% 
        select(county_name_neighbor, state_name_neighbor) %>% 
        DT::datatable(options = list(dom = 't'), colnames = c("County" = "county_name_neighbor", "State" = "state_name_neighbor"))
    }
  })
  
  # switch what is displayed to the main panel depending on if 
  # we're showing raw or percentile values
  output$bar <- renderUI({
    switch(input$comparison, 
           "Raw Values" = plotOutput("bar_raw"),
           "County Comparison" = dataTableOutput("bar_knn"),
    )
  })
  
  
  race_data <- function(...){
    
    col_names <- c(...)
    
    race_plot <- raw_perc %>% 
      select(col_names, state_name, county_name) %>% 
      filter(state_name == input$state2, county_name ==  input$county2) %>% 
      pivot_longer(cols = -c(county_name, state_name), names_to = "Measure", values_to = "Percent") %>% 
      filter(!is.na(Percent) & Percent != 0)
  }
  
  # function to make boiler bar charts for each of the racial 
  # disparities plots
  racial_disp <- function(...){
    
    col_names <- c(...)
    
    race_plot <- raw_perc %>% 
      select(col_names, state_name, county_name) %>% 
      filter(state_name == input$state2, county_name ==  input$county2) %>% 
      pivot_longer(cols = -c(county_name, state_name), names_to = "Measure", values_to = "Percent") %>% 
      filter(!is.na(Percent) & Percent != 0)
    
   
    race_plot %>% 
      mutate(Measure = recode(Measure,
                              share_employed_black_non_hispanic ="Black",
                              share_employed_hispanic = "Hispanic",
                              share_employed_white_non_hispanic =  "White",
                              share_employed_other_races_and_ethnicities =  "Another race",
                              share_employed_all = "Overall",
                              ratio_black_nh_house_value_households = "Black",
                              ratio_hispanic_house_value_households = "Hispanic",
                              ratio_white_nh_house_value_households = "White",
                              ratio_other_nh_house_value_households = "Another race",
                              per_h_owner = "Overall",
                              per_h_owner_black = "Black",
                              per_h_owner_hisp = "Hispanic",
                              per_h_owner_non_hisp_white = "White",
                              per_h_owner_native = "Native",
                              per_h_owner_asian = "Asian",
                              per_h_owner_pi = "Pacific Islander",
                              share_white_nh_exposure = "White",
                              share_black_nh_exposure = "Black",
                              share_hispanic_exposure = "Hispanic",
                              share_other_nh_exposure = "Another race",
                              per_non_hisp_black = "Black",
                              per_non_hisp_white = "White",
                              per_hisp = "Hispanic",
                              per_non_hisp_native = "Native",
                              per_non_hisp_pi = "Pacific Islander",
                              per_non_hisp_asian = "Asian",
                              share_poverty_exposure_all = "Overall",
                              share_poverty_exposure_black = "Black",
                              share_poverty_exposure_hispanic = "Hispanic",
                              share_poverty_exposure_white_non_hispanic = "White",
                              share_poverty_exposure_other_races_and_ethnicities =  "Another race",
                              per_below_fpl_overall = "Overall",
                              per_below_fpl_black = "Black",
                              per_below_fpl_hisp = "Hispanic",
                              per_below_fpl_white_non_hisp = "White",
                              per_below_fpl_native = "Native",
                              per_below_fpl_asian = "Asian",
                              per_below_fpl_pi = "Pacific Islander",
                              median_hh_income = "Overall", 
                              median_hh_income_black = "Black", 
                              median_hh_income_hisp = "Hispanic", 
                              median_hh_income_non_hisp_white =  "White", 
                              median_hh_income_native = "Native", 
                              median_hh_income_asian = "Asian", 
                              median_hh_income_pi = "Pacific Islander",
                              fi_rate_overall = "Overall", 
                              fi_rate_black_any_eth = "Black",
                              fi_rate_hisp_any_race = "Hispanic", 
                              fi_rate_white_non_hisp = "White")) %>% 
      ggplot()+
      geom_col(aes(x = Measure, y = Percent, color = Measure, fill = Measure))+
      scale_x_discrete(labels =  function(x) str_wrap(x, width = 10))+
      scale_fill_manual(values = c("Hispanic"="#ec008b","White" ="#a2d4ec","Black"= "#fdbf11","Asian" = "#0a4c6a","Pacific Islander" = "#1696d2" ,"Native" = "#55b748","Another race" = "#d2d2d2", "Overall" = "#000000"))+
      scale_color_manual(values = c("Hispanic"="#ec008b","White" ="#a2d4ec","Black"= "#fdbf11","Asian" = "#0a4c6a","Pacific Islander" = "#1696d2" ,"Native" = "#55b748","Another race" = "#d2d2d2", "Overall" = "#000000"))+
      guides(color = "none", fill = "none")+
      theme(axis.text = element_text(size = 9))+
      labs(x = NULL, 
           y = NULL)
  }
  
  # racial disparities tab plots 
  output$demog <- renderPlot({
    
    validate(need(nrow(race_data("per_non_hisp_black", "per_non_hisp_white", "per_hisp", "per_non_hisp_native", 
                            "per_non_hisp_asian", "per_non_hisp_pi")) != 0, "No Data"))
    
    # call function to create bar plot and 
    racial_disp("per_non_hisp_black", "per_non_hisp_white", "per_hisp", "per_non_hisp_native", 
                "per_non_hisp_asian", "per_non_hisp_pi")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Racial Demographics")
  })
  
  # racial segregation bar plot
  output$racial_seg <- renderPlot({
    
    # add error message if no data
    validate(need(nrow(race_data("share_white_nh_exposure", "share_black_nh_exposure",
                                 "share_hispanic_exposure", "share_other_nh_exposure"))
                  != 0, "No Data"))
    
    # call function to create bar plot and 
    racial_disp("share_white_nh_exposure", "share_black_nh_exposure",
                "share_hispanic_exposure", "share_other_nh_exposure")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Percent of neighbors who are a different race or ethnicity")
  })
  
  # ratio of home value bar plot
  output$housing1 <- renderPlot({
    
    # add error message if no data
    validate(need(nrow(race_data("ratio_black_nh_house_value_households", "ratio_hispanic_house_value_households",
                                 "ratio_white_nh_house_value_households", "ratio_other_nh_house_value_households"))
                  != 0, "No Data"))
    
    # call function to create bar plot and 
    racial_disp("ratio_black_nh_house_value_households", "ratio_hispanic_house_value_households",
                "ratio_white_nh_house_value_households", "ratio_other_nh_house_value_households")+
      labs(title = "Ratio of home value by race")+
      scale_y_continuous(limits = c(0,8), breaks = seq(0,8,1))
  })
  
  # home ownership rate bar plot
  output$housing2 <- renderPlot({
    
    # add error message if no data
    validate(need(nrow(race_data("per_h_owner","per_h_owner_black", "per_h_owner_hisp", "per_h_owner_non_hisp_white",
                                 "per_h_owner_native", "per_h_owner_asian", "per_h_owner_pi"))
                  != 0, "No Data"))
    
    # call function to create bar plot and 
    racial_disp("per_h_owner","per_h_owner_black", "per_h_owner_hisp", "per_h_owner_non_hisp_white",
                "per_h_owner_native", "per_h_owner_asian", "per_h_owner_pi")+
      labs(title = "Homeownership rate by race")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))
  })
  
  # employment rate
  output$employ <- renderPlot({
    
    # add error message if no data
    validate(need(nrow(race_data("share_employed_all","share_employed_black_non_hispanic","share_employed_hispanic", 
                                 "share_employed_white_non_hispanic", "share_employed_other_races_and_ethnicities"))
                  != 0, "No Data"))
    
    # plot data
    racial_disp("share_employed_all","share_employed_black_non_hispanic","share_employed_hispanic", 
                "share_employed_white_non_hispanic", "share_employed_other_races_and_ethnicities")+
      labs(title = "Share employed by race")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))
  })
  
  # Share of people experiencing poverty who live in high-poverty neighborhoods
  output$income1 <- renderPlot({
    # add error message if no data
    validate(need(nrow(race_data("share_poverty_exposure_all","share_poverty_exposure_black", "share_poverty_exposure_hispanic", 
                                 "share_poverty_exposure_white_non_hispanic", 
                                 "share_poverty_exposure_other_races_and_ethnicities")) != 0, "Data on poverty concentration only available for counties with high-poverty neighborhoods"))
    
    # plot data
    racial_disp("share_poverty_exposure_all","share_poverty_exposure_black", "share_poverty_exposure_hispanic", 
                "share_poverty_exposure_white_non_hispanic", 
                "share_poverty_exposure_other_races_and_ethnicities")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Share of people experiencing poverty\nwho live in high-poverty neighborhoods by race")
  })
  
  # Percent of population below the federal poverty line
  output$income2 <- renderPlot({
    # add error message if no data
    validate(need(nrow(race_data("per_below_fpl_overall","per_below_fpl_black","per_below_fpl_hisp", 
                                 "per_below_fpl_white_non_hisp", "per_below_fpl_native",
                                 "per_below_fpl_asian", "per_below_fpl_pi")) != 0, "No Data"))
    # plot data
    racial_disp("per_below_fpl_overall","per_below_fpl_black","per_below_fpl_hisp", 
                "per_below_fpl_white_non_hisp", "per_below_fpl_native",
                "per_below_fpl_asian", "per_below_fpl_pi")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Percent of population below the\nfederal poverty line")
  })
  
  output$income3 <- renderPlot({
    
    # add error message if no data
    validate(need(nrow(race_data("median_hh_income", "median_hh_income_black", "median_hh_income_hisp", 
                                 "median_hh_income_non_hisp_white","median_hh_income_native", 
                                 "median_hh_income_asian", "median_hh_income_pi")) != 0, "No Data"))
    
    # plot data
    racial_disp("median_hh_income", "median_hh_income_black", "median_hh_income_hisp", 
                "median_hh_income_non_hisp_white","median_hh_income_native", 
                "median_hh_income_asian", "median_hh_income_pi")+
      scale_y_continuous(labels = scales::dollar)+
      labs(title = "Median household income by race")
  })
  
  # Food insecurity rate
  output$food_insec <- renderPlot({
    
    # add error message if no data
    validate(need(nrow(race_data("fi_rate_overall","fi_rate_black_any_eth", "fi_rate_hisp_any_race", "fi_rate_white_non_hisp"))
                  != 0, "No Data"))
    # plot data
    racial_disp("fi_rate_overall","fi_rate_black_any_eth", "fi_rate_hisp_any_race", "fi_rate_white_non_hisp")+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = "Food insecurity rate by race")
  })
}


# Run app ----
shinyApp(ui, server)
