# Load required libraries 
library(shiny)
library(shinydashboard)
library(tmap)
library(leaflet)
library(dplyr)
library(stargazer)
library(sf)
library(ggplot2)

# Load required data sets
relevant.groups <- read.csv("./relevantgroups.csv")
clean_data <- read.csv("./ethnic_clean.csv")
QoG <- read.csv("./data.clean.csv")
country_efrac <- read.csv("./efrac_qog.csv")
data(World)

# Rename name to country in World data set 
World <- rename(World, country = name)

# Merge country_efrac and World data set to create ethnic map
world_frac <- merge(country_efrac, World, by = "country") 

# Convert to sf 
world_fracc <- st_as_sf(world_frac)

# Count number of groups in each status for conflict and ethnic Map
new_data <- relevant.groups %>%
  group_by(statename, status) %>%  
  summarise(count = n())

# Rename statename to name
new_data <- rename(new_data, name = statename)

# Rename USA to US 
new_data$name <- replace(new_data$name, new_data$name == "United States of America", "United States")

# Merge World map data set and QoG
Map.data <- dplyr::left_join(World, QoG, by = c("country" = "cname"))
Map.data <- Map.data %>% 
  filter(country != "Antarctica")

# Group by country and status again to combine all countries
map_summarized <- new_data %>% 
  group_by(name, status) %>% 
  summarize(count_summary = sum(count))

# Combine status and count values 
map_summarized$count_status <- paste(map_summarized$status, ": ", map_summarized$count_summary, "<br/>")

# Group the data by country again and summarize the count/status values
map_summarized <- map_summarized %>%
  group_by(name) %>%
  summarize(count_status_sum = paste(count_status, collapse = ""))

# Merge Map.data and map_summarized for conflict and ethnic map 
map_all <- dplyr::left_join(Map.data, map_summarized, by = c("country" = "name"))

# Rename values to Democracy/Non-Democracy for scatter plot
QoG$br_dem <- replace(QoG$br_dem, QoG$br_dem == 1, "Democracy")
QoG$br_dem <- replace(QoG$br_dem, QoG$br_dem == 0, "Non-Democracy")

# Regression Model
mod1 <- lm(bti_ci + ti_cpi ~ efrac, data = country_efrac)
reg_table <- stargazer(mod1, type = "HTML")

# Use image of regression table to add to the app
reg_img <- img(src = "ethnic.png", height = 80, width = 80)

# Define UI for application
ui <- tagList(
  dashboardPage(
  dashboardHeader(title = "Ethnic Fractionalization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Maps", tabName = "maps", icon = icon("gear")),
      menuItem("Analysis", tabName = "analysis", icon = icon("gears")),
      menuItem("Regression", tabName = "regression", icon = icon("th")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("check"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h2("Ethnic Diversity and Conflict: Are ethnically diverse countries more prone to conflict?"),
              fluidRow(
                box(title = "Introduction",
                p("As the world continues to globalize, societies are becoming increasingly ethnically diverse. 
                A globalized world makes diversity inevitable, providing countries with various benefits including more innovative and creative societies. 
                Ethnic diversity is a social construct, meaning that the lines to which we decide what makes one in or out of an ethnic group can be quite blurry. 
                Yet, societies still divide, and groups form along these ethnic lines leading to both societal advantages but also at times the emergence of conflict. 
                Because of this, the benefits of ethnic diversity can often be overshadowed by the tensions it is shown to bring to a society.", br()), 
                p(""),
                p("Looking deeper into this relationship researchers have often suggested that the existence of ethnic diversity in a country makes it more likely to experience conflict. 
                Should this be the case, there are drastic implications this could have on peace and conflict resolution throughout societies. 
                Not only could this help to inform and justify policies that could be anti-immigration and suppress diversity, but it could also justify the support for more far-right policies. 
                Beyond local policy implications, the consequences could be far-reaching across the globe having a significant impact on peace and conflict resolution helping to inform international policies when approaching conflict and engaging in international relations.", br()),  
                p(""),
                p("This study will be examining whether ethnic fractionalization within countries is associated with higher levels of conflict. The variables that will be examined are:", br()),
                p(""),
                p("Independent: Number of ethnic groups"), 
                p(""),
                p("Dependent: Level of conflict"), 
                p(""),
                p("Control: Population size", br()),  
                p(""),
                p("The data sets that will be used are as follows: Gothenberg’s Quality of Government, Ethnic Power Relations, State Fragility Index, and Politically Relevant Ethnic Groups."),
                width = 9)
              ),
      ),
      
      # Second tab content
      tabItem(tabName = "maps",
              h2("Divided by Diversity: Mapping Ethnic Difference and Conflict"),
              fluidRow(
                box(
                p("The map shows the dispersion of conflict across the globe. 
                Countries shaded darker have higher levels of conflict, and countries shaded lighter have lower levels of conflict."),
                p(""),    
                p("Typically, when looking at conflict, especially ethnic conflict, the level of conflict a country experiences is measured by looking at the outbreak of violent conflict. 
                While a commonly used measure, conflict is more multifaceted than just a violent outbreak and can still cause problems in a society regardless of whether arms have been taken up. 
                The existence of tension in society can still have far-reaching consequences including political unrest, economic instability, and low social capital. 
                These consequences make it important to look at all levels of conflict rather than just violent outbreaks."),
                p(""),
                p("On this map, a conflict rating of one is social cohesion and absence of conflict and a 10 is the complete onset of civil war.  
                As can be seen in this map, conflict is most prominent in areas that are known to be quite ethnically diverse. 
                Most notably, throughout the continent of Africa conflict is seen to be quite high where arbitrary borders were drawn during colonialism combining and separating various ethnic groups. 
                A phenomenon that likely helps to explain why conflict is higher in those areas."), 
                p("Conversely, countries such as Japan that are almost entirely ethnically homogenous are shown to experience the lowest levels of conflict."),  
                p(""),
                p("Images such as this have led to the notion that ethnic diversity in a society is a common driver of conflict- the more diverse the society- the more division and conflict will exist. To create a definition or measure of ethnic diversity, the ethnic fractionalization index was created. Ethnic fractionalization is a measurement of ethnic differences in an area. The index works by measuring the likelihood any two people in the area would be of the same ethnicity. The measurement of ethnic fractionalization is on a 0-1 scale, with one being high ethnic fractionalization and zero being low. So, for countries such as Japan, there would be low levels of ethnic fractionalization as the society is pretty much ethnically homogenous. Whereas Canada would measure highly, as it is incredibly ethnically diverse. 
                The map below paints a picture of how ethnic fractionalization is dispersed throughout the world."),
                selectInput(inputId = "variable",
                            label = "Variable:",
                            choices = c("Ethnic Fractionalization", "Conflict"),
                            selected = "Ethnic Fractionalization"),
                tmapOutput(outputId = "ethnicmap"),
                p("One can generally conclude by looking at these two maps that certainly in countries that we understand to have high levels of ethnic fractionalization, there are also high levels of conflict more often than not."), 
                p("But how can this be true when countries such as Canada are praised for their diversity, and experience such low levels of conflict? To explore this relationship a little bit deeper, the below map offers a variety of features to help investigate and visualize the relationship."),
                p(""),
                p("The map below shows the level of conflict each country experiences with the ability to adjust the map to look at countries with certain ethnic fractionalization rates, allowing for the evaluation of what levels of ethnic fractionalization relate to real-world levels of conflict."),
                p(""),
                p("While ethnic fractionalization is the common measure of ethnic diversity in a society, it has often been argued that there are other ways to determine how ethnic diversity impacts conflict. What is commonly suggested is not the number of ethnic groups, but their size that matters. The Politically Relevant Ethnic Groups data set is comprised of data from over 155 countries and includes the size of each ethnic group as well as their access to power. On this map below, a viewer can hover over a country of interest and see the number of groups in each country and see how many groups are: a monopoly, fighting for power, discriminated against, irrelevant, dominant, senior partner, junior partner, regionally autonomous and separatist autonomy."),
                sliderInput("efrac_slider",
                            "Select level of Ethnic Fractionalization",
                            min = 0,
                            max = 1,
                            value = c(0, 1),
                            step = 0.01),
                leafletOutput("conflict_map"),
                p(""),
                p("The idea to look beyond the ethnic fractionalization rate of a country to also consider its ethnic composition was inspired by a popular study in political science, “The Political Salience of Cultural Difference” by Daniel Posner."),
                p(""),
                p("Looking at the size of cultural groups as an indication of their political salience, the article seeks to answer why the cultural difference matters for politics in some circumstances but not others. 
                Posner posits that the political salience of cultural differences depends on the size of the group relative to the size of the political arena. 
                Specifically, he argues that should the groups be large enough to politically mobilize, then their salience will inevitably be more prominent, and thus, so will the likelihood of conflict. 
                These findings have important implications for the area of conflict resolution and mere understanding of cultural differences’ relationship with political salience."),
                p(""),
                p("Additional studies have also provided a framework or theory for why conflict exists in some societies but not others. Ellingsen helps to create a framework for theorizing why these compositions lead to conflict (2000). Ellingsen outlines how the measurement of the size of ethnic groups is useful in determining the level of conflict (2000). When looking at ethnic fractionalization it is useful to look closer to see the composition of the largest group, the number of ethnic groups, and the largest ethnic minority group. Ellingsen details that domestic conflict is most likely to occur in societies in which the dominant groups are large, but not too large (2000). What they mean by this is if power is concentrated in the hands of a single ethnic group, there is likely to be less power and increased deprivation among the others. However, the size of the additional groups must also be large enough to mobilize against the leading group.  
                Because of these theories, we added the feature of pop-ups to be able to interact with the map and see which countries are comprised of what sort of ethnic groups and their access to power and how that impacts the level of conflict they experience. While the map helps to visualize this, the graphs in the next tab look deeper into the statistical relationship between conflict and ethnic fractionalization."),
                width = 9
                ),
              ),
      ),
      
      
      # Third tab content
      tabItem(tabName = "analysis",
              h2("From Data to Insights: Visualizing the Relationship between Ethnic Fractionalization and Conflict"),
              fluidRow(
                box(title = "Plot",
                    p("The plot below examines the relationship between ethnic fractionalization and conflict, with the use of a regression line."),
                    plotOutput("ethnicplot", height = 400),
                    p("The hypothesis for this analysis is: a unit increase in ethnic fractionalization would increase the level of corruption. 
                      By looking at the plot, it can be seen that there is a positive relationship between the two variables, as indicated by the upward slope of the regression line. 
                      This means that an increase in ethnic fractionalization increases the level of conflict. 
                      The data points in the plot represent a single country. 
                      The points have been categorized by whether the country is a democracy or not. 
                      This allows us to identify whether there are possible differences in the relationship between ethnic fractionalization and conflict across different types of countries."),
                    p(""),
                    p("In the plot, we can observe that countries that are a democracy or non-democracy have similar relationships between ethnic fractionalization and conflict. 
                      Although, it is important to note that most of the outliers are non-democratic countries. 
                      This is insightful as non-democratic countries will likely not have the proper systems in place to manage such tensions which could increase the occurrence of conflict. 
                      Further, when done correctly, democratic systems can help to provide adequate representation of ethnic groups and limit the sentiments of deprivation or inequity that are commonly associated with ethnic differences."),
                    width = 9)
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "regression",
              h2("A Regression Analysis"),
              fluidRow(
                box(p("To further analyze the relationship between ethnic fractionalization and conflict, a regression analysis was carried out."),
                    p("The regression table below analyzes ethnic fractionalization as the predictor variable and conflict as the outcome variable. 
                      The coefficient for ethnic fractionalization is 3.964, this means that as ethnic fractionalization increases by one unit, the level of conflict increases by 3.964. 
                      It is important to note that although the coefficient seems to be quite high, ethnic fractionalization is measured from 0 to 1, which means that the coefficient demonstrates the increase of conflict when we move from 0 to 1. 
                      Furthermore, the p-value is less than 0.05, which indicates that it is statistically significant at a 95% confidence level, meaning that we can be confident that the relationship between ethnic fractionalization and conflict is not due to chance. 
                      The null hypothesis should be rejected as the p-value is less than 0.05. Therefore, we can conclude that there is evidence to support the hypothesis that an increase in ethnic fractionalization would increase the level of conflict."),
                    p(div(style = "display:flex;justify-content:center;align-items:center;height:100%",HTML("<i>Figure 1: Regression Table</i>"))),
                    div(style = "text-align: center;",img(src = "ethnic.png", height = 350, width = 400)),
                    p(""),
                    p("The findings suggest that higher levels of ethnic fractionalization are associated with increased levels of conflict, indicating that countries with greater ethnic diversity are more prone to conflict. 
                    This could be due to various reasons, such as historical issues, competition for resources, or political polarization along ethnic lines. 
                    These results can be valuable for policymakers that are interested in preventing and reducing conflict. 
                      By analyzing the relationship between ethnic fractionalization and conflict across countries, policymakers can work toward policies that help address the underlying causes of conflict and promote greater social cohesion."),
                    width = 9
                    )
              )
      ),
      
      # Fifth tab content
      tabItem(tabName = "conclusion",
              h2("Examining the Relationship between Ethnic Diversity and Conflict: Key Findings and Implications"),
              fluidRow(
                box(p(HTML("What can we conclude from all this information?"),
                      p("Certainly, there exists a relationship between conflict and ethnic fractionalization, but ultimately what this study shows is that the world is too complex, and countries' individual situations are too unique to come to any sort of all-encompassing conclusion. 
                      Circling back to the maps on the first tab, a user can explore the relationship between conflict and ethnic difference in a society, both through a measure of ethnic fractionalization and the composition of different ethnic groups. 
                      These measures allow the user to visually interact with a common measure of ethnic difference and its relationship with conflict as well as an in-depth analysis of countries of interest. 
                      For example, hovering over Bosnia and Herzegovina, a country that recently experienced brutal levels of conflict, one can see that there are three senior partners within the country, all likely competing for conflict. 
                      Looking at another country with high levels of conflict, Cameroon has five junior partners and one senior partner. 
                      However, looking on the other side of the coin, countries with low levels of conflict can also have a variety of different dynamics and ethnic groups yet still benefit from low levels of conflict such as Canada, the United States, and the United Kingdom. 
                      While no concrete conclusions can be drawn from this map, it helps to show how complex and unique each country's situation is and how these circumstances may or may not lead to conflict."),
                      p(""),
                      p("We then conducted a statistical analysis to try and test for a more concrete relationship. 
                      What can we conclude from the statistical portion of our study is: 
                      Based on our analysis, we have found that ethnic fractionalization does have an impact on conflict. 
                      The plots and regression analysis demonstrated that there is a relationship between the two, however, we cannot conclude that ethnic fractionalization impacts conflict. 
                      Seeing a relationship between the two variables does not imply that this is the only reason. 
                      This is because other factors could be playing a role in the effects of conflict, which means that there could be an interaction between conflict and other variables such as corruption and economic inequalities."),
                      ), width = 9)
                ),
      )
    ),
    # Add a footer 
    tags$footer("Copyright © Riya Sakhrani and Rachel Way")
  )
  )
)


server <- function(input, output, session) {
  # Conflict Plot
  output$ethnicplot <- renderPlot({
    ggplot(QoG, aes(x = al_ethnic2000, y = bti_ci)) +
      geom_point(aes(color = br_dem), jitter = list(x = 0.1, y = 0.1)) +
      labs(title="Ethnic Fractionalization and Conflict",
           x="Ethnic Fractionalization",
           y = "Conflict",
           color = " ") +
      geom_smooth(method = "lm")
  })
  
  # Corruption Plot
  output$corruptplot <- renderPlot({
    ggplot(QoG, aes(x = al_ethnic2000, y = ti_cpi)) +
      geom_point(jitter = list(x = 0.1, y = 0.1))+
      labs(title="Ethnic Fractionalization and Corruption",
           x="Ethnic Fractionalization",
           y = "Corruption") +
      geom_smooth(method = "lm")
  })
  
  # Create a reactive variable that corresponds to the correct data set depends on user input
  selected_var <- reactive({
    switch(input$variable,
           "Ethnic Fractionalization" = world_fracc$efrac,
           "Conflict" = world_fracc$bti_ci)
  })
  
  # Ethnic Fractionalization Map
  output$ethnicmap <- renderLeaflet({
    pal <- colorNumeric(palette = "Reds", domain = selected_var())
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(data = world_fracc, 
                  fillColor = ~pal(selected_var()),
                  fillOpacity = 0.6, 
                  weight = 1,
                  popup = paste0("<strong> Country: </strong> ", world_fracc$country, "<br>",
                                 "<strong>", input$variable, ":</strong> ", selected_var())) %>%
      setView(14, 30, zoom = 2) %>%
      addLegend(pal = pal,
                values = selected_var(),
                title = input$variable,
                position = "bottomright")
  })
  
  # Define a reactive data set based on slider input 
  filtered_data <- reactive({
    map_all %>%
      filter(al_ethnic2000 >= input$efrac_slider[1] & al_ethnic2000 <= input$efrac_slider[2])
  })
  
  # Conflict and Ethnic Fractionalization Map (Integrated)
  output$conflict_map <- renderLeaflet({
    
    con.pal <- colorNumeric(palette = "Blues", domain = Map.data$bti_ci)
    legend.labels <- c("Low", "Medium", "High")
    
    conflict.map <- filtered_data() %>% 
      leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(weight = 1,
                  color = "black",
                  fillColor = ~con.pal(bti_ci),
                  label = ~paste0(country, " Ethnic Fractionalization: ", al_ethnic2000),
                  highlight = highlightOptions(weight = 1,
                                               color = "navy",
                                               bringToFront = TRUE),
                  popup = ~paste0("<b>", filtered_data()$country,"<br>", "Status", "</b>", "<br>", filtered_data()$count_status_sum),
                  opacity=1,
                  fillOpacity=1) %>%
      addLegend(pal = con.pal,
                values = map_all$bti_ci,
                title = "Conflict level",
                labels = legend.labels,
                position = "bottomright")
    
  })
  
  # Load image for regression table 
  output$image <- renderUI({
    HTML(reg_table)
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
