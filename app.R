library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(gapminder)
library(tidyverse)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# load data
gapminder_data <- gapminder

# function get continents
get_continents <- function() {
  return (unique(gapminder_data$continent))
}

# function get countries
get_countries <- function() {
  return (unique(gapminder_data$country))
}

create_continents_list <- function() {
  continent_list <- list()
  for (continent in get_continents()) {
    continent_list <- append(continent_list,
                             list(list("label" = continent,
                                       "value" = continent)))
  }
  return(continent_list)
}

create_countries_list <- function() {
  country_list <- list()
  for (country in get_countries()) {
    country_list <- append(country_list,
                           list(list("label" = country,
                                     "value" = country)))
  }
  return(country_list)
}

# Sync continents and countries in filter
app$callback(
  output("country-selector", 'options'),
  list(input("continent-selector", 'value')),
  function(selected_continent = "Europe") {
    valid_countries <- gapminder_data %>%
      filter(continent == selected_continent)
    valid_countries <- droplevels.factor(unique(valid_countries$country))
    
    country_options <- list()
    for (country in valid_countries) {
      country_options <- append(country_options,
                                list(list("label" = country,
                                          "value" = country)))
    }
    return(country_options)
  }
)


app$callback(
  output('timeseries_plot', 'figure'),
  list(input('continent-selector', 'value'),
       input("country-selector", "value"),
       input("time-series-col", "value")),
  
  function(selected_continent="Europe", selected_countries="Austria", timeseries_col = time-series-col){
    if ((!is.null(selected_countries)) | (!length(selected_countries) == 0)){
          plot_data <- gapminder_data %>%
            filter(continent == selected_continent & country %in% selected_countries)
     
    plot <- ggplot(plot_data) +
            geom_line(aes(x=year,
                          y = !!sym(timeseries_col),
                          color = country))
    }
    
    else {
      plot_data <- gapminder_data %>%
        group_by(continent, year) %>%
        mutate(timeseries_col = mean(!!sym(timeseries_col))) %>%
        filter(continent == selected_continent)
      
      plot <- ggplot(plot_data) +
        geom_line(aes(x=year,
                      y = !!sym(timeseries_col),
                      color = continent))
    }
    ggplotly(plot)
  })

  
app %>% set_layout(
  h1('GapminderDash'),
    dbcContainer(
      dbcRow(
        list(
          dbcCol(
            list(
              htmlLabel('Select Continent'),
              dccDropdown(
                id = "continent-selector",
                options = create_continents_list(),
                value = 'Europe'
              )
            )
          ),
          dbcCol(
            list(
              htmlLabel('Select Country'),
              dccDropdown(
                id = "country-selector",
                options = create_countries_list(),
                value = "Austria",
                multi = TRUE
              )
            )
          )
        )
      ), style = list('max-width' = '85%')  # Change left/right whitespace for the container
    ),
  dbcContainer(
    list(
      dccTabs(
        id = "time-series-col",
        value = "gdpPercap",
        children = list(
          dccTab(
            label = "GDP",
            value = "gdpPercap",
          ),
          dccTab(
            label = "Life Expectancy",
            value = "lifeExp",
          )
        )
      )
    )),
  dbcCard(
    dbcCardBody(
      list(
        h4("Time Series Plot", className = "card-title"),
        dccGraph(id="timeseries_plot")
      )
    )
  ),
  htmlLabel('   Note: Please select both Continent and Country to view the modified plot.')
  
)

# Run the app
app$run_server(host = '0.0.0.0')