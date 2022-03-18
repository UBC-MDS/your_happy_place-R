library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

dat <- readr::read_csv(here::here('data','processed', 'us_counties_processed.csv'))
bar <- dat %>% group_by(county) %>% summarise(percent_unemployed_CDC = mean(percent_unemployed_CDC),
                                              population_density_per_sqmi= mean(population_density_per_sqmi),
                                              percent_age_17_and_younger= mean(percent_age_17_and_younger),
                                              percent_age_65_and_older = mean(percent_age_65_and_older))

line <- dat %>% group_by(county,month) %>% summarise(mean_temp = mean(mean_temp),
                                                     precipitation= mean(precipitation),
                                                     max_temp= mean(max_temp),
                                                     min_temp = mean(min_temp),
                                                     rain = mean(rain),
                                                     snow = mean(snow))

app$layout(
  dbcContainer(
    dbcCol(
      list(
        dbcRow(
          list(
            htmlH2(
              "Your Happy Place"
            )
          )
        ),
        dbcRow(
          list(
            dccDropdown(
              id='col-select',
              options = as.list(unique(bar$county)),
              multi = T,
              value=list('Alachua','Accomack'))
          )
        ),
        dbcRow(
          list(
            dbcCol(
              
              list(
                dccGraph(id='unemplot'),
                dccGraph(id='popplot'),
                dccGraph(id='17plot'),
                dccGraph(id='65plot')
              )
            ),
            dbcCol(
              list(
                dccGraph(id='tempplot'),
                dccGraph(id='maxplot'),
                dccGraph(id='minplot')
              )
            ),
            dbcCol(
              list(
                dccGraph(id='perplot'),
                dccGraph(id='rainplot'),
                dccGraph(id='snowplot')
              )
            )
          )
        )
      )
    )
  )
)
app$callback(
  output('unemplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    bar1 <- bar %>%
      filter(county %in% (xcol))
    p <- ggplot(bar1) +
      aes(x = county,
          y = percent_unemployed_CDC,fill=county) +
      geom_bar(stat="identity")+
      ggtitle('percent_unemployed_CDC')
    ggplotly(p)
  }
)

app$callback(
  output('popplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    bar1 <- bar %>%
      filter(county %in% xcol)
    p <- ggplot(bar1) +
      aes(x = county,
          y = population_density_per_sqmi,fill=county) +
      geom_bar(stat="identity")+
      ggtitle('population_density_per_sqmi')
    ggplotly(p)
  }
)
app$callback(
  output('17plot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    bar1 <- bar %>%
      filter(county %in% xcol)
    p <- ggplot(bar1) +
      aes(x = county,
          y = percent_age_17_and_younger,fill=county) +
      geom_bar(stat="identity")+
      ggtitle('percent_age_17_and_younger')
    ggplotly(p)
  }
)
app$callback(
  output('65plot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    bar1 <- bar %>%
      filter(county %in% xcol)
    p <- ggplot(bar1) +
      aes(x = county,
          y = percent_age_65_and_older,fill=county) +
      geom_bar(stat="identity")+
      ggtitle('percent_age_65_and_older')
    ggplotly(p)
  }
)

app$callback(
  output('tempplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = month,
                           y = mean_temp,
                           color = county)) +
      geom_line() +
      xlab("Month") +
      ylab("Mean Temperature (F)") +
      ggtitle("Mean temperature by county") +
      labs(color="county")
    ggplotly(p)
  }
)    

app$callback(
  output('perplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = month,
                           y = precipitation,
                           color = county)) +
      geom_line() +
      xlab("Month") +
      ylab("Precipitation") +
      ggtitle("Precipitation by county") +
      labs(color="county")
    ggplotly(p)
  }
)  
app$callback(
  output('maxplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = month,
                           y = max_temp,
                           color = county)) +
      geom_line() +
      xlab("Month") +
      ylab("max_temp") +
      ggtitle("max_temp by county") +
      labs(color="county")
    ggplotly(p)
  }
) 
app$callback(
  output('minplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = month,
                           y = min_temp,
                           color = county)) +
      geom_line() +
      xlab("Month") +
      ylab("min_temp") +
      ggtitle("min_temp by county") +
      labs(color="county")
    ggplotly(p)
  }
) 
app$callback(
  output('rainplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = month,
                           y = rain,
                           color = county)) +
      geom_line() +
      xlab("Month") +
      ylab("Rain") +
      ggtitle("Rain by county") +
      labs(color="county")
    ggplotly(p)
  }
) 
app$callback(
  output('snowplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = month,
                           y = snow,
                           color = county)) +
      geom_line() +
      xlab("Month") +
      ylab("Snow") +
      ggtitle("Snow by county") +
      labs(color="county")
    ggplotly(p)
  }
) 




app$run_server(host = '0.0.0.0')
