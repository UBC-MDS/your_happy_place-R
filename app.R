library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(readr)
library(here)
library(purrr)
library(ggthemes)
library(shiny)
library(scales)


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

dat <- readr::read_csv(here::here('data','processed', 'us_counties_processed.csv'))
bar <- dat %>% group_by(state,county) %>% summarise(percent_unemployed_CDC = mean(percent_unemployed_CDC),
                                              population_density_per_sqmi= mean(population_density_per_sqmi),
                                              percent_age_17_and_younger= mean(percent_age_17_and_younger),
                                              percent_age_65_and_older = mean(percent_age_65_and_older))


line <- dat %>% group_by(state,county,month) %>% summarise(mean_temp = mean(mean_temp),
                                                     precipitation= mean(precipitation),
                                                     max_temp= mean(max_temp),
                                                     min_temp = mean(min_temp),
                                                     rain = mean(rain),
                                                     snow = mean(snow))

line$Date<-as.Date(with(line,paste('2020',month,'1',sep="-")),"%Y-%m-%d")
#head(line)

app$layout(
  dbcContainer(
    dbcCol(
      list(
        dbcRow(
          list(
            htmlH2(
              "Your Happy Place", style=list('padding' = '20px')
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
        dbcContainer(
        dbcRow(
          list(
          dbcTabs(
            list(
              dbcTab(label='Social',
                    dbcRow(list(
                     dbcCol(list(
                         dccGraph(id='unemplot'),
                         dccGraph(id='popplot'))
                     ), 
                     dbcCol(list(
                         dccGraph(id='17plot'),
                         dccGraph(id='65plot'))
                     ))
                    ),style=list('padding-top' = '20px')),
              dbcTab(label='Climate', 
                dbcRow(
                  list(
                dbcCol(
                  list(
                    dccGraph(id='tempplot'),
                    dccGraph(id='maxplot'),
                    dccGraph(id='minplot')
                  ), width=6
                ),
                dbcCol(
                  list(
                    dccGraph(id='perplot'),
                    dccGraph(id='rainplot'),
                    dccGraph(id='snowplot')
                  ), width=6
                )
                  )),style=list('padding-top' = '20px')
              )
            ),style=list('padding-top' = '20px')
          )
          )
        ))
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
          y = percent_unemployed_CDC,fill=state,linetype=county) +
      geom_bar(stat="identity")+
      ggtitle('Percent Unemployed')+
      labs(y="Unemployed (%)")
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
          y = population_density_per_sqmi,fill=state,linetype=county) +
      geom_bar(stat="identity")+
      ggtitle('Population Density')+
      labs(y='Density (sqmi)')
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
          y = percent_age_17_and_younger,fill=state,linetype=county) +
      geom_bar(stat="identity")+
      ggtitle('Young Population: 17 and Below') +
      labs(y='Under 18yo (%)')
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
          y = percent_age_65_and_older,fill=state,linetype=county) +
      geom_bar(stat="identity")+
      ggtitle('Senior Population: 65 and Up') +
      labs(y='Over 64yo (%)')
    ggplotly(p)
  }
)


app$callback(
  output('tempplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = Date,
                           y = mean_temp,
                           color = state,linetype=county)) +
      geom_line() +
      xlab("Month") +
      ylab("Mean Temperature (F)") +
      ggtitle("Mean Temperature by County") +
      labs(color="county")+scale_x_date(labels = date_format("%b"))
    ggplotly(p)
  }
)    

app$callback(
  output('perplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = Date,
                           y = precipitation,
                           color = state,linetype=county)) +
      geom_line() +
      xlab("Month") +
      ylab("Precipitation (in)") +
      ggtitle("Precipitation by County") +
      labs(color="county") +
      labs(color="county")+scale_x_date(labels = date_format("%b"))
    ggplotly(p)
  }
)  
app$callback(
  output('maxplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = Date,
                           y = max_temp,
                           color = state,linetype=county)) +
      geom_line() +
      xlab("Month") +
      ylab("Max Temperature (F)") +
      ggtitle("Max Temperature by County") +
      labs(color="county") +
      labs(color="county")+scale_x_date(labels = date_format("%b"))
    ggplotly(p)
  }
) 
app$callback(
  output('minplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = Date,
                           y = min_temp,
                           color = state,linetype=county)) +
      geom_line() +
      xlab("Month") +
      ylab("Min Temperature (F)") +
      ggtitle("Min Temperature by County") +
      labs(color="county") +
      labs(color="county")+scale_x_date(labels = date_format("%b"))
    ggplotly(p)
  }
) 
app$callback(
  output('rainplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = Date,
                           y = rain,
                           color = state,linetype=county)) +
      geom_line() +
      xlab("Month") +
      ylab("Rain (in)") +
      ggtitle("Rain by County") +
      labs(color="county") +
      labs(color="county")+scale_x_date(labels = date_format("%b"))
    ggplotly(p)
  }
) 
app$callback(
  output('snowplot', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    line1 <- line %>%
      filter(county %in% xcol)
    p <- ggplot(line1, aes(x = Date,
                           y = snow,
                           color = state,linetype=county)) +
      geom_line() +
      xlab("Month") +
      ylab("Snow (in)") +
      ggtitle("Snow by County") +
      labs(color="county") +
      labs(color="county")+scale_x_date(labels = date_format("%b"))
    ggplotly(p)
  }
) 

app$run_server(host = '0.0.0.0')
