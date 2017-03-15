library(shiny)

fluidPage(
  tabsetPanel(
    tabPanel("Анализ",
      fluidRow(
        headerPanel('Анализ СВНЦ на электроэнергию и мощность по ГП')),
        fluidRow(
          column(4, 
          h3('Фильтры'),
          selectInput(inputId = "pcode", label = "Сначала выберите коды участников", 
                      choices = pcodes, multiple = TRUE),
          uiOutput(inputId = "regs", "regions"),
          selectInput(inputId = "grapIndicator", label = "Выберите показатель", 
                      choices = c("СВНЦ на электроэнергию", "СВНЦ на мощность")),
          downloadButton('downloadFactData', 'Сохранить данные')),
          column(8, h3('Динамика СВНЦ'),
                 plotOutput('fact_svnc'))
          ),
        fluidRow(
          tableOutput('svnc_table')
      )
    ),
    tabPanel("Прогноз",
      fluidRow(
        headerPanel('Прогнозирование СВНЦ на электроэнергию и мощность по ГП')),
      fluidRow(
        # every column breaks in 12 units
        column(4,
          h3("Параметры прогноза"),
          selectInput(inputId = "pcd_forecast", label = "Выберите код участника", 
                      choices = pcodes_fcst),
          uiOutput(inputId = "regs_forecast", "regions_forecast"), # reactive list with regions
          fluidRow(
            column(4, 
                    actionButton(inputId = "launchForecast", label = "Прогноз")
                   ),
            column(8,
                    checkboxInput("all", label = "По всем парам ГП-регион", value = FALSE)
                    )
            
          ),
          fluidRow(
            column(4,
                   downloadButton('downloadForecastData', 'Сохранить данные')    
                  ),
            column(
              8
            )
          )
        ),
        column(8 
          
        )
      ),
      fluidRow(
        h3("Результаты прогноза на ", 
           as.character(format(as.Date(cut(max(svnc_data$TDATE) + 62, "month")), "%b %Y")))
      ),
      fluidRow(
        tableOutput('svnc_n_fcst')
      )
    )
  )
)