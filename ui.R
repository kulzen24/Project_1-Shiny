shinyUI(dashboardPage( skin = 'purple',
  dashboardHeader(title = 'TA CORE'),
 
  dashboardSidebar(
    sidebarUserPanel('Away From Home', image = 'https://f332c00d2e7d232dccae-7fb50b218d153ea70ad48d487f749494.ssl.cf2.rackcdn.com/9c45897cb8616a084ea7d28e4479c64d-aee66fc22389a251628d262dc5770488.png'),
   
    sidebarMenu(
      menuItem('Map', tabName = 'map', icon = icon('globe')),
      menuItem('Match-Maker', tabName = 'match-maker', icon = icon('handshake-o')),
      menuItem('Insights', tabName = 'insights', icon = icon('flask')))
  ),
  
  dashboardBody(tabItems(
    tabItem(tabName = 'map',
            selectizeInput('selected',
                        'Select Statistic to Display', #gradient maps | gray out inactive hoods
                        choice),
            fluidRow(box(id = 'box1', leafletOutput('la_map'), height = 'auto', weight = 250, collapsible = TRUE), #make city name dynamic
                     box(id = 'box2', leafletOutput('ny_map'), height = 'auto', weight = 250, collapsible = TRUE)),
            fluidRow(infoBoxOutput('maxBox1',6),
                     infoBoxOutput('maxBox2',6)),
            fluidRow(infoBoxOutput('minBox1',6),
                     infoBoxOutput('minBox2',6)),
            fluidRow(infoBoxOutput('medBox1',6),
                     infoBoxOutput('medBox2',6))
            ),
    tabItem(tabName = 'match-maker',
            column(12, align = 'center', (selectizeInput('city1',
                           'Select a City',
                           city))),
            tags$div(
              tags$br(),
              tags$br()
            ),
            column(12, align = 'center', (selectizeInput('hood',
                           'Select a Neighborhood',
                           hoods))),
            column(12, align = 'center', (selectizeInput('compare_on',
                           'Select Statistic to Display',
                           feature))),
            column(12, align = 'center', (selectizeInput('city2',
                           'Select a City to compare',
                           city))),
            tags$div(
              tags$br(),
              tags$br(),
              fluidRow(column(12, align = 'center', tags$h1('Similar Neighborhoods'))),
              #Add URL feature to datasets and paste them here
              fluidRow((div(style = 'align: center', valueBoxOutput('basehood',6)))),
              fluidRow(valueBoxOutput('match1',4),
                       valueBoxOutput('match2',4),
                       valueBoxOutput('match3',4))
               #fluidRow(box(DT::dataTableOutput('matches'), width = 12))
             )
    ),
    tabItem(tabName = 'insights',
            fluidRow(box(htmlOutput('income_ed'))),
            fluidRow(box(htmlOutput('vio_prop'))),
            fluidRow(box(htmlOutput('size_income'))),
            fluidRow(box(htmlOutput('histovio'))),
            fluidRow(box(htmlOutput('histoprop'))),
            fluidRow(box(htmlOutput('histo_age'))),
            fluidRow(box(htmlOutput('histo_ed'))),
            fluidRow(box(htmlOutput('transitscores'))),
            #Trying to make an auto-sort function
            fluidRow(selectizeInput('selectedjr',
                            'Select Statistic to Highlight',
                            choice)),
            fluidRow(box(DT::dataTableOutput('table'), width = 12))
            ) #Dashboard; add dashboard boxes above the table
  ))
)
)