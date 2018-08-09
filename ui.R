library(shiny)
shinyUI(fluidPage(
        titlePanel("Historical NFL (American Football) Data"),
        sidebarLayout(
                sidebarPanel(
                        numericInput("start", "What year to start from?", 
                                     value = 2011, min = 1966, max = 2017, step = 1),
                        numericInput("end", "What year to end?",
                                     value = 2011, min = 1966, max = 2017, step = 1),
                        radioButtons("stat","Choose which type of stat to display",
                                     c("Points" = "Points",
                                       "Yards" = "Yards",
                                       "Turnovers" = "Turnovers")),
                        submitButton("Submit")
                ),
                mainPanel(
                        h5("Below, the two tabs will show historical data collected
                           from boxscores for games played in the NFL (American
                           Footabll). On the left, you can specify which years
                           to collect data from, and which particular statistic
                           to display. \"Points\" represents points scored by the
                           winning team, \"Yards\" represents yards gained
                           by the winning team, and \"Turnovers\" represents
                           the number of turnovers committed by the winning team.
                           For any selection, the two tabs below will show: 1)
                           A model showing how the selected statistic effects
                           win probability; and 2) A histogram showing the number
                           of teams to accumulate different amounts of the selected
                           statistic."),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Prediction", br(),plotOutput("plot2")),
                                    tabPanel("Histogram",br(),plotOutput("plot1"))
                        ),
                        h6("Note: A logistic model was used to fit Points and Yards,
                           while an exponential model was used to fit Turnovers")
                )
        )
))