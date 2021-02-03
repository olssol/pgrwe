
## ----------------------------------------------------------------
## ----------------------------------------------------------------
##
##               UI COMPONENTS
##
## ----------------------------------------------------------------
## ----------------------------------------------------------------
div_age_dis <- function(grp) {
    rst <- list(numericInput(paste("inAMu", grp, sep = ""),
                             label = "Age mean", value = 60,
                             step = 0.25),
                numericInput(paste("inASd", grp, sep = ""),
                             label = "Age SD", value = 5, step = 0.1),
                numericInput(paste("inDis", grp, sep = ""),
                             label = "Prob(Disease = 1)",
                             value = 0.5,
                             step = 0.01,
                             min = 0.01, max = 0.99))
    if (2 == grp)
        rst <- c(rst,
                 list(numericInput("inN",
                              label = "Size",
                              value = 100,
                              step  = 50)))

    rst
}

box_summary <- function(grp = c(1, 2)) {
    lab <- c("Target Summary Statistics",
             "RWD Patients")[grp]

    lst_0 <- list(title = lab,
                 status = "primary",
                 width = 12,
                 ## height = 350,
                 solidHeader = TRUE,
                 collapsible = TRUE)

    lst <- c(lst_0, div_age_dis(grp))

    if (1 == grp) {
        lst <- c(lst,
                 list(checkboxInput("inChkAdj",
                                    label = "Show adjusted results",
                               value = F)))
    }

    do.call(shinydashboard::box, lst)
}

box_simu <- shinydashboard::box(
    title = "Outcome Setting",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 12,
    ## height = 350,
    numericInput("inInt",  label = "Intercept",            value = -5),
    numericInput("inBAge", label = "Coefficient: Age",     value = 0.1),
    numericInput("inBDis", label = "Coefficient: Disease", value = -2)
)

box_stats <- shinydashboard::box(
    title = "Observed and Adjusted RWD Statistics",
    status = "primary",
    width = 12,
    solidHeader = TRUE,
    collapsible = TRUE,
    tableOutput('tblStats')
)


box_data <- shinydashboard::box(
    title = "RWD Data",
    status = "primary",
    width = 12,
    solidHeader = TRUE,
    collapsible = TRUE,
    DT::dataTableOutput("tblData")
)

box_plot_age <- shinydashboard::box(
    title = "Age Distribution",
    status = "primary",
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("plotAge")
)

box_plot_disease <- shinydashboard::box(
    title = "Disease Distribution",
    status = "primary",
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("plotDisease")
)

box_plot_weights <- shinydashboard::box(
    title = "Weights",
    status = "primary",
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("plotWeights")
)


box_plot_post <- shinydashboard::box(
    title = "Posterior Distribution of Outcome",
    status = "primary",
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("plotPost")
)

box_plot_weights <- shinydashboard::box(
    title = "Weights",
    status = "primary",
    width = 6,
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("plotWeights")
)

## Header
header <- shinydashboard::dashboardHeader(
    title = "Demonstration of Entropy Balancing",
    titleWidth = 450
)

## Sidebar
sidebar <- shinydashboard::dashboardSidebar(disable = TRUE)

## Body
body <- shinydashboard::dashboardBody(
    tags$style(".shiny-file-input-progress {display: none}"),
    tags$head(tags$link(
                       rel = "stylesheet",
                       type = "text/css", href = "styles.css"
                   )),

    fluidRow(
        column(3,
               box_summary(2),
               box_summary(1),
               box_simu
               ),
        column(9,
               box_stats,
               box_plot_age,
               box_plot_disease,
               box_plot_post,
               box_plot_weights,
               box_data)
    )
)
