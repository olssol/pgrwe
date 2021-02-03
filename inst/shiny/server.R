require(ggplot2)

## Server
shinyServer(function(input, output, session) {

    ##---------------------------------------------------------------
    ##---------LOAD FUNCTIONS----------------------------------------
    ##---------------------------------------------------------------
    source("pg_shiny_tools.R",  local = TRUE)

    ## ----------------------------------------------------
    ##           VALUES
    ## ----------------------------------------------------

    ## simulated RWD Data
    rv_simu_data <- reactive({
        if (is.null(input$inAMu2))
            return(NULL)

        p_age     <- c(input$inAMu2, input$inASd2)
        p_disease <- c(input$inDis2)
        n         <- input$inN
        beta      <- c(input$inInt, input$inBAge, input$inBDis)
        rst       <- simu_data(n, p_age, p_disease, beta)

        rst
    })

    ## truth
    rv_target_truth <- reactive({
        if (is.null(input$inAMu1))
            return(NULL)

        p_age     <- c(input$inAMu1, input$inASd1)
        p_disease <- c(input$inDis1)
        beta      <- c(input$inInt, input$inBAge, input$inBDis)
        rst       <- simu_data(10000, p_age, p_disease, beta)

        mean(rst$y)
    })

    rv_sum_stats <- reactive({
        if (is.null(input$inAMu1))
            return(NULL)

        p_age     <- c(input$inAMu1, input$inASd1)
        p_disease <- c(input$inDis1)

        set_sum_stat(p_age, p_disease)
    })

    rv_eb_data <- reactive({
        dat   <- rv_simu_data()
        stats <- rv_sum_stats()

        if (is.null(dat) | is.null(stats))
            return(NULL)

        get_weights(stats, dat)
    })


    ## --------------------------------------------------------
    ##                    UI Update
    ## --------------------------------------------------------
    output$tblData <- DT::renderDataTable({
        eb_dat <- rv_eb_data()
        if (is.null(eb_dat))
            return(NULL)

        rst <- eb_dat$dat
        if (!input$inChkAdj) {
            rst$weights <- 1 / nrow(rst)
        }

        rst <- rst[order(rst$weights), ]
    },
    rownames  = NULL,
    selection = "none",
    options   = list(pageLength = 10))

    output$tblStats <- renderTable({
        eb_dat <- rv_eb_data()
        if (is.null(eb_dat))
            return(NULL)

        rst <- eb_dat$stats
        if (!input$inChkAdj)
            rst <- rst[-2, , drop = F]

        rst
    }, rownames = TRUE)

    output$plotAge <- renderPlot({
        eb_dat <- rv_eb_data()
        if (is.null(eb_dat))
            return(NULL)

        plot_age(eb_dat$dat, input$inChkAdj)
    }, bg = "transparent")

    output$plotDisease <- renderPlot({
        eb_dat <- rv_eb_data()
        if (is.null(eb_dat))
            return(NULL)

        plot_disease(eb_dat$dat, input$inChkAdj)
    }, bg = "transparent")

    output$plotPost <- renderPlot({
        eb_dat <- rv_eb_data()

        if (is.null(eb_dat))
            return(NULL)

        plot_posterior(eb_dat$dat,
                       truth = rv_target_truth(),
                       adj = input$inChkAdj)
    }, bg = "transparent")

    output$plotWeights <- renderPlot({
        eb_dat <- rv_eb_data()

        if (is.null(eb_dat))
            return(NULL)

        plot_weights(eb_dat$dat, input$inChkAdj)
    }, bg = "transparent")
})
