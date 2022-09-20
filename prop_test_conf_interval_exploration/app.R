#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#'

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hypothesized proportion under the null"),


    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("pi",
                        "Size of PI:",
                        min = 0,
                        max = 1,
                        value = .3333),
            sliderInput("n",
                        "Sample Size, n:",
                        min = 20,
                        max = 200,
                        value = 20),
            shiny::textInput("phat",
                        "p-hat:",
                        value = .3333)

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("distText")
        )
    ),

    # titlePanel(x_mod)


)

c(rep(1, 30), rep(0,3)) ->
    cancer_dog_performance # :-)

sd_prop <- function(pi, n){

    sqrt(pi*(1-pi)/n)

}

stamp_dist_null_pi_n <- function(pi, n, height){

    ggxmean:::stamp_normal_dist(sd = sd_prop(pi, n),
                                mean = pi,
                                height = 30)

}

"        ggstamp::ggcanvas() +
            ggstamp::stamp_point(x = input$pi, y = 0) +
            geom_vline(xintercept = input$pi, linetype = 'dashed') +
            coord_cartesian() +
            geom_vline(xintercept = 1/3) +
            scale_x_continuous(limits = 0:1) +
            stamp_dist_null_pi_n(pi = input$pi, n = input$n)" ->
    for_shiny



phat <- mean(cancer_dog_performance)



# Define server logic required to draw a histogram
server <- function(input, output) {





    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R


    eval(parse(text = for_shiny))

    })

    output$distText <- renderText({

        for_shiny %>%
            str_replace_all("input\\$pi", as.character(input$pi)) %>%
            str_replace_all("input\\$n", as.character(input$n))

    })


}

# Run the application
shinyApp(ui = ui, server = server)

#' ->
#my_shiny_app


#eval(parse(text = my_shiny_app))

