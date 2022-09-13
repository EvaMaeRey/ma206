---
    title: "Untitled"
author: "Evangeline Reynolds"
date: "9/12/2022"
output: html_document
---


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

Pi = .3
Complement = 1-Pi
PiTimesComplement = Pi*(Complement)
SqrtPiTimesComplement = sqrt(PiTimesComplement)



library(ggstamp)
ggcanvas() +
    stamp_segment(yend = 0) +
    stamp_point(size = 2, x = 1) +
    stamp_point(size = 2) +
    stamp_spoke(x0 = Pi, y0 = 0,
                angle = pi,
                radius = Pi,
                color = "cadetblue"
                ) +
    stamp_spoke(x0 = Pi, y0 = 0, angle = 0,
                radius = 1 - Pi,
                color = "goldenrod3") +
    stamp_point(x = Pi, y = 0, size = 3)


ggcanvas() +
    stamp_segment(yend = 0) +
    stamp_point(size = 2, x = 1) +
    stamp_point(size = 2) +
    stamp_spoke(x0 = Pi, y0 = 0,
                angle = pi,
                radius = Pi,
                color = "cadetblue"
    ) +
    stamp_spoke(x0 = Pi, y0 = 0, angle = 0:20/40 * pi,
                radius = 1 - Pi, alpha = 1/21:1,
                color = "goldenrod3") +
    stamp_point(x = Pi, y = 0, size = 3)

ggcanvas() +
    stamp_segment(yend = 0) +
    stamp_point(size = 2, x = 1) +
    stamp_point(size = 2) +
    stamp_spoke(x0 = Pi, y0 = 0,
                angle = pi,
                radius = Pi,
                color = "cadetblue"
    ) +
    stamp_spoke(x0 = Pi, y0 = 0, angle = 0:20/40 * pi,
                radius = 1 - Pi, alpha = 1/21:1,
                color = "goldenrod3") +
    stamp_point(x = Pi, y = 0, size = 3) +
    stamp_rect(xmin = 0, ymin = 0,
               xmax = Pi, ymax = 1-Pi,
               color = "cadetblue",
               fill = "cadetblue",
               alpha = .3)


ggcanvas() +
    stamp_segment(yend = 0) +
    stamp_point(size = 2, x = 1) +
    stamp_point(size = 2) +
    stamp_spoke(x0 = Pi, y0 = 0,
                angle = pi,
                radius = Pi,
                color = "cadetblue") +
    stamp_spoke(x0 = Pi, y0 = 0, angle = 0:20/40 * pi,
                radius = 1 - Pi, alpha = 1/21:1,
                color = "goldenrod3") +
    stamp_point(x = Pi, y = 0, size = 3) +
    stamp_rect(xmin = Pi - sqrt(Pi * (1-Pi)), ymin = 0,
               xmax = Pi, ymax = sqrt(Pi * (1-Pi)),
               color = "cadetblue",
               fill = "cadetblue",
               alpha = .3)


ggcanvas() +
    stamp_segment(yend = 0) +
    stamp_point(size = 2, x = 1) +
    stamp_point(size = 2) +
    stamp_spoke(x0 = Pi, y0 = 0,
                angle = pi,
                radius = Pi,
                color = "cadetblue"
    ) +
    stamp_spoke(x0 = Pi, y0 = 0, angle = 0:20/40 * pi,
                radius = 1 - Pi, alpha = 1/21:1,
                color = "goldenrod3") +
    stamp_point(x = Pi, y = 0, size = 3) +
    stamp_rect(xmin = Pi - sqrt(Pi * (1-Pi)), ymin = 0,
               xmax = Pi, ymax = sqrt(Pi * (1-Pi)),
               color = "cadetblue",
               fill = "cadetblue",
               alpha = .3) +
    ggxmean::stamp_normal_dist(mean = Pi, sd = SqrtPiTimesComplement)


# Run the application
shinyApp(ui = ui, server = server)
