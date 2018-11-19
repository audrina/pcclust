library(shiny)

bestPCSet <- read.csv(system.file("pcclust_visualization", "dfBestPC.csv", package = "pcclust"), sep=" ")
pcData <- read.csv(system.file("pcclust_visualization", "pcData.csv", package = "pcclust"), sep=" ")
pcData <- as.matrix(sapply(pcData, as.numeric))
bestPCSet$clusters <- as.factor(bestPCSet$clusters)

# Define UI

myUi <- fluidPage(
  titlePanel("Query point on PCA plot"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      sliderInput(
        inputId = "xcoord",
        label = "x-coordinate query pt.",
        min = round(min(pcData[ ,colnames(bestPCSet)[1]]) - 3, 2),
        max = round(max(pcData[ ,colnames(bestPCSet)[1]]) + 3, 2),
        value = 0
      )
      ,
      sliderInput(
        inputId = "ycoord",
        label = "y-coordinate query pt.",
        min = round(min(pcData[ ,colnames(bestPCSet)[2]]) - 3, 2),
        max = round(max(pcData[ ,colnames(bestPCSet)[2]]) + 3, 2),
        value = 0
      )
    ),

    mainPanel(textOutput(outputId = "coord")
              ,
              fluidRow(
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  # Output: PCA
                  plotOutput(outputId = "pca"),
                  # Output: lollipop
                  plotOutput(outputId = "lollipop")
                )

              ))
  )
)



myServer <- function(input, output) {

  vals <- reactiveValues()

  observe({

    if (is.null(input$xcoord)) {
      input$xcoord <- 0
    }

    if (is.null(input$ycoord)) {
      input$ycoord <- 0
    }

    xAxis <- bestPCSet[ ,1]
    yAxis <- bestPCSet[ ,2]

    nearestX <- findNearestNeighbor(xAxis, input$xcoord)
    nearestY <- findNearestNeighbor(yAxis, input$ycoord)

    # choose sample with the least combined absolute deviation from query

    closestSample <- chooseClosestSample(bestPCSet, input$xcoord, input$ycoord, nearestX, nearestY)

    # row index of sample that is nearest to query point
    vals$rowIdx <- which(pcData[ ,closestSample$colPC] == closestSample$val)

    vals$qLabel <- sprintf("QUERY: (%.2f,%.2f)", dfBestPC[vals$rowIdx, ][1], dfBestPC[vals$rowIdx, ][2])


  })

  output$pca <- renderPlot({

    ggplot(bestPCSet, aes_string(x=colnames(bestPCSet)[1], y=colnames(bestPCSet)[2])) +
      aes(color=`clusters`, shape=`clusters`, alpha=0.8) +
      geom_point(size=3) +
      guides(alpha=FALSE) +
      scale_color_brewer(palette="Dark2") +
      geom_rug() +
      stat_ellipse() +
      geom_text(data=bestPCSet[vals$rowIdx, ], color="red", size=3, label=vals$qLabel, alpha=1)

  })

  pcDf <- reactive(
    data.frame(PC=colnames(pcData), sample_value=round(pcData[vals$rowIdx, ], 2))
  )

  output$lollipop <- renderPlot({

    ggplot(pcDf(), aes(x=`sample_value`, y=PC, label=`sample_value`)) +
            geom_point(stat='identity', fill="blue", size=10)  +
            geom_segment(aes(x = 0,
                             y = `PC`,
                             xend = `sample_value`,
                             yend = `PC`),
                         color = "blue") +
            geom_text(color="white", size=3) +
            labs(title=sprintf("Diverging lollipop chart of PC breakdown for %s", vals$qLabel),
                 subtitle="Value of queried sample in terms of the PCs: Lollipop")

  })

  output$coord <- renderText({
    sprintf("The nearest neighbor to your query is %s\n", vals$qLabel)
  })

}

shinyApp(ui = myUi, server = myServer)

# [END]
