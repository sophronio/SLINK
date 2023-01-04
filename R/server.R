
library(shiny)
#library(reticulate)
library(fastLink)
library("DT")
library("ggplot2")
library(dplyr)
library(bslib)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library("stringr")
library("stats")
library("utils")



#' Defines the server logic for the FastLinkGUIne app
#' @description This function is the backend for the FastLinkGUIne App.
#' @export
#' @import fastLink
#' @import DT
#' @import shinyjs
#' @import ggplot2
#' @import dplyr
#' @import bslib
#' @import shinythemes
#' @import stringr
#' @importFrom stats setNames
#' @importFrom utils View read.csv write.csv
#' @rawNamespace import(shiny, except=c(dataTableOutput,renderDataTable,runExample))
#' @rawNamespace import(shinyWidgets, except = alert)
#' @param input - input from UI
#' @param output - output to UI
#' @param session - user session
server <- function(input, output, session) {
  ID <- NULL
  r <- reactiveValues(nHeaders = 0, headersLinked = FALSE, leftSelect = list(), rightSelect= list())
  disable(selector = '.navbar-nav a[data-value="Results"')
  disable(selector = '.navbar-nav a[data-value="Review"')
  createDropdown <- FALSE
  onclick("info", function() {output$infoHelp <- renderUI({
    showModal(modalDialog(
      title = "Information",
      HTML("<ul>
            <li>stringdist.match: A vector of variable names indicating which variables should use string distance matching. Must be a subset of 'varnames' and must not be present in 'numeric.match'.</li>
            <li>numeric.match: A vector of variable names indicating which variables should use numeric matching. Must be a subset of 'varnames' and must not be present in 'stringdist.match'.</li>
            <li>partial.match: A vector of variable names indicating whether to include a partial matching category for the string distances. Must be a subset of 'varnames' and 'stringdist.match'.</li>
            <li>cut.a: Lower bound for full string-distance match, ranging between 0 and 1. Default is0.94</li>
            <li>cut.p: Lower bound for partial string-distance match, ranging between 0 and 1. Default is 0.88</li>
            <li>estimate.only: Whether to stop running the algorithm after the EM step (omitting getting the matched indices of dataset A and dataset B). Only the EM object will be returned. Can be used when running the match on a random sample and applying to a larger dataset, or for out-of-sample prediction of matches.</li>
         </ul>"),
      easyClose = TRUE

    ))})})
  #Data frames created using input csv files
  mydata1 <- reactive({
    inFile1 <- input$file1
    ext <- tools::file_ext(inFile1$datapath)

    if (is.null(inFile1))
      return(NULL)

    req(inFile1)
    validate(need(ext == "csv", "Please upload a csv file."))

    read.csv(inFile1$datapath)
  })
  mydata2 <- reactive({
    inFile2 <- input$file2
    ext <- tools::file_ext(inFile2$datapath)

    if (is.null(inFile2))
      return(NULL)

    req(inFile2)
    validate(need(ext == "csv", "Please upload a csv file."))

    read.csv(inFile2$datapath)
  })

  #Checks if both files inputted and if the headers are the same for both.
  returnSame <- function(a, b) {
    return(c(validate(need(SameElements(a, b), "Please make sure that both files have the same headers.")), TRUE))
  }
  SameElements <- function(a, b) return(identical(sort(a), sort(b)))
  observe({
    if (is.null(input$file1) || is.null(input$file2))
      return(NULL)
    cnames1 <- colnames(mydata1())
    cnames2 <- colnames(mydata2())
    satisfied <- FALSE
    output$nHeaderIncrease <- renderUI({
      actionButton("increaseH", "Add Column to link")
    })
    output$nHeaderAccept <- renderUI({
      actionButton("acceptH", "Accept")
    })
    if (r$headersLinked) {
      updateCheckboxGroupInput(session, inputId = "show_vars", choices = r$leftSelect)
      updateCheckboxGroupInput(session, inputId = "showCols", choices = union(colnames(mydata1()), colnames(mydata2())), selected = union(colnames(mydata1()), colnames(mydata2())))
      output$dropdown2 <-renderUI({
        lapply(1:r$nHeaders, function(i){
          disabled(
            selectInput(paste0(r$leftSelect[i]), label=NULL, choices=c("partial match", "stringdist match", "numeric match"), multiple= TRUE)
          )

        })
      })
      output$selectCSS <- renderUI({
        tags$style(type="text/css", "
                 .selectize-control{
                    margin-left:0px !important;
                    width: 400px;
                    height: 25px;
                 }")
      })
      output$checkboxCSS <- renderUI({
        tags$style(type="text/css", "
                  .checkbox{
                    padding-top:10px;
                    padding-bottom:5px;
                  }")
      })

    }
  })
  h <- reactiveValues(headerChosen = FALSE)
  observeEvent(input$increaseH, {
    h$headerChosen <- TRUE
    cnames1 <- colnames(mydata1())
    cnames2 <- colnames(mydata2())
    tmp <- r$nHeaders + 1
    r$nHeaders <- r$nHeaders + 1
    insertUI(".headerLeft", ui={
      div(selectInput(paste0("hInputL",as.character(tmp)), paste0("Header ",as.character(tmp)," Left"), cnames1))
    })
    insertUI(".headerRight", ui={
      div(selectInput(paste0("hInputR",as.character(tmp)), paste0("Header ",as.character(tmp)," Right"), cnames2))
    })
    h$headerChosen <- TRUE
  })
  observeEvent(input$acceptH, {
    output$acceptError <- renderText({validate(
      need(h$headerChosen == TRUE, "Please select at least one header.")
    )})
    if (h$headerChosen) {
      for (i in 1:r$nHeaders) {
        r$leftSelect <- append(r$leftSelect, list(eval(parse(text=gsub(" ", "",paste("input$hInputL",as.character(i), collapse=''))))))
        r$rightSelect <- append(r$rightSelect, list(eval(parse(text=gsub(" ", "",paste("input$hInputR",as.character(i), collapse=''))))))
      }
      r$headersLinked <- TRUE
    }
  })
  output$dataset1 <- DT::renderDataTable({
    mydata1()
  })
  output$dataset2 <- DT::renderDataTable({
    mydata2()
  })

  observeEvent(input$show_vars, {
    cnames1 = colnames(mydata1())
    for (var in cnames1) {
      if (var %in% input$show_vars) {
        enable(id=var)
      } else {
        disable(id=var)
      }

    }

  })
  #Get Partials
  getPartials <- function() {
    partials <- NULL
    count <- 0
    for (var in input$show_vars) {
      if ("partial match" %in% input[[var]] && count == 0) {
        partials <- c(var)
        count <- count + 1
      } else if ("partial match" %in% input[[var]]) {
        partials <- c(partials, var)
      }
    }
    return(partials)
  }
  #Get StringDist
  getStringDist <- function() {
    stringdist <- NULL
    count <- 0
    for (var in input$show_vars) {
      if ("stringdist match" %in% input[[var]] && count == 0) {
        stringdist <- c(var)
        count <- count + 1
      } else if ("stringdist match" %in% input[[var]]) {
        stringdist <- c(stringdist, var)
      }
    }
    return(stringdist)
  }
  getNumeric <- function() {
    numericM <- NULL
    count <- 0
    for (var in input$show_vars) {
      if ("numeric match" %in% input[[var]] && count == 0) {
        numericM <- c(var)
        count <- count + 1
      } else if ("numeric match" %in% input[[var]]) {
        numericM <- c(numericM, var)
      }
    }
    return(numericM)
  }

  check <- function() {
    return(c(validate(
      need(!is.null(input$file1), "Please input the first dataset."),
      need(!is.null(input$file2), "Please input the second dataset."),
      need(!is.null(input$show_vars), "Please select the headers to link."),
      need(!is.null(input$threshold), "Please enter a threshold value.")
    ), TRUE))
  }

  checkDedupe <- function() {
    if (!is.null(input$dedupe) && input$dedupe == "Yes") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  dataframeValues <- reactiveValues(df=NULL, rownum = 0, bigDf = NULL, accepted1 = NULL, accepted2 = NULL, unaccepted1 = NULL, unaccepted2 = NULL, unmatchedDataA = NULL, unmatchedDataB = NULL, tmpDataA = NULL, matched1 = NULL, matched2 = NULL, nologic1 = NULL, nologic2 = NULL, test="a")
  observeEvent(input$link, {
    dataframeValues$test <- "b";
    satisfied <- FALSE
    output$txt2 <- renderText({check()[0]})
    satisfied <- check()[1]
    if (satisfied) {
      #Transform mydata2() from it's col names to dataset 1 col names
      dataframeValues$test <- "c";
      tmp1 <- mydata1()
      tmp2 <- mydata2()
      tmpDfa <- tmp1[unlist(r$leftSelect)]
      tmp3 <- unlist(r$rightSelect)
      tmpDfb <- tmp2[unlist(r$rightSelect)]
      colnames(tmpDfb) <- unlist(r$leftSelect)
      fastL.out <- NA
      # Create test variable for unit tests t oread
      dataframeValues$testDataA <- tmpDfa
      # Create test variable for unit tests to read
      dataframeValues$testDataB <- tmpDfb
      if (ncol(tmpDfa) == 1 && ncol(tmpDfb) == 1) {
        tmpDfa['dummy'] <- NA
        tmpDfb['dummy'] <- NA
      }
      fastL.out <- fastLink(dfA = tmpDfa, dfB = tmpDfb, varnames = input$show_vars, partial.match = getPartials(), stringdist.match = getStringDist(), threshold.match = input$threshold, numeric.match = getNumeric(), cut.a = input$cutA, cut.p = input$cutP, dedupe.matches = checkDedupe())
      matched_dfs <- getMatches(
        dfA = tmpDfa, dfB = tmpDfb, threshold.match = input$threshold,
        fl.out = fastL.out, combine.dfs = FALSE
      )
      dataframeValues$matched1 <- matched_dfs[[1]]
      dataframeValues$matched2 <- matched_dfs[[2]]
      dataframeValues$rownum <- nrow(matched_dfs[[1]])
      shinyInput <- function(FUN, n, id, ...) {

        # for each of n, create a new input using the FUN function and convert
        # to a character
        vapply(seq_len(n), function(i){
          as.character(FUN(paste0(id, i), ...))
        }, character(1))

      }

      dataframeValues$df <- tibble(
        # parameters here:
        #   * actionButton - type of input to create
        #   * 5 - how many we need
        #   * 'button_' - the ID prefix
        #   * label - label to show on the button
        #   * onclick - what to do when clicked
        Accept = shinyInput(
          FUN = actionButton,
          n = dataframeValues$rownum,
          id = 'button_',
          label = "Accept",
          onclick = 'Shiny.setInputValue(\"accept_button\", this.id, {priority: \"event\"})'
        ),
        Deny = shinyInput(
          FUN = actionButton,
          n = dataframeValues$rownum,
          id = 'button_',
          label = "Deny",
          onclick = 'Shiny.setInputValue(\"deny_button\", this.id, {priority: \"event\"})'
        )
      )
      if (nrow(matched_dfs[[1]]) == 0 && nrow(matched_dfs[[2]]) == 0) {
        output$noMatches <- renderText("No matches found. Please retry using different parameters.")

      } else {
        output$noMatches <- renderText(NULL)
        if ("dummy" %in% colnames(tmpDfa) && "dummy" %in% colnames(tmpDfb)) {
          temp1 <- cbind(matched_dfs[[1]] %>% select(!"gamma.1":last_col(), -"dummy"), dataframeValues$df)
          temp2 <- cbind(temp1, matched_dfs[[2]] %>% select(!"gamma.1":last_col(), -"dummy"))
          colnames(temp12) <- unlist(r$rightSelect)
          temp2 <- cbind(temp1, temp2)

        } else {
          temp1 <- cbind(matched_dfs[[1]] %>% select(!"gamma.1":last_col()), dataframeValues$df)
          temp1_5 <- matched_dfs[[2]] %>% select(!"gamma.1":last_col())
          colnames(temp1_5) <- unlist(r$rightSelect)
          temp2 <- cbind(temp1, temp1_5)
        }
        temp2$ID <- seq_along(temp2[,1])
        dataframeValues$accepted1 <- data.frame(matrix(ncol = ncol(temp2), nrow = 0))
        dataframeValues$accepted2 <- data.frame(matrix(ncol = ncol(temp2), nrow = 0))
        dataframeValues$unnaccepted1 <- data.frame(matrix(ncol = ncol(temp2), nrow = 0))
        dataframeValues$unnaccepted2 <- data.frame(matrix(ncol = ncol(temp2), nrow = 0))
        output$reviewData <- DT::renderDataTable(
          temp2, escape = FALSE
        )
        dataframeValues$bigDF <- temp2
        dataframeValues$unmatchedDataA <- anti_join(mydata1(), matched_dfs[[1]])
        dataframeValues$unmatchedDataB <- anti_join(mydata2(), matched_dfs[[2]])
        allColumnChoices <- colnames(mydata1())
        allColumnChoices <- append(allColumnChoices, colnames(dataframeValues$bigDF), ncol(mydata1()))
        allColumnChoices <- append(allColumnChoices, colnames(mydata2()), length(allColumnChoices))
        output$colSelect <- renderUI({
          div(align="center", selectInput("selectReview","Select Cols You want to View",
                                          choices=allColumnChoices,multiple=TRUE,
                                          selected=colnames(dataframeValues$bigDF)))})
        enable(selector = '.navbar-nav a[data-value="Review"')
        updateTabsetPanel(session, "sNav",
                          selected = "Review")
      }
    }
  })

  observeEvent(input$acceptAll, {
    df1 <- dataframeValues$bigDF %>% select(!"Accept":last_col())
    df2 <- dataframeValues$bigDF %>% select("Deny":last_col(), -"Deny", -"ID")
    dataframeValues$accepted1 <- rbind(dataframeValues$accepted1, df1[, unlist(r$leftSelect)])
    dataframeValues$accepted2 <- rbind(dataframeValues$accepted2, df2[, unlist(r$rightSelect)])
    dataframeValues$bigDF <- dataframeValues$bigDF[0,0]
    output$reviewData <- renderDataTable(dataframeValues$bigDF, escape = FALSE)
    if(nrow(dataframeValues$bigDF) == 0) {
      output$finish <- renderUI({
        div(
          h5("Are you finished reviewing the matches?"),
          actionButton("finishButton", "Yes")
        )
      })
    }
  })

  observeEvent(input$accept_button, {
    selectedRow <- as.numeric(strsplit(input$accept_button, "_")[[1]][2])
    #Add stuff to datasets
    #This might cause some heavy overhead... might be a better way to do this
    df1 <- subset(dataframeValues$bigDF, ID == selectedRow) %>% select(!"Accept":last_col())
    df2 <- dataframeValues$bigDF[dataframeValues$bigDF$ID == selectedRow,] %>% select("Deny":last_col(), -"Deny", -"ID")
    dataframeValues$accepted1 <- rbind(dataframeValues$accepted1, df1[, unlist(r$leftSelect)])
    dataframeValues$accepted2 <- rbind(dataframeValues$accepted2, df2[, unlist(r$rightSelect)])
    dataframeValues$bigDF <- subset(dataframeValues$bigDF, ID!=selectedRow)
    temp1 <- dataframeValues$bigDF[,dataframeValues$nologic1]
    temp3 <- dataframeValues$bigDF %>% select("Accept":"Deny")
    temp4 <- dataframeValues$bigDF %>% select("ID")
    temp2 <- dataframeValues$bigDF %>% select("Deny":last_col(), -"Deny", -"ID")
    colnames(temp2) <- dataframeValues$nologic2
    temp1 <- cbind(temp1, temp3)
    temp1 <- cbind(temp1, temp2)
    temp1 <- cbind(temp1, temp4)
    dataframeValues$bigDF <- temp1
    output$reviewData <- renderDataTable(dataframeValues$bigDF, escape = FALSE)
    if(nrow(dataframeValues$bigDF) == 0) {
      output$finish <- renderUI({
        div(
          h5("Are you finished reviewing the matches?"),
          actionButton("finishButton", "Yes")
        )
      })
    }
  })
  # Add a title to display this stuff
  observeEvent(input$deny_button, {
    selectedRow <- as.numeric(strsplit(input$deny_button, "_")[[1]][2])
    #Add stuff to datasets
    #This might cause some heavy overhead... might be a better way to do this
    df1 <- subset(dataframeValues$bigDF, ID == selectedRow) %>% select(!"Accept":last_col())
    df2 <- dataframeValues$bigDF[dataframeValues$bigDF$ID == selectedRow,] %>% select("Deny":last_col(), -"Deny", -"ID")
    dataframeValues$unaccepted1 <- rbind(dataframeValues$unaccepted1, df1[, unlist(r$leftSelect)])
    dataframeValues$unaccepted2 <- rbind(dataframeValues$unaccepted2, df2[, unlist(r$rightSelect)])
    dataframeValues$bigDF <- subset(dataframeValues$bigDF, ID!=selectedRow)
    temp1 <- dataframeValues$bigDF[,dataframeValues$nologic1]
    temp3 <- dataframeValues$bigDF %>% select("Accept":"Deny")
    temp4 <- dataframeValues$bigDF %>% select("ID")
    temp2 <- dataframeValues$bigDF %>% select("Deny":last_col(), -"Deny", -"ID")
    colnames(temp2) <- dataframeValues$nologic2
    temp1 <- cbind(temp1, temp3)
    temp1 <- cbind(temp1, temp2)
    temp1 <- cbind(temp1, temp4)
    dataframeValues$bigDF <- temp1
    output$reviewData <- renderDataTable(dataframeValues$bigDF, escape = FALSE)
    if(nrow(dataframeValues$bigDF) == 0) {
      output$finish <- renderUI({
        div(
          h5("Are you finished reviewing the matches?"),
          actionButton("finishButton", "Yes")
        )
      })
    }
  })

  observeEvent(input$selectReview, {
    if (("Deny" %in% input$selectReview && "Accept" %in% input$selectReview && unlist(r$rightSelect) %in% input$selectReview && unlist(r$leftSelect) %in% input$selectReview && "ID" %in% input$selectReview)) {
      tmp1 <- mydata1()
      tmp2 <- mydata2()
      nologic <- input$selectReview[!input$selectReview %in% c('Accept', 'Deny', 'ID')]
      nologic3 <- input$selectReview[input$selectReview %in% c('ID')]
      dataframeValues$nologic1 <- intersect(nologic, colnames(mydata1()))
      dataframeValues$nologic2 <- intersect(nologic, colnames(mydata2()))
      tmp1_5 <-(dataframeValues$bigDF %>% select(!"Accept":last_col()))
      tmp2_5 <- (dataframeValues$bigDF %>% select("Deny":last_col(), -"ID", -"Deny"))
      temp1 <- subset(tmp1[, dataframeValues$nologic1],tmp1[,dataframeValues$nologic1[1]] %in% tmp1_5[, dataframeValues$nologic1[1]])
      temp2 <- subset(tmp2[, dataframeValues$nologic2], tmp2[,dataframeValues$nologic2[1]] %in% tmp2_5[, dataframeValues$nologic2[1]])
      nologic4 <- intersect(dataframeValues$nologic1, colnames(tmp1_5))
      nologic5 <- intersect(dataframeValues$nologic2, colnames(tmp1_5))
      temp3 <- transform(merge(temp1,tmp1_5[, nologic4],by=0,how="left",all=TRUE))
      temp4 <- merge(temp2, tmp2_5[, nologic5], how="right", all=TRUE)
      for (i in row.names(temp3)) {
        if (str_detect(i, ".1")) {
          index <- strsplit(i, ".", fixed=TRUE)
          temp3[i, ] <- temp1[index[[1]],]
        }
      }
      temp3 <- select(temp3, !matches(".y"))
      colnames(temp3) <- dataframeValues$nologic1
      temp1 <- cbind(temp3, dataframeValues$bigDF[, c('Accept', 'Deny')])
      temp1 <- cbind(temp1, temp4)
      temp5 <- dataframeValues$bigDF %>% select("ID")
      temp1 <- cbind(temp1, temp5)
      dataframeValues$bigDF <- temp1
      output$reviewData <- renderDataTable(dataframeValues$bigDF, escape = FALSE)
    } else {
      output$colSelectError<- renderText({validate(
        need("Deny" %in% input$selectReview && "Accept" %in% input$selectReview && unlist(r$rightSelect) %in% input$selectReview && unlist(r$leftSelect) %in% input$selectReview && "ID" %in% input$selectReview, "Please do not deselect the original choices.")
      )})
    }
  })
  observeEvent(input$finishButton, {
    tmp1 <- mydata1()
    tmp2 <- mydata2()


    colnames(dataframeValues$accepted2) <- unlist(r$rightSelect)
    dataframeValues$accepted1 <- subset(tmp1, tmp1[,unlist(r$leftSelect)[1]] %in% dataframeValues$accepted1[, unlist(r$leftSelect)[1]])
    dataframeValues$accepted2 <- subset(tmp2, tmp2[,unlist(r$rightSelect)[1]] %in% dataframeValues$accepted2[, unlist(r$rightSelect)[1]])
    dataframeValues$unaccepted1 <- subset(tmp1, tmp1[,unlist(r$leftSelect)[1]] %in% dataframeValues$unaccepted1[, unlist(r$leftSelect)[1]])
    dataframeValues$unaccepted2 <- subset(tmp2, tmp2[,unlist(r$rightSelect)[1]] %in% dataframeValues$unaccepted2[, unlist(r$rightSelect)[1]])
    if (!is.null(dataframeValues$unaccepted1)) {
      View(dataframeValues$unmatchedDataA)
      View(dataframeValues$unaccepted1)
      dataframeValues$unmatchedDataA <- rbind(dataframeValues$unmatchedDataA, dataframeValues$unaccepted1)
    }
    if (!is.null(dataframeValues$unaccepted2)) {
      colnames(dataframeValues$unaccepted2) <- unlist(r$rightSelect)
      dataframeValues$unmatchedDataB <- rbind(dataframeValues$unmatchedDataB, setNames(dataframeValues$unaccepted2, names(dataframeValues$unmatchedDataB)))
    }
    output$matchedTable1 <- renderDataTable(dataframeValues$accepted1)
    output$matchedTable2 <- renderDataTable(dataframeValues$accepted2)
    output$unmatched1 <- renderDataTable(dataframeValues$unmatchedDataA)
    output$unmatched2 <- renderDataTable(dataframeValues$unmatchedDataB)
    output$unmatchedA <- downloadHandler(
      filename = function() {
        paste('data-', 'unmatchedA', '.csv', sep='')
      },
      content = function(con) {
        write.csv(dataframeValues$unmatchedDataA, con)
      }
    )
    output$unmatchedB <- downloadHandler(
      filename = function() {
        paste('data-', 'unmatchedB', '.csv', sep='')
      },
      content = function(con) {
        write.csv(dataframeValues$unmatchedDataA, con)
      }
    )
    output$matchedA <- downloadHandler(
      filename = function() {
        paste('data-', 'matchedA', '.csv', sep='')
      },
      content = function(con) {
        write.csv(dataframeValues$accepted1, con)
      }
    )
    output$matchedB <- downloadHandler(
      filename = function() {
        paste('data-', 'matchedB', '.csv', sep='')
      },
      content = function(con) {
        write.csv(dataframeValues$accepted2, con)
      }
    )
    enable(selector = '.navbar-nav a[data-value="Results"')
    updateTabsetPanel(session, "sNav",
                      selected = "Results")
  })

  observeEvent(input$showCols, {
    temp1 <- mydata1()
    temp2 <- mydata2()
    namesIn1 <- intersect(colnames(mydata1()), input$showCols)
    namesIn2 <- intersect(colnames(mydata2()), input$showCols)
    output$dataset1 <- DT::renderDataTable(temp1[, namesIn1])
    output$dataset2 <- DT::renderDataTable(temp2[, namesIn2])
    output$matchedTable1 <- renderDataTable(dataframeValues$accepted1[, namesIn1])
    output$matchedTable2 <- renderDataTable(dataframeValues$accepted2[, namesIn2])
    output$unmatched1 <- renderDataTable(dataframeValues$unmatchedDataA[, namesIn1])
    output$unmatched2 <- renderDataTable(dataframeValues$unmatchedDataB[, namesIn2])
  })


}
