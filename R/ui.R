
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


tags$head(
  tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    ")))
# Define UI for application that draws a histogram
ui <- navbarPage(div(img(src="sophronLOGO.png", height = "30px", width="136px"), "Sophron Record Linkage"), id = "sNav",
                 theme = shinytheme("cerulean"),
                 tabPanel("Main",
                          useShinyjs(),
                          uiOutput("infoHelp"),
                          fluidRow(
                            column(3, h2("File Input"))
                          ),
                          fluidRow(
                            column(6,
                                   br(),
                                   div(fileInput("file1", h3("Please input the first dataset."), width = '100%'))
                            ),
                            column(6,
                                   br(),
                                   div(fileInput("file2", h3("Please input the second dataset."), width = '100%'))
                            )
                          ),
                          div(textOutput("txt")),
                          fixedRow(width=12,
                                   column(6,div(class="headerLeft"),
                                          div(uiOutput("nHeaderIncrease"))
                                   ),
                                   column(6,div(class="headerRight"),
                                          div(uiOutput("nHeaderAccept")),
                                          div(textOutput("acceptError"))
                                   )
                          ),
                          br(),
                          fixedRow(width=12,
                                   column(2,
                                          h5("Headers for both datasets:"),
                                          checkboxGroupInput("show_vars", label=NULL, choices=NULL),
                                          uiOutput("checkboxCSS")
                                   ),
                                   column(4,
                                          h5("Variable Types:"),
                                          uiOutput("dropdown2"),
                                          uiOutput("selectCSS")

                                   ),
                                   column(2,
                                          numericInput("threshold", "Please enter a threshold between 0 and 1", value = 0, min = 0, max = 1),
                                          br(),
                                          radioButtons("dedupe", "Would you like to deduplicate the matches?", choices = c("Yes", "No"), selected="No")
                                   ),
                                   column(2,
                                          sliderInput("cutA", "Please enter a cut.a value between 0 and 1", value = 0.94, min = 0, max = 1)
                                   ),
                                   column(2,
                                          sliderInput("cutP", "Please enter a cut.p value between 0 and 1", value = 0.88, min = 0, max = 1)
                                   ),
                          ),
                          mainPanel(
                            div(h2("Link")),
                            div(actionButton("link","Link")),
                            div(textOutput("txt2")),
                            div(textOutput("noMatches")),


                          ),
                 ),
                 tabPanel("Review",
                          div(h2("Review Matches")),
                          div(
                            h5("Would you like to accept all matches?"),
                            actionButton("acceptAll", "Accept")
                          ),
                          div(uiOutput("colSelect")),
                          div(textOutput("colSelectError")),
                          br(),
                          div(DT::dataTableOutput("reviewData")),
                          div(uiOutput("finish")),

                 ),
                 tabPanel("Results",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("showCols","Select columns to view:", choices = NULL),
                              div(h2("Download Data")),
                              downloadButton("matchedA", "Matches Dataset 1"),
                              downloadButton("matchedB", "Matches Dataset 2"),
                              downloadButton("unmatchedA", "Unmatched Dataset 1"),
                              downloadButton("unmatchedB", "Unmatched Dataset 2")
                            ),
                            mainPanel(
                              div(h2("Inputted Datasets")),
                              tabsetPanel(
                                id = 'dataset',
                                tabPanel("Dataset 1 ", DT::dataTableOutput("dataset1")),
                                tabPanel("Dataset 2", DT::dataTableOutput("dataset2")),
                                tabPanel("Matches Dataset1", DT::dataTableOutput("matchedTable1")),
                                tabPanel("Matches Dataset2", DT::dataTableOutput("matchedTable2")),
                                tabPanel("Unmatched 1", DT::dataTableOutput("unmatched1")),
                                tabPanel("Unmatched 2", DT::dataTableOutput("unmatched2"))
                              ),
                            ),
                            position = c("left", "right"),
                            fluid = TRUE
                          )
                 ),
                 header = div(
                   tags$script(
                     HTML("var header = $('.navbar > .container-fluid');
                                  header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"info\" type=\"button\" class=\"btn btn-primary action-button\">i</button></div>')")
                   ))
)
