#===================================================================================
#
# Program Name: CLT.r
# Programmer  : Arteid Memaj
# Date        : 
# Purpose     : Visualize Central Limit Theorem 
#
#         Modification History
#   Date         Version         Programmer           Description
#   -------      ---------       ----------           ------------
#
#===================================================================================


library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)

u <- shinyUI(fluidPage(
  titlePanel("Central Limit Theorem"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("donum1", "Frequency",             value = T),
                             checkboxInput("donum2", "Mean of those numbers", value = F),
                             sliderInput("ppl","How many people?",                           min=5,max=200,value=100),
                             sliderInput("rolls","How many times will each roll?",           min=5,max=200,value=5)
                ),
                mainPanel("main panel",
                          column(4,plotOutput(outputId="plotgraph", width="500px",height="400px"))
                ))))

s <- shinyServer(function(input, output) 
{
  set.seed(123)
  
  #define RollDie
  RollDie = function(n) sample(1:6,n,replace=T)
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    #create empty DF
    cum.df <- data.frame(peeps=numeric(),nums=numeric())
    for (i in 1:input$rolls) {
      peeps   <- c(1:input$ppl)
      nums    <- RollDie(input$ppl)
      comb.df <- data.frame(peeps,nums)
      cum.df  <- rbind(cum.df,comb.df)
    }
    qplot(cum.df$nums,fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    cum.df <- data.frame(peeps=numeric(),nums=numeric())
    for (i in 1:input$rolls) {
      peeps   <- c(1:input$ppl)
      nums    <- RollDie(input$ppl)
      comb.df <- data.frame(peeps,nums)
      cum.df  <- rbind(cum.df,comb.df)
    }
    #get mean per person. round each means to 1 decimal
    by_peeps      <- group_by(cum.df,peeps)
    mean.by.peeps <-summarise(by_peeps, mean.peep = mean(nums))
    qplot(mean.by.peeps$mean.peep,fill=I("blue"),binwidth=0.05,main="plotgraph2")
  })
  
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2())
    wtlist <- c(input$ppl,input$rolls)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
})
shinyApp(u,s)