#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(listviewer)
library(DT)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
   
   # Application title
   titlePanel("数据结构化校验平台"),
   
   column(fileInput("file","上传50例原始文本表格"),width=6),
   column(fileInput("json","上传json文件"),width=6),
   column(dataTableOutput("raw"),width = 9),
   column(jsoneditOutput("json_result"),width = 3)
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$raw=renderDataTable({
    data("mtcars")
    mtcars
    datatable(mtcars, extensions = 'Buttons',options = list(pageLength = 50,dom = 'Bfrtip',buttons = I('colvis')))
  })
  output$json_result=renderJsonedit({
    jsonedit(mtcars)
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

