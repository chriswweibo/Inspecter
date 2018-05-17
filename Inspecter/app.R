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
library(RJSONIO)
library(rlist)
library(readxl)
options(shiny.maxRequestSize=100*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Genowis数据结构化校验平台"),
   fluidRow(
   column(fileInput("file","上传50例原始文本表格(必须包含“病理号”列)"),width=3),
   column(h4("步骤：1上传xlsx和json文件；2，搜索病理号，在右侧json区修改结果；3 完毕后点击“text”视图，复制结果至左下文本框；4 点击“开始计算”，看结果"),width=6),
   column(fileInput("json","上传ANSI编码的json文件（可以是全量json，后台会根据病理号自动筛选）"),width=3)),
   fluidRow(
   column(dataTableOutput("raw"),width = 9),
   column(jsoneditOutput("json_result",height = "700px"),width = 3)),
   fluidRow(
   column(textAreaInput("json_final","输入最终的json文件",width="1400px"),width = 9),
   #column(submitButton("开始计算",icon=icon("refresh"),width="120%"),width=1),
   column(tableOutput("results"),width=3))
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  rawFile=reactive({
    rawPath=input$file$datapath
    read_xlsx(rawPath)
  })
  jsonFile=reactive({
    jsonPath=input$json$datapath
    PID=rawFile()$病理号
    #PID=c("510129","510036")
    jsonData=RJSONIO::fromJSON(jsonPath)
    list.filter(jsonData,base_info$病理号 %in% PID)
  })
  
  output$raw=renderDataTable({
    datatable(rawFile(), extensions = list('Buttons',"Scroller"),
              options = list(pageLength = 50,dom = 'Bfrt',buttons = I('colvis'),scrollY = 600,scroller = TRUE))
  })
  output$json_result=renderJsonedit({
    jsonedit(jsonFile(),height=600,modes=c("text","tree"))
  })
  
  
  json_Inspect=function(x,y){
    # x,y are the json files with same length (observation lines), y is the final result, or standard set
    finalCell=sum(!is.na(as.matrix(y)))
    originCell=sum(!is.na(as.matrix(x)))
    完整率=round(originCell/finalCell,digits = 4)
    
    commonCols=intersect(colnames(y),colnames(x))
    final_common=y[,commonCols] %>%  as.matrix()
    final_common[is.na(final_common)]="NA"
    #final_common=final_common[order(final_common$病理号),]
    origin_common=x[,commonCols] %>%  as.matrix()
    origin_common[is.na(origin_common)]="NA"
    #origin_common=origin_common[order(origin_common$病理号),]
    tmp=(final_common==origin_common)
    equal_num=sum(tmp)
    准确率=round(equal_num/(nrow(final_common)*length(commonCols)),digits = 4)
    
    return(list(完整率=as.character(完整率),准确率=as.character(准确率)))
  }
  
  
  output$results=renderTable({
    finalResult=input$json_final
    json_final=jsonlite::fromJSON(finalResult,flatten = T)
    json_origin=jsonlite::fromJSON(toJSON(jsonFile()),flatten = T)
    result=json_Inspect(json_origin,json_final)
    paste("完整率:",result$完整率,"\t 准确率:",result$准确率,sep="")
    data.frame(result)
    
  })
  

 
}

# Run the application 
shinyApp(ui = ui, server = server,options=list(port = 0331,host = "0.0.0.0",launch.browser=F, quiet=T))

