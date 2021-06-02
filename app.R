library(tidyverse)
library(data.table)


boxoffice_new = data.table::fread("data/boxoffice_new.csv")
rmro = data.table::fread("data/rmro.csv")

mytheme = theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(face = "bold")
  )



ui <- fluidPage(
  titlePanel(strong("코로나19 이후 국내 박스오피스 실태")),
  
  ### Part 1 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 1. 개별 단위 통계")),
      selectInput("category1_1", "단위", choices = c("월별", "연도별")),
      selectInput("category1_2", "지표", choices = c("영화 1편 당 평균 하루 매출",
                                                     "영화 1편 당 평균 하루 관객 수",
                                                     "영화 1편 당 평균 하루 상영 횟수",
                                                     "상영 1회 당 평균 매출",
                                                     "상영 1회 당 평균 관객 수",
                                                     "상영관 1관 당 평균 상영 횟수")
      )
    ),
    
    mainPanel(
      plotOutput("plot1")
    )
    
  ),
  
  hr(),
  
  ### Part 2 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 2. 기록형 통계 (총, 최대)")),
      selectInput("category2", "지표 (영화 1편 당)", choices = c("평균 최대 하루 매출",
                                                                 "평균 총 매출",
                                                                 "평균 최대 하루 관객 수",
                                                                 "평균 총 관객 수")
      ),
      strong("재개봉 영화 제외 데이터 기반"),
      p("박스오피스 데이터 내 최초 상영일이 개봉일로부터 365일보다 지난 시점일 경우 재개봉 영화로 간주"),
    ),
    
    mainPanel(
      plotOutput("plot2")
    )
    
  ),
  
  hr(),
  
  ### Part 3 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 3. 영화 카테고리별 통계")),
      selectInput("category3_1", "분류 기준", choices = c("국내 및 해외 영화", "국가별 영화", "장르별 영화", "재개봉 및 초개봉 영화")),
      selectInput("category3_2", "지표", choices = c("상영 횟수", "상영 점유율", "수", "점유율"))
    ),
    
    mainPanel(
      plotOutput("plot3")
    )
    
  ),
)


server <- function(input, output){
  ### Part 1 ###
  data1 <- reactive({
    if (input$category1_1 == "월별"){
      if (input$category1_2 == "영화 1편 당 평균 하루 매출"){
        boxoffice_new[, mean(salesAmt), .(year, month)]
      }else if (input$category1_2 == "영화 1편 당 평균 하루 관객 수"){
        boxoffice_new[, mean(audiCnt), .(year, month)]
      }else if (input$category1_2 == "영화 1편 당 평균 하루 상영 횟수"){
        boxoffice_new[, mean(showCnt), .(year, month)]
      }else if (input$category1_2 == "상영 1회 당 평균 매출"){
        boxoffice_new[, mean(sales_per_show), .(year, month)]
      }else if (input$category1_2 == "상영 1회 당 평균 관객 수"){
        boxoffice_new[, mean(audi_per_show), .(year, month)]
      }else{
        boxoffice_new[, mean(show_per_scrn), .(year, month)]
      }
    }else{
      if (input$category1_2 == "영화 1편 당 평균 하루 매출"){
        boxoffice_new[, mean(salesAmt), year]
      }else if (input$category1_2 == "영화 1편 당 평균 하루 관객 수"){
        boxoffice_new[, mean(audiCnt), year] 
      }else if (input$category1_2 == "영화 1편 당 평균 하루 상영 횟수"){
        boxoffice_new[, mean(showCnt), year]
      }else if (input$category1_2 == "상영 1회 당 평균 매출"){
        boxoffice_new[, mean(sales_per_show), year]
      }else if (input$category1_2 == "상영 1회 당 평균 관객 수"){
        boxoffice_new[, mean(audi_per_show), year]
      }else{
        boxoffice_new[, mean(show_per_scrn), year]
      }
    }
  })
  
  ylab1 <- reactive({
    if (str_detect(input$category1_2, "매출")){
      "매출 (원)"
    }else if (str_detect(input$category1_2, "관객 수")){
      "관객 수 (명)"
    }else{
      "상영 횟수 (편)"
    }
  })
  
  plotting1 <- reactive({
    if (input$category1_1 == "월별"){
      ggplot(data1(), aes(x = factor(month), y = V1, fill = factor(year))) +
        geom_col(position = "dodge") +
        xlab("월") +
        ylab(ylab1())
    }else{
      ggplot(data1(), aes(x = factor(year), y = V1, fill = factor(year))) +
        geom_col() +
        xlab("연도") +
        ylab(ylab1())
    }
  })
  
  output$plot1 <- renderPlot({
    plotting1() +
      ggtitle(input$category1_2) +
      labs(fill = "연도") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 2 ###
  data2 <- reactive({
    if (input$category2 == "평균 최대 하루 매출"){
      rmro[, .N, .(year, salesPeak, movieCd)][, mean(salesPeak), year]
    }else if (input$category2 == "평균 총 매출"){
      dt = rmro[, .N, .(year, salesTotal, movieCd)][, mean(salesTotal), year]
      dt[, V1 := V1 %>% as.numeric]
      return(dt)
    }else if (input$category2 == "평균 최대 하루 관객 수"){
      rmro[, .N, .(year, audiPeak, movieCd)][, mean(audiPeak), year] 
    }else{
      rmro[, .N, .(year, audiTotal, movieCd)][, mean(audiTotal), year]
    }
  })
  
  ylab2 <- reactive({
    if (input$category2 == "평균 최대 하루 매출"){
      "최대 하루 매출 (원)"
    }else if (input$category2 == "평균 총 매출"){
      "총 매출 (원)"
    }else if (input$category2 == "평균 최대 하루 관객 수"){
      "최대 하루 관객 수 (명)"
    }else{
      "총 관객 수 (명)"
    }
  })
  
  output$plot2 <- renderPlot({
    ggplot(data2(), aes(x = factor(year), y = V1, fill = factor(year))) +
      geom_col() +
      ggtitle(str_c("영화 1편 당 ", input$category2)) +
      xlab("연도") +
      ylab(ylab2()) +
      labs(fill = "연도") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 3 ###
  var3 <- reactive({
    if (input$category3_1 == "국내 및 해외 영화"){
      "dom_or_for"
    }else if (input$category3_1 == "국가별 영화"){
      "nation"
    }else if (input$category3_1 == "장르별 영화"){
      "genre"
    }else{
      "reopen"
    }
  })
  
  data3 <- reactive({
    if (input$category3_1 == "국내 및 해외 영화"){
      if (input$category3_2 == "상영 횟수"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
      }else if (input$category3_2 == "상영 점유율"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "수"){
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
      }else{
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "국가별 영화"){
      if (input$category3_2 == "상영 횟수"){
        dt = boxoffice_new[, .N, .(year, nation)]
      }else if (input$category3_2 == "상영 점유율"){
        dt = boxoffice_new[, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "수"){
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
      }else{
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "장르별 영화"){
      if (input$category3_2 == "상영 횟수"){
        dt = boxoffice_new[, .N, .(year, genre)]
      }else if (input$category3_2 == "상영 점유율"){
        dt = boxoffice_new[, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "수"){
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
      }else{
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }
    }else{
      if (input$category3_2 == "상영 횟수"){
        dt = boxoffice_new[, .N, .(year, reopen)]
      }else if (input$category3_2 == "상영 점유율"){
        dt = boxoffice_new[, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "수"){
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
      }else{
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }
    }
    setnames(dt, var3(), "V1")
    return(dt)
  })
  
  output$plot3 <- renderPlot({
    if (str_detect(input$category3_2, "점유율")){
      p = ggplot(data3(), aes(x = factor(year), y = N, fill = V1)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent)
      xlab = "연도"
      ylab = "점유율"
      if (str_detect(input$category3_1, "별")){
        labs = str_replace(input$category3_1, "별 영화", "")
      }else{
        labs = str_replace(input$category3_1, " 영화", "")
      }
    }else{
      p = ggplot(data3(), aes(x = V1, y = N, fill = factor(year))) +
        geom_col(position = "dodge")
      if (str_detect(input$category3_1, "별")){
        xlab = str_replace(input$category3_1, "별 영화", "")
      }else{
        xlab = str_replace(input$category3_1, " 영화", "")
      }
      ylab = "(편)"
      labs = "연도"
    }
    p + 
      ggtitle(str_c(input$category3_1, " ", input$category3_2)) +
      xlab(xlab) +
      ylab(ylab) +
      labs(fill = labs) +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
}


shinyApp(ui = ui, server = server)