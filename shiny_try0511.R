library(shiny)
library(tidyverse)
library(nCov2019)
library(shinythemes)

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rgeos)
message("开始下载数据")
history_data_cn <- load_nCov2019(lang = 'en')
global_hsitory_data <- history_data_cn$global

# history_data <- load_nCov2019(lang = 'zh')
china_history_data <- history_data_cn$data


load("ProvinceMapDatas.Rda")

df_China <- df_China %>% mutate(en_name = 
                      case_when(NAME == "黑龙江" ~ "Heilongjiang",
                                NAME == "内蒙古" ~ "Inner Mongolia",
                                NAME == "新疆" ~ "Xinjiang",
                                NAME == "吉林" ~ "Jilin",
                                NAME == "辽宁" ~ "Liaoning",
                                NAME == "甘肃" ~ "Guizhou",
                                NAME == "河北" ~ "Hebei",
                                NAME == "北京" ~ "Beijing",
                                NAME == "山西" ~ "Shanxi",
                                NAME == "天津" ~ "Tianjin",
                                NAME == "陕西" ~ "Shaanxi",
                                NAME == "宁夏" ~ "Ningxia",
                                NAME == "青海" ~ "Qinghai",
                                NAME == "山东" ~ "Shandong",
                                NAME == "西藏" ~ "Tibet",
                                NAME == "河南" ~ "Henan",
                                NAME == "江苏" ~ "Jiangsu",
                                NAME == "安徽" ~ "Anhui",
                                NAME == "四川" ~ "Sichuan",
                                NAME == "湖北" ~ "Hubei",
                                NAME == "重庆" ~ "Chongqing",
                                NAME == "上海" ~ "Shanghai",
                                NAME == "浙江" ~ "Zhejiang",
                                NAME == "湖南" ~ "Hunan",
                                NAME == "江西" ~ "Jiangxi",
                                NAME == "云南" ~ "Yunnan",
                                NAME == "贵州" ~ "Guizhou",
                                NAME == "福建" ~ "Fujian",
                                NAME == "广西" ~ "Guangxi",
                                NAME == "台湾" ~ "Taiwan",
                                NAME == "广东" ~ "Guangdong",
                                NAME == "香港" ~ "Hong Kong",
                                NAME == "海南" ~ "Hainan"))

message("数据下载完毕")
world = ne_countries(scale = "medium", returnclass = "sf")

# 
# library(thematic)
# thematic_on(
#   bg = "#222222", fg = "white", accent = "#0CE3AC",
#   font = font_spec("Oxanium", scale = 1.25)
# )


ui <- fluidPage(
  
  shinythemes::themeSelector(),
  #plot 1
  h1("choose a date and a city to show the data by table and bar plot"),
  sidebarLayout(
    sidebarPanel(dateInput(inputId = "p1date",
                        label = "choose time",
                        min = as.Date("2019-01-21"),
                        max = as.Date("2020-05-01")),
    selectInput(inputId = "province",
                          label = "choose a province",
                          choices = china_history_data$province %>% unique())),
    mainPanel(tableOutput("table1"),plotOutput("plot1", height = "800px"))
    
  ),
  h1("choose a city to show the history data by scatter-line plot"),
  # plot 2 中国城市每日折线图
  
  fluidRow(
    column(6, selectInput(inputId = "city",
                          label = "choose a city",
                          choices = china_history_data$city %>% unique())),
    column(6, plotOutput("plot2", height = "800px"))
  ),
  
  # plot3 各个省之间的增长
  h1("show the different in province"),
  sidebarLayout(
    sidebarPanel(checkboxGroupInput(inputId = "province_3",
                          label = "choose a province",
                          choices = china_history_data$province %>% unique())),
    mainPanel(plotOutput("plot3", height = "800px"))
  ),
  
  # 画某一天的地图
  h1("choose a date and map data to china map"),
  fluidRow(
    column(6, dateInput(inputId = "p4date",
                        label = "choose time",
                        min = as.Date("2019-01-21"),
                        max = as.Date("2020-05-01"))),
    column(6, plotOutput("plot4", height = "800px"))
  ),
  h1("choose a date then show the top 20"),
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sidebarLayout(
    sidebarPanel(dateInput(inputId = "p5date",
                        label = "choose time",
                        min = as.Date("2019-01-21"),
                        max = as.Date("2020-05-01"))),
    mainPanel(plotOutput("plot5", height = "800px"))
  ),
  h1("choose a country and see the data by scatter plot"),
  fluidRow(
    column(6, selectInput(inputId = "country_6",
                          label = "choose a country",
                          choices = global_hsitory_data$country %>% unique())),
    column(6, plotOutput("plot6", height = "800px"))
  ),
  h1("plot global map by choose a date"),
  sidebarLayout(
    sidebarPanel(dateInput(inputId = "p7date",
                        label = "choose time",
                        min = as.Date("2019-01-21"),
                        max = as.Date("2020-05-01"))),
    mainPanel(plotOutput("plot7", height = "800px"))
  )
  
  
)




server <- function(input, output, session) {
  # plot1
  output$table1 <- renderTable({
    req(input$p1date)
    req(input$province)
    china_history_data %>% 
      filter(province == input$province) %>%
      filter(time == input$p1date) %>% 
      mutate(city = reorder(city, cum_confirm))
  })
  
  output$plot1 <- renderPlot({
    req(input$p1date, input$province)
    # req(input$province)
    china_history_data %>% 
      filter(province == input$province) %>%
      filter(time == input$p1date) %>% 
      mutate(city = reorder(city, cum_confirm)) %>% 
      ggplot(aes(x = city, y = cum_confirm)) + 
      geom_col(aes(fill = cum_confirm), show.legend = FALSE) + 
      geom_label(aes(label = cum_confirm)) + 
      theme_bw() + scale_y_log10() + scale_fill_viridis_c() + 
      labs(fill = 'cum_confirm', x = 'city', y = 'cum confirm',
           title = glue::glue("city: {input$province} data")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
      
  })
  # plot2
  output$plot2 <- renderPlot({
    req(input$city)
    china_history_data %>% filter(city %in% input$city) %>% 
      ggplot(aes(x = time, y = cum_confirm, color = city)) + 
      geom_point() +geom_line() + theme_bw() + 
      labs(title = glue::glue("history data of {input$city}")) +
      theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(~ city, scales = 'free')
    
  })
  # plot 3
  output$plot3 <- renderPlot({
    req(input$province_3)
    china_history_data %>% filter(province %in% input$province_3) %>% 
      group_by(province, time) %>% 
      summarise(day_count = sum(cum_confirm)) %>% 
      mutate(diff_count = c(NA, diff(day_count))) %>% 
      ggplot(aes(x = time, y = diff_count, color = province)) + theme_bw() + 
      geom_point() + geom_line() + labs(y = 'increase') + 
      # labs(title = glue::glue("daily increase of {input$province_3}")) +
      theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ province, scales = 'free')
  })
  # plot 4
  output$plot4 <- renderPlot({
    req(input$p4date)
    
    
    # 离散型显示某一天的数据
    plot_china_data_d <- df_China %>% 
      left_join(china_history_data %>% filter(time == input$p4date),
                by = c("en_name" = "province")) %>% 
      mutate(c_sum_confirm = 
               cut(cum_confirm, 
                   breaks = c(0, 10, 100, 1000, 3000, 5000, 10000, 30000, 40000, 50000, 60000, 70000, Inf),
                   labels = c('<10', '10-100','100-1000', '1k-3k','3k-5k', '5k-1w','1w-3w', '3w-4w', '4w-5w', '5w-6w', '6w-7w', '7w>'))) 
    ggplot() +
      geom_polygon(data = plot_china_data_d,
                   aes(x=long, y=lat, group=interaction(class, group), 
                       fill=c_sum_confirm), colour="black", size=0.25) + # 中国地图, 包括中国主体部分和长方形方块内的南海诸岛数据
      geom_rect(aes(xmin=124, xmax=124+9.4, ymin=16-0.3, ymax=16+9), 
                fill=NA, colour="black", size=0.25) +# 绘制长方形方框
      geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), 
                colour="black", size=1) +  # 绘制长方形方框内的中国南海八段线 
      scale_fill_brewer(palette="Reds") +
      coord_map() +  
      ylim(14, 54) +
      labs(x="lat", y="lon", fill = 'breaks') +
      ggtitle(glue::glue("time : {input$p4date}")) + theme_bw() +
      theme(
        plot.title=element_text(hjust=0.5),
        # legend.position = c(0.1,0.2),
        legend.background = element_blank())+
      guides(fill=guide_legend(reverse=TRUE))   # 反转图例顺序
  })
  
  # plot 5
  output$plot5 <- renderPlot({
    req(input$p5date)
    global_hsitory_data %>% filter(time == input$p5date) %>% 
      arrange(desc(cum_confirm)) %>% slice(1:20) %>% 
      mutate(country = reorder(country, cum_confirm)) %>% 
      ggplot(aes(x = country, y = cum_confirm)) + geom_col() + 
      geom_label(aes(label = cum_confirm)) + theme_bw() + 
      labs(title = glue::glue("top 20 of {input$p5date} ")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)) + 
      scale_y_log10() 
  })
  
  # plot 6
  output$plot6 <- renderPlot({
    req(input$country_6)
    global_hsitory_data %>% filter(country == input$country_6) %>% 
      pivot_longer(cols = c("cum_confirm", "cum_heal", "cum_dead"), names_to = "type") %>% 
      ggplot(aes(x = time, y = value, color = type)) + geom_point() + geom_line() +
      theme_bw() + labs(title = glue::glue("all data of {input$country_6}")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  #plot 7
  output$plot7 <- renderPlot({
    req(input$p7date)
    global_hsitory_data %>% filter(time == input$p7date) %>% 
      arrange(desc(cum_confirm)) %>% slice(1:200) %>% 
      left_join(world, by = c("country" = "subunit")) %>% 
      ggplot(aes(fill = cum_confirm, geometry = geometry), color = NA) + geom_sf()+ 
      scale_fill_viridis_c(option = "plasma", trans = 'log') + 
      labs(title = glue::glue("day: {input$p7date} world map")) + 
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui, server)
