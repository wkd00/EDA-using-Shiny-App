library(shiny)
library(ggplot2)
library(GGally)
library(dplyr)
library(viridis) 
library(rlang)
library(hexbin)
library(DT)

data <- read.csv("student_lifestyle_dataset.csv")

data$Stress_Level <- factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))

# Extra_Hours 추가
data$Extra_Hours_Per_Day <- data$Extracurricular_Hours_Per_Day +
  data$Social_Hours_Per_Day +
  data$Physical_Activity_Hours_Per_Day

# 분위 변수들 추가
data$GPA_Range <- cut(data$GPA,
                      breaks = quantile(data$GPA, probs = seq(0, 1, 0.25), na.rm = TRUE),
                      labels = c("1Q", "2Q", "3Q", "4Q"),
                      include.lowest = TRUE)

data$Sleep_Range <- cut(data$Sleep_Hours_Per_Day,
                        breaks = quantile(data$Sleep_Hours_Per_Day, probs = seq(0, 1, 0.25), na.rm = TRUE),
                        labels = c("1Q", "2Q", "3Q", "4Q"),
                        include.lowest = TRUE)

data$Study_Range <- cut(data$Study_Hours_Per_Day,
                        breaks = quantile(data$Study_Hours_Per_Day, probs = seq(0, 1, 0.25), na.rm = TRUE),
                        labels = c("1Q", "2Q", "3Q", "4Q"),
                        include.lowest = TRUE)

data$Extra_hour_range <- cut(data$Extra_Hours_Per_Day,
                             breaks = quantile(data$Extra_Hours_Per_Day, probs = seq(0, 1, 0.25), na.rm = TRUE),
                             labels = c("1Q", "2Q", "3Q", "4Q"),
                             include.lowest = TRUE)

data$Social_hour_range <- cut(data$Social_Hours_Per_Day,
                              breaks = quantile(data$Social_Hours_Per_Day, probs = seq(0, 1, 0.25), na.rm = TRUE),
                              labels = c("1Q", "2Q", "3Q", "4Q"),
                              include.lowest = TRUE)

data$Physical_Activity_range <- cut(data$Physical_Activity_Hours_Per_Day,
                                    breaks = quantile(data$Physical_Activity_Hours_Per_Day, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                    labels = c("1Q", "2Q", "3Q", "4Q"),
                                    include.lowest = TRUE)

# Extracurricular 분위 변수 추가
data$Extracurricular_range <- cut(data$Extracurricular_Hours_Per_Day,
                                  breaks = quantile(data$Extracurricular_Hours_Per_Day, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                  labels = c("1Q", "2Q", "3Q", "4Q"),
                                  include.lowest = TRUE)

given_quantile_vars <- grep("_range$|_Range$", colnames(data), value = TRUE)
non_quantile_vars <- setdiff(colnames(data), c("Stress_Level", given_quantile_vars, "Student_ID"))

getThemeOverview <- function(theme_name) {
  switch(theme_name,
         "Default" = theme_gray(),
         "Classic" = theme_classic(),
         "Minimal" = theme_minimal(),
         "Dark" = theme_dark())
}

ui <- navbarPage("Student Lifestyle Analysis",
                 
                 # 1페이지
                 tabPanel("Overall Data Overview",
                          fluidPage(
                            titlePanel("Overall Understanding of Student Lifestyle Data"),
                            sidebarLayout(
                              sidebarPanel(
                                conditionalPanel(
                                  condition = "input.tab_1_subpages == 'General Overview'",
                                  h3("General Trend Options"),
                                  selectInput("plot_type", "Select Plot Type:",
                                              choices = c("GPA vs Study Hours", "GPA vs Sleep Hours", "GPA vs Extra Hours",
                                                          "GPA vs Social Hours", "GPA vs Physical Hours")),
                                  selectInput("stress_level", "Select Stress Level:",
                                              choices = c("All", "Low", "Moderate", "High")),
                                  checkboxInput("show_loess", "Show Loess Trend Line", value = FALSE),
                                  selectInput("theme_ov", "Theme:",
                                              choices = c("Default", "Classic", "Minimal", "Dark"),
                                              selected = "Default")
                                ),
                                conditionalPanel(
                                  condition = "input.tab_1_subpages == 'View Data'",
                                  h3("View Data Options"),
                                  selectInput("page_length", "Rows per page:",
                                              choices = c(10, 25, 50, 100), selected = 10)
                                ),
                                width = 3
                              ),
                              mainPanel(
                                tabsetPanel(
                                  id = "tab_1_subpages",
                                  tabPanel("General Overview",
                                           plotOutput("main_plot")
                                  ),
                                  tabPanel("View Data",
                                           DTOutput("raw_data_table")
                                  ),
                                  tabPanel("Overall Summary",
                                           wellPanel(
                                             tableOutput("overall_summary_table")
                                           )
                                  ),
                                  tabPanel("Summary by Stress Level",
                                           wellPanel(
                                             tableOutput("stress_summary_table")
                                           )
                                  ),
                                  tabPanel("Summary by Binary Stress",
                                           wellPanel(
                                             tableOutput("binary_stress_summary_table")
                                           )
                                  )
                                )
                              )
                            )
                          )
                 ),
                 
                 # 2페이지: Logistic Analysis
                 tabPanel("Logistic Analysis",
                          fluidPage(
                            titlePanel("Logistic Analysis (Binary Stress Model)"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("logistic_model_type", "Model Type:",
                                            choices = c("Study Hours Only", "Sleep Hours Only", "Extra Hours Only", 
                                                        "Physical Hours Only", "Social Hours Only", "Extracurricular Hours Only",
                                                        "Weighted (Study+Sleep)"),
                                            selected = "Study Hours Only"),
                                conditionalPanel(
                                  condition = "input.logistic_model_type == 'Weighted (Study+Sleep)'",
                                  sliderInput("study_weight", "Study Weight (w):", 
                                              min = 0, max = 1, value = 0.5, step = 0.1),
                                  p("Weighted Predictor (binary model) = w * Study_Hours_Per_Day + (1 - w) * Sleep_Hours_Per_Day")
                                ),
                                selectInput("theme_la", "Theme:",
                                            choices = c("Default", "Classic", "Minimal", "Dark"),
                                            selected = "Default"),
                                width = 3
                              ),
                              mainPanel(
                                plotOutput("logistic_model_plot"),
                                wellPanel(
                                  h3("Logistic Model Coefficients"),
                                  tableOutput("logistic_model_table")
                                )
                              )
                            )
                          )),
                 
                 # 3페이지: GPA Prediction
                 tabPanel("GPA Prediction",
                          fluidPage(
                            titlePanel("GPA Prediction Model: GPA ~ Sleep + Study"),
                            tags$hr(),
                            fluidRow(
                              column(3,
                                     wellPanel(
                                       h3("Model Options"),
                                       selectInput("gpa_model_type", "Model Type:",
                                                   choices = c("Sleep Only", "Study Only", "Social Only", "Extracurricular Only", "Physical Only", "Extra Only", "Weighted Sleep+Study"),
                                                   selected = "Sleep Only"),
                                       conditionalPanel(
                                         condition = "input.gpa_model_type == 'Weighted Sleep+Study'",
                                         sliderInput("gpa_sleep_weight", "Sleep Weight (for Weighted Model):",
                                                     min = 0, max = 1, value = 0.5, step = 0.1),
                                         p("Weighted Predictor = w * Sleep_Hours_Per_Day + (1 - w) * Study_Hours_Per_Day")
                                       ),
                                       selectInput("theme_gpa", "Theme:",
                                                   choices = c("Default", "Classic", "Minimal", "Dark"),
                                                   selected = "Default")
                                     )
                              ),
                              column(9,
                                     fluidRow(
                                       column(12,
                                              wellPanel(
                                                h3("GPA Model Visualization"),
                                                plotOutput("gpa_model_plot", height="400px")
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              wellPanel(
                                                h3("GPA Model Coefficients"),
                                                tableOutput("gpa_model_table")
                                              )
                                       )
                                     )
                              )
                            )
                          )
                 ),
                 
                 # 4페이지: Correlation Analysis
                 tabPanel("Correlation Analysis",
                          fluidPage(
                            titlePanel("Explore Variable Correlations"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "variables", 
                                  "Variables:", 
                                  choices = non_quantile_vars,
                                  selected = non_quantile_vars[1],
                                  multiple = TRUE 
                                ),
                                selectInput(
                                  "strata", 
                                  "Strata:", 
                                  choices = c("None", "Stress_Level"), 
                                  selected = "None" 
                                ),
                                selectInput(
                                  "theme_c",
                                  "Theme:",
                                  choices = c("Default", "Classic", "Minimal", "Dark"), 
                                  selected = "Default" 
                                ),
                                h3("Graph Options"),
                                selectInput("upper", "Upper Panel:", 
                                            choices = c("cor","blank","box","smooth","points"),
                                            selected = "cor"),
                                selectInput("lower", "Lower Panel:", 
                                            choices = c("cor","blank","box","smooth","points"),
                                            selected = "smooth"),
                                selectInput("diag", "Diagonal Panel:", 
                                            choices = c("densityDiag", "barDiag", "blank"),
                                            selected = "densityDiag"),
                                width = 3
                              ),
                              mainPanel(
                                plotOutput("correlationPlot")
                              )
                            )
                          )
                 ),
                 
                 # 5페이지
                 tabPanel("Variable Comparisons by Given Variable",
                          fluidPage(
                            titlePanel("Compare Variables by a Given Quantile Variable"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "quantileVar", 
                                  "Given Quantile-Based Variable:", 
                                  choices = setNames(given_quantile_vars, given_quantile_vars),
                                  selected = given_quantile_vars[1]
                                ),
                                selectInput(
                                  "xvar", 
                                  "X Variable:", 
                                  choices = setNames(non_quantile_vars, non_quantile_vars), 
                                  selected = non_quantile_vars[1]
                                ),
                                selectInput(
                                  "yvar", 
                                  "Y Variable:", 
                                  choices = setNames(non_quantile_vars, non_quantile_vars), 
                                  selected = non_quantile_vars[2]
                                ),
                                selectInput(
                                  "plotType",
                                  "Plot Type:",
                                  choices = c("Scatter", "Loess", "Contour (2D)", "Hexbin", "Boxplot", "Violin"), 
                                  selected = "Scatter"
                                ),
                                conditionalPanel(
                                  condition = "input.plotType == 'Contour (2D)'",
                                  sliderInput("contour_bins", "Number of Contour Bins:", min=5, max=50, value=10)
                                ),
                                conditionalPanel(
                                  condition = "input.plotType == 'Hexbin'",
                                  sliderInput("hex_bins", "Number of Hex Bins:", min=10, max=100, value=30)
                                ),
                                selectInput(
                                  "theme_q",
                                  "Theme:",
                                  choices = c("Default", "Classic", "Minimal", "Dark"), 
                                  selected = "Default" 
                                ),
                                width = 3
                              ),
                              mainPanel(
                                plotOutput("quantilePlot")
                              )
                            )
                          )
                 ),
                 
                 # 6페이지
                 tabPanel("Distribution by Chosen Ranges",
                          fluidPage(
                            titlePanel("Distribution by Selected Ranges and X Variable"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("facet_range_var", "Select Facet Variable:", 
                                            choices = given_quantile_vars, selected = "Sleep_Range"),
                                uiOutput("facet_levels_ui"),
                                
                                selectInput("fill_range_var", "Select Fill Variable:", 
                                            choices = given_quantile_vars, selected = "Study_Range"),
                                uiOutput("fill_levels_ui"),
                                
                                selectInput("xvar_5", "X Variable:", 
                                            choices = non_quantile_vars,
                                            selected = "GPA"),
                                
                                selectInput("theme_dist", "Theme:",
                                            choices = c("Default", "Classic", "Minimal", "Dark"),
                                            selected = "Default"),
                                width = 3
                              ),
                              mainPanel(
                                plotOutput("dist_plot")
                              )
                            )
                          ))
)

server <- function(input, output, session) {
  
  getThemeOverviewReactive <- reactive({
    getThemeOverview(input$theme_ov)
  })
  
  getThemeLogisticReactive <- reactive({
    getThemeOverview(input$theme_la)
  })
  
  getThemeGPAReactive <- reactive({
    getThemeOverview(input$theme_gpa)
  })
  
  getThemeCorrelation <- reactive({
    switch(input$theme_c,
           "Default" = theme_gray(),
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  getThemeQuantile <- reactive({
    switch(input$theme_q,
           "Default" = theme_gray(),
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  getThemeDistribution <- reactive({
    switch(input$theme_dist,
           "Default" = theme_gray(),
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  sample_data <- function(df, size=500) {
    if (nrow(df) > size) {
      set.seed(123)
      df <- df %>% sample_n(size)
    }
    df
  }
  
  continuous_fun <- function(data, mapping, method="cor", strata=NULL, ...){
    if (!is.null(strata) && strata == "Stress_Level") {
      if (method == "cor") {
        return(GGally::ggally_cor(data, mapping, ...))
      } else if (method == "blank") {
        return(ggplot() + theme_void())
      } else if (method == "box") {
        y_var <- rlang::quo_name(mapping$y)
        ggplot(data, aes_string(x = strata, y = y_var, color=strata)) +
          geom_boxplot(alpha=0.7) +
          theme_minimal() +
          labs(x = strata, y = y_var)
      } else if (method == "smooth") {
        ggplot(data, mapping) +
          geom_point(alpha=0.7, aes(color=Strata)) +
          geom_smooth(method="loess", se=FALSE, aes(color=Strata, group=Strata), size=1.5)
      } else if (method == "points") {
        ggplot(data, mapping) + 
          geom_point(alpha=0.7, aes(color=Strata))
      } else {
        ggplot() + theme_void()
      }
    } else {
      if (method == "cor") {
        GGally::ggally_cor(data, mapping, ...)
      } else if (method == "blank") {
        ggplot() + theme_void()
      } else if (method == "box") {
        y_var <- rlang::quo_name(mapping$y)
        ggplot(data, aes_string(x = 'factor("All")', y = y_var)) +
          geom_boxplot(alpha=0.7) +
          theme_minimal() +
          labs(x = "All", y = y_var)
      } else if (method == "smooth") {
        ggplot(data, mapping) +
          geom_point(alpha=0.7) +
          geom_smooth(method="loess", se=FALSE, color="red", size=1.5)
      } else if (method == "points") {
        ggplot(data, mapping) + geom_point(alpha=0.7)
      } else {
        ggplot() + theme_void()
      }
    }
  }
  
  # diag_fun 수정
  diag_fun <- function(data, mapping, method="densityDiag", strata=NULL, ...){
    if (method == "densityDiag") {
      # strata 여부 확인
      if (!is.null(strata) && strata == "Stress_Level") {
        # Strata 있을 때: 색 다른 선형 density
        ggplot(data, mapping) +
          geom_density(aes(color=Strata), fill=NA) + 
          theme_minimal()
      } else {
        # Strata 없을 때 기본 ggally_densityDiag
        GGally::ggally_densityDiag(data, mapping, ...)
      }
    } else if (method == "barDiag") {
      GGally::ggally_barDiag(data, mapping, ...)
    } else {
      ggplot() + theme_void()
    }
  }
  
  data2 <- data %>% mutate(binary_stress = ifelse(Stress_Level == "High", 1, 0))
  selected_data <- data %>% mutate(Stress_High = ifelse(Stress_Level == "High", 1, 0))
  
  output$overall_summary_table <- renderTable({
    num_cols <- sapply(data, is.numeric)
    num_data <- data[, num_cols, drop=FALSE]
    stat_list <- lapply(num_data, function(x){
      c(Min = min(x, na.rm=TRUE),
        Q1 = as.numeric(quantile(x, 0.25, na.rm=TRUE)),
        Median = median(x, na.rm=TRUE),
        Mean = mean(x, na.rm=TRUE),
        Q3 = as.numeric(quantile(x, 0.75, na.rm=TRUE)),
        Max = max(x, na.rm=TRUE),
        SD = sd(x, na.rm=TRUE))
    })
    stat_df <- do.call(rbind, stat_list)
    data.frame(Variable = rownames(stat_df), stat_df, row.names=NULL)
  })
  
  # View Data
  output$raw_data_table <- renderDT({
    datatable(data,
              options = list(pageLength = input$page_length,
                             dom = 'tp'), 
              filter = "none")
  })
  
  output$main_plot <- renderPlot({
    req(input$plot_type, input$stress_level)
    plot_data <- if (input$stress_level == "All") {
      data
    } else {
      data %>% filter(Stress_Level == input$stress_level)
    }
    
    plot_data <- sample_data(plot_data, 500)
    
    x_var <- switch(input$plot_type,
                    "GPA vs Study Hours" = "Study_Hours_Per_Day",
                    "GPA vs Sleep Hours" = "Sleep_Hours_Per_Day",
                    "GPA vs Extra Hours" = "Extra_Hours_Per_Day",
                    "GPA vs Social Hours" = "Social_Hours_Per_Day",
                    "GPA vs Physical Hours" = "Physical_Activity_Hours_Per_Day")
    
    x_label <- switch(input$plot_type,
                      "GPA vs Study Hours" = "Study Hours Per Day",
                      "GPA vs Sleep Hours" = "Sleep Hours Per Day",
                      "GPA vs Extra Hours" = "Extra Hours Per Day",
                      "GPA vs Social Hours" = "Social Hours Per Day",
                      "GPA vs Physical Hours" = "Physical Activity Hours Per Day")
    
    p <- ggplot(plot_data, aes_string(x = x_var, y = "GPA", color = "Stress_Level")) +
      geom_point(alpha = 0.7) +
      facet_wrap(~ Stress_Level) +
      labs(title = paste("Overall Trend:", gsub("GPA vs ", "", input$plot_type), "by Stress Level"),
           x = x_label,
           y = "GPA") +
      scale_color_viridis_d(name = "Stress Level") +
      getThemeOverviewReactive()
    
    if (input$show_loess) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color="red")
    }
    
    p
  })
  
  output$stress_summary_table <- renderTable({
    df <- data %>%
      group_by(Stress_Level) %>%
      summarise(
        Count = n(),
        Mean_GPA = mean(GPA, na.rm = TRUE),
        Mean_Sleep = mean(Sleep_Hours_Per_Day, na.rm = TRUE),
        Mean_Study = mean(Study_Hours_Per_Day, na.rm = TRUE),
        Mean_Extra = mean(Extra_Hours_Per_Day, na.rm = TRUE)
      ) %>%
      mutate(Stress_Level = factor(Stress_Level, levels = c("High","Moderate","Low"))) %>%
      arrange(Stress_Level)
    df
  })
  
  output$binary_stress_summary_table <- renderTable({
    data2 %>%
      mutate(binary_stress = ifelse(binary_stress == 1, "High", "Low+Moderate")) %>%
      group_by(binary_stress) %>%
      summarise(
        Count = n(),
        Mean_GPA = mean(GPA, na.rm = TRUE),
        Mean_Sleep = mean(Sleep_Hours_Per_Day, na.rm = TRUE),
        Mean_Study = mean(Study_Hours_Per_Day, na.rm = TRUE),
        Mean_Extra = mean(Extra_Hours_Per_Day, na.rm = TRUE)
      ) %>%
      mutate(binary_stress = factor(binary_stress, levels = c("High","Low+Moderate"))) %>%
      arrange(binary_stress)
  })
  
  glm_study <- glm(Stress_High ~ Study_Hours_Per_Day, data = selected_data, family = binomial)
  glm_sleep <- glm(Stress_High ~ Sleep_Hours_Per_Day, data = selected_data, family = binomial)
  glm_extra <- glm(Stress_High ~ Extra_Hours_Per_Day, data = selected_data, family = binomial)
  glm_physical <- glm(Stress_High ~ Physical_Activity_Hours_Per_Day, data = selected_data, family = binomial)
  glm_social <- glm(Stress_High ~ Social_Hours_Per_Day, data = selected_data, family = binomial)
  glm_extra_curricular <- glm(Stress_High ~ Extracurricular_Hours_Per_Day, data = selected_data, family = binomial)
  
  logistic_model_reactive <- reactive({
    model_type <- input$logistic_model_type
    if (model_type == "Study Hours Only") {
      glm_study
    } else if (model_type == "Sleep Hours Only") {
      glm_sleep
    } else if (model_type == "Extra Hours Only") {
      glm_extra
    } else if (model_type == "Physical Hours Only") {
      glm_physical
    } else if (model_type == "Social Hours Only") {
      glm_social
    } else if (model_type == "Extracurricular Hours Only") {
      glm_extra_curricular
    } else {
      w <- input$study_weight
      data2_w <- selected_data %>% 
        mutate(Weighted_Hours = w * Study_Hours_Per_Day + (1 - w) * Sleep_Hours_Per_Day)
      glm(Stress_High ~ Weighted_Hours, data = data2_w, family = binomial)
    }
  })
  
  output$logistic_model_table <- renderTable({
    glm_mod <- logistic_model_reactive()
    coef_summary <- summary(glm_mod)$coefficients
    data.frame(
      Term = rownames(coef_summary),
      Estimate = coef_summary[, "Estimate"],
      Std_Error = coef_summary[, "Std. Error"],
      Z_value = coef_summary[, "z value"],
      P_value = coef_summary[, "Pr(>|z|)"],
      row.names = NULL
    )
  })
  
  output$logistic_model_plot <- renderPlot({
    glm_mod <- logistic_model_reactive()
    model_type <- input$logistic_model_type
    
    if (model_type == "Study Hours Only") {
      xvar <- "Study_Hours_Per_Day"
      df <- selected_data
    } else if (model_type == "Sleep Hours Only") {
      xvar <- "Sleep_Hours_Per_Day"
      df <- selected_data
    } else if (model_type == "Extra Hours Only") {
      xvar <- "Extra_Hours_Per_Day"
      df <- selected_data
    } else if (model_type == "Physical Hours Only") {
      xvar <- "Physical_Activity_Hours_Per_Day"
      df <- selected_data
    } else if (model_type == "Social Hours Only") {
      xvar <- "Social_Hours_Per_Day"
      df <- selected_data
    } else if (model_type == "Extracurricular Hours Only") {
      xvar <- "Extracurricular_Hours_Per_Day"
      df <- selected_data
    } else {
      w <- input$study_weight
      df <- selected_data %>% mutate(Weighted_Hours = w * Study_Hours_Per_Day + (1 - w)*Sleep_Hours_Per_Day)
      xvar <- "Weighted_Hours"
    }
    
    ggplot(df, aes_string(x = xvar, y = "Stress_High")) + 
      geom_jitter(height = 0.1, width = 0.25, size = 0.7, alpha=0.6, color="black") + 
      stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") + 
      stat_smooth(method = "loess", color = "blue") +
      scale_y_continuous(breaks = c(0,1), labels = c("Low+Moderate", "High")) +
      labs(title = paste("Logistic Model:", model_type)) +
      getThemeLogisticReactive()
  })
  
  gpa_model_reactive <- reactive({
    model_type <- input$gpa_model_type
    w <- if (model_type == "Weighted Sleep+Study") input$gpa_sleep_weight else 0.5
    if (model_type == "Sleep Only") {
      lm(GPA ~ Sleep_Hours_Per_Day, data=data)
    } else if (model_type == "Study Only") {
      lm(GPA ~ Study_Hours_Per_Day, data=data)
    } else if (model_type == "Social Only") {
      lm(GPA ~ Social_Hours_Per_Day, data=data)
    } else if (model_type == "Extracurricular Only") {
      lm(GPA ~ Extracurricular_Hours_Per_Day, data=data)
    } else if (model_type == "Physical Only") {
      lm(GPA ~ Physical_Activity_Hours_Per_Day, data=data)
    } else if (model_type == "Extra Only") {
      lm(GPA ~ Extra_Hours_Per_Day, data=data)
    } else {
      data_w <- data %>%
        mutate(Weighted_Hours = w * Sleep_Hours_Per_Day + (1-w)*Study_Hours_Per_Day)
      lm(GPA ~ Weighted_Hours, data=data_w)
    }
  })
  
  output$gpa_model_table <- renderTable({
    m <- gpa_model_reactive()
    coef_summary <- summary(m)$coefficients
    data.frame(
      Term = rownames(coef_summary),
      Estimate = coef_summary[, "Estimate"],
      Std_Error = coef_summary[, "Std. Error"],
      t_value = coef_summary[, "t value"],
      P_value = coef_summary[, "Pr(>|t|)"],
      row.names = NULL
    )
  })
  
  output$gpa_model_plot <- renderPlot({
    m <- gpa_model_reactive()
    model_type <- input$gpa_model_type
    w <- if (model_type == "Weighted Sleep+Study") input$gpa_sleep_weight else 0.5
    
    xvar <- switch(model_type,
                   "Sleep Only" = "Sleep_Hours_Per_Day",
                   "Study Only" = "Study_Hours_Per_Day",
                   "Social Only" = "Social_Hours_Per_Day",
                   "Extracurricular Only" = "Extracurricular_Hours_Per_Day",
                   "Physical Only" = "Physical_Activity_Hours_Per_Day",
                   "Extra Only" = "Extra_Hours_Per_Day",
                   "Weighted Sleep+Study" = "Weighted_Hours")
    
    if (model_type == "Weighted Sleep+Study") {
      df <- data %>%
        mutate(Weighted_Hours = w * Sleep_Hours_Per_Day + (1-w)*Study_Hours_Per_Day)
      xlab <- paste0("Weighted Hours (w=", w, ")")
    } else {
      df <- data
      xlab <- gsub("_Hours_Per_Day"," Hours", xvar)
      xlab <- gsub("_"," ", xlab)
    }
    
    p <- ggplot(df, aes_string(x=xvar, y="GPA")) +
      geom_point(alpha=0.6, color="black") +
      geom_smooth(method="lm", color="red", se=FALSE) +
      geom_smooth(method="loess", color="blue", se=FALSE) +
      labs(x = xlab, y = "GPA", title = paste(model_type, "Model")) +
      theme_minimal() + getThemeGPAReactive()
    
    p
  })
  
  continuous_fun_wrapper <- function(method) {
    function(data, mapping, ...) {
      continuous_fun(data, mapping, method=method, strata=input$strata, ...)
    }
  }
  
  output$correlationPlot <- renderPlot({
    req(input$variables)
    selected_vars <- input$variables
    strata <- input$strata
    
    if (length(selected_vars) == 0) {
      return(NULL)
    }
    
    plot_data <- data
    if (strata != "None") {
      plot_data <- plot_data %>% mutate(Strata = .data[[strata]])
    }
    
    plot_data <- sample_data(plot_data, 500)
    
    ggpairs(plot_data, columns = match(selected_vars, colnames(plot_data)),
            mapping = if (strata != "None") aes(color = Strata) else NULL,
            upper = list(continuous = continuous_fun_wrapper(input$upper)),
            lower = list(continuous = continuous_fun_wrapper(input$lower)),
            diag = list(continuous = function(data,mapping,...){diag_fun(data,mapping,method=input$diag,strata=input$strata,...)})
    ) + getThemeCorrelation()
  })
  
  output$quantilePlot <- renderPlot({
    req(input$quantileVar, input$xvar, input$yvar, input$plotType)
    quantile_var <- input$quantileVar
    x_var <- input$xvar
    y_var <- input$yvar
    plot_type <- input$plotType
    
    data_quantile <- data %>%
      mutate(
        Quantile_Group = factor(.data[[quantile_var]], 
                                levels = c("1Q", "2Q", "3Q", "4Q"))
      ) %>%
      filter(!is.na(Quantile_Group))
    
    data_quantile <- sample_data(data_quantile, 500)
    
    if (plot_type %in% c("Scatter", "Loess", "Contour (2D)", "Hexbin")) {
      p <- ggplot(data_quantile, aes_string(x = x_var, y = y_var))
    } else {
      p <- ggplot(data_quantile, aes(x = Quantile_Group, y = .data[[y_var]]))
    }
    
    if (plot_type == "Scatter") {
      p <- p + geom_point(alpha = 0.6, color = "black") +
        facet_wrap(~ Quantile_Group, nrow = 2, ncol = 2)
    } else if (plot_type == "Loess") {
      p <- p + geom_point(alpha = 0.6, color = "black") +
        geom_smooth(method = "loess", se = FALSE, color = "red") +
        facet_wrap(~ Quantile_Group, nrow = 2, ncol = 2)
    } else if (plot_type == "Contour (2D)") {
      p <- p + geom_density_2d_filled(alpha = 0.5, bins = input$contour_bins) +
        facet_wrap(~ Quantile_Group, nrow = 2, ncol = 2)
    } else if (plot_type == "Hexbin") {
      p <- p + geom_hex(bins = input$hex_bins) +
        facet_wrap(~ Quantile_Group, nrow = 2, ncol = 2)
    } else if (plot_type == "Boxplot") {
      p <- p + geom_boxplot(alpha = 0.7, fill="dodgerblue")
    } else if (plot_type == "Violin") {
      p <- p + geom_violin(trim = FALSE, fill="lightgreen", alpha=0.7)
    }
    
    p + getThemeQuantile() +
      labs(
        title = paste("Comparison by", quantile_var, "Groups:", x_var, "vs", y_var),
        x = if (plot_type %in% c("Boxplot", "Violin")) "Quantile Group" else x_var,
        y = y_var
      )
  })
  
  observeEvent(input$facet_range_var, {
    req(input$facet_range_var)
    lvls <- levels(data[[input$facet_range_var]])
    updateCheckboxGroupInput(session, "facet_levels", choices = lvls, selected = lvls)
  }, ignoreInit = TRUE)
  
  observeEvent(input$fill_range_var, {
    req(input$fill_range_var)
    lvls <- levels(data[[input$fill_range_var]])
    updateCheckboxGroupInput(session, "fill_levels", choices = lvls, selected = lvls)
  }, ignoreInit = TRUE)
  
  output$facet_levels_ui <- renderUI({
    req(input$facet_range_var)
    lvls <- levels(data[[input$facet_range_var]])
    checkboxGroupInput("facet_levels", "Facet Levels:", choices = lvls, selected = lvls)
  })
  
  output$fill_levels_ui <- renderUI({
    req(input$fill_range_var)
    lvls <- levels(data[[input$fill_range_var]])
    checkboxGroupInput("fill_levels", "Fill Levels:", choices = lvls, selected = lvls)
  })
  
  output$dist_plot <- renderPlot({
    req(input$facet_range_var, input$fill_range_var, input$facet_levels, input$fill_levels, input$xvar_5)
    facet_var <- input$facet_range_var
    fill_var <- input$fill_range_var
    x_var_5 <- input$xvar_5
    
    plot_data <- data %>%
      filter(.data[[facet_var]] %in% input$facet_levels,
             .data[[fill_var]] %in% input$fill_levels)
    
    plot_data <- sample_data(plot_data, 500)
    
    ggplot(plot_data, aes(x = .data[[x_var_5]], fill = .data[[fill_var]])) +
      geom_density(alpha = 0.7) +
      facet_wrap(as.formula(paste("~", facet_var))) +
      labs(title = "Distribution by Selected Ranges and X Variable",
           x = x_var_5, 
           y = "Density") +
      scale_fill_viridis_d(name = fill_var) +
      getThemeDistribution()
  })
  
}

shinyApp(ui, server)

