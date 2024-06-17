# load packages -----------------------------------------------------------

library(highcharter)
# library(custom.map)
library(tidyverse)
library(dplyr)
library(readxl)
library(htmltools)
library(shiny)
library(bsutils)
library(jsonlite)
library(magrittr)
library(quarto)
library(revealjs)

#STYLES -----------------
## set colors --------------------------------------------------------------

###define colors-------------------------------------------------------
main_blue <- "#005182"
light_blue <- "#70C4E8"
main_orange <- "#C65227"
dark_orange <- "#90361F"
main_yellow <- "#EBB41F"
light_yellow <- "#FFD780"
NA_color <- "#E6E1E7"
dark_accent <- "#242c3d"
light_accent <- "#cae6f2"
icon_accent <- "#47BA83"
main_colors <- c(light_blue, main_orange, main_yellow, icon_accent, main_blue)
dark_main_colors <- c("#325F87","#D86B45", "#A67B05","#287E58", "#15395A") #these are for bubble

###gov colors --------------------------------------------------------
gov_color_2 <- c(main_blue, main_orange)
gov_color_3 <- c(light_yellow, main_yellow, main_orange)
gov_color_4 <- c(main_yellow, main_blue, light_blue, main_orange)
gov_color_5 <- c(light_blue, main_orange, main_blue, icon_accent, main_yellow)
gov_oranges <- c(light_yellow, main_yellow,main_orange,dark_orange)

###structure colors --------------------------------------------------
struct_base <- c(main_orange, main_blue)
struct_color_3 <-c(light_blue, light_accent, NA_color)
struct_color_2 <-c(light_blue, NA_color)


# define visualization theme ----------------------------------------------

astho_theme <- hc_theme(
  colors = main_colors,
  chart = list(
    backgroundColor = NULL
  ),
  style = list(
    fontFamily = "Jost"
  ),
  title = list(
    style = list(
      color = dark_accent,
      fontFamily = "Jost",
      fontWeight = "500",
      fontSize = "20px"
    )
  ),
  subtitle = list(
    style = list(
      color = dark_accent,
      fontFamily = "Jost",
      fontSize = "14px"
    )
  ),
  caption = list(
    style = list(
      color = "#7e7f7f",
      fontFamily = "Jost",
      fontSize = "12px"
    )
  ),
  xAxis = list(
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "15px",  # Adjusted to be consistent with yAxis labels
        fontWeight = "normal",
        color = "#666"
      )
    ),
    title = list(
      style = list(
        color = dark_accent,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "15px"
      )
    )
  ),
  yAxis = list(
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "15px",  # Adjusted to be consistent with xAxis labels
        fontWeight = "normal",
        color = "#666"
      )
    ),
    title = list(
      style = list(
        color = dark_accent,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "15px"
      )
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Jost",
      color = dark_accent,  # Removed duplicate color attribute
      fontSize = "17px",
      fontWeight = "normal"
    ),
    title = list(
      style = list(
        textDecoration = "none",
        fontFamily = "Jost",
        fontSize = "16px"
      )
    )
  ),
  tooltip = list(
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "14px")
  )
)





#topics counts viz-----------------------------------------------
topic_counts <- data.frame(
  Topic = c("Documents", "Importance/WIFM", "Types of considerations",
            "Meetings", "Section 508, ADA, OCR, etc.", "Language",
            "Color and Design",
            "Culture", "Descriptive links",
            "Differences in recommendations",
            "3rd party platform eval", "Disability justice",
            "Vendor Selection/VPATs/etc.",
            "Budgeting", "Data Viz",
            "Multiplicity",
            "Basics/101"),
  Count = c(2, 3, 2, 8, 3, 3, 5, 3, 3, 2, 5, 5, 1, 2, 4, 2, 1),
  Category = c("Products", "Awareness", "Awareness",
               "Internal", "Compliance", "Products",
               "Products",
               "Internal", "Products",
               "Awareness",
               "Compliance", "Internal",
               "Compliance",
               "Internal", "Products",
               "Awareness",
               "Awareness")
)

# Display the data frame
# View(topic_counts)

topic_bubble <- hchart(topic_counts, "packedbubble",
                          hcaes(name = Topic, value = Count, group = Category )) %>%
  hc_xAxis(type = "category", title = list(text = "Strength of Evidence Base")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_plotOptions(
    packedbubble = list(
      maxSize = "100%",
      zMin = 0,
      layoutAlgorithm = list(
        gravitationalConstant = 0.045,
        splitSeries = TRUE,
        seriesInteraction = TRUE,
        dragBetweenSeries = FALSE,
        parentNodeLimit = TRUE
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}"

      ),
      style = list(
        color = "black",
        textOutline = "none",
        fontWeight = "normal"
      ),
      labels = list(
        style = list(fontSize = "10px")
      ),
      tooltip = list(
        pointFormat = "<b>{point.group}</b>{point.name}<br> n = {point.value}"  # Added line break with <br> tag
      )
    )
  ) %>%
  hc_add_theme(astho_theme)

topic_bubble


#training modes viz--------------------------
training_modes <- data.frame(
  Mode = c("Small groups within our units", "All staff meeting", "Interactive",
           "External evaluation and reflection", "Monday board training plan",
           "Videos and quizzes", "Self-paced",
           "Integrated into ASTHO processes",
           "Gamification", "Practice evaluations", "Anonymous story sharing",
           "Training by someone with lived experience", "ASTHO Day/Week Session"),
  Count = c(1, 2, 2, 1, 2, 1, 1, 10, 4, 3, 5, 6, 5)
)

# Display the data frame

# View(training_modes)

mode_cloud <- hchart(training_modes,
                        "wordcloud",
                        rotation = 0,
                        hcaes(name = Mode, weight = Count)
  ) %>%
  # hc_title(text = "Summary of Emerging Needs Addressed, by Health Topic Area <br> (n = 118)") %>%
  # tooltip
  hc_tooltip(
    headerFormat = "", # remove header
    pointFormat = "<span style='font-weight: 500; font-size:16px; color:
                      #005182;'>{point.name} <br> n = {point.weight} </span>",
    useHTML = TRUE,
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "16px"
    )
  ) %>%
  hc_add_theme(astho_theme) %>%
  hc_colors(main_blue)

mode_cloud

#resources---------
resources <- data.frame(
  Type = c("Checklists (CD, meetings, etc.)", "On demand offerings", "Break Room Tips",
           "Proactive TA for S/THOs", "PD Opportunities", "Assesment of S/THAs",
           "ASTHO Internal perception assesment", "Easy things you can do right now",
           "Communication best practices", "List of partners that can help",
           "Policy statement on inclusion"),
  Count = c(6, 2, 1, 7, 2, 3, 2, 2, 5, 1, 4)
)

# view(resources)

resources_cloud <- hchart(resources,
                     "wordcloud",
                     rotation = 0,
                     hcaes(name = Type, weight = Count)
) %>%
  # hc_title(text = "Summary of Emerging Needs Addressed, by Health Topic Area <br> (n = 118)") %>%
  # tooltip
  hc_tooltip(
    headerFormat = "", # remove header
    pointFormat = "<span style='font-weight: 500; font-size:16px; color:
                      #005182;'>{point.name} <br> n = {point.weight} </span>",
    useHTML = TRUE,
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "16px"
    )
  ) %>%
  hc_add_theme(astho_theme) %>%
  hc_colors(main_blue)

resources_cloud
