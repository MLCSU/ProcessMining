# PM04-Version of the Shiny app to show final activities prior to departure
# (and any after recorded after departure)

#options(shiny.error = browser)

library(shiny)
library(tidyverse)
library(bupaverse)
library(DiagrammeR)
grVizOutput(outputId = "process")

## The below taken from chunks in PM04-LastActivities.qmd
##Should be simplified/made into functions
# Load data
rawdata <- read_csv("../data/raw/DES Data Final.csv",
                    col_types=list(
                      AttendanceIdentifier = col_character()
                    ))
# 252 rows have NA timestamps, in 218 attendances
# remove those attendances completely
attendances <- rawdata %>% 
  filter(is.na(TimeStamp)) %>% 
  distinct(AttendanceIdentifier)
rawdata <- rawdata %>% 
  filter(! AttendanceIdentifier %in% pull(attendances))

# Sort by Activity for events with same timestamp as two attendances with same
# activities at same time but in a different order would show as different 
# process map 
# Probably should decide on a 'best' order rather than just alphabetic
# Here we put departed after other things with same timestamp
rawdata <- rawdata %>% 
  arrange(AttendanceIdentifier, 
          TimeStamp,
          str_detect(Activity, 'Departed'),
          Activity,
          ActivityType)

# Remove the 'location' activities as not relevant for this view
rawdata <- rawdata %>% 
  filter(Activity != "Location")

# Get the last timestamp that occurs before departure
prevTime <- rawdata %>% 
  group_by(AttendanceIdentifier) %>%
  mutate(depTime = max(TimeStamp[Activity=="Departed"])) %>% 
  filter(TimeStamp < depTime) %>% 
  summarise(prevTime = max(TimeStamp))

# Then join that back into the dataframe
# and filter to only activities on or after the last timestamp before
# departure (note that some things recorded after departure)
data <- rawdata %>% 
  left_join(prevTime) %>% 
  filter(TimeStamp >= prevTime)

# We have removed locations, but keeping this from shiny_01 for forms
## EITHER Label the locations
#data$ActivityDetail <- case_when(data$Activity == "Location" ~ 
#                             paste(data$Activity, data$ActivityType, sep=": "),
#                             .default = data$Activity)
# OR Label locations and CDC Forms
# Below adding str_remove_all gets rid of syntax error (due to ' in a form)
# but still fails to draw the map, out of memory error
#data$ActivityDetail <- case_when(data$Activity == "Location" ~ 
#                             paste(data$Activity, data$ActivityType, sep=": "),
#                             data$Activity == "CDC Form" ~ 
#                             paste("Form", 
#                                   str_remove_all(data$ActivityType,"'"),
#                                   sep=": "),
#                             .default = data$Activity)
# OR Label locations and Top 10 Forms vs other CDC Forms
data$ActivityDetail <- case_when(data$Activity == "Location" ~ 
                                   paste(data$Activity, data$ActivityType,
                                         sep=": "),
                                 data$Activity == "CDC Form" & 
                                   data$ActivityType %in% 
                                   c("Medical Assessment - ED (UHMB)",
                                     "Nursing Assessment - ED (UHMB)",
                                     "Manchester Triage System (UHMB)",
                                     "Doctors and ANP Ward Note v4 (UHMB)",
                                     "Specialist Nurse Note",
                                     "Safeguarding Note UHMB",
                                     "ED - Streaming (UHMB)",
                                     "Doctor Admission Assessment (UHMB)",
                                     "Health Professional Ward Note",
                                     "Safeguarding Assessment (UHMB)") ~
                                   paste("Form", 
                                         str_remove_all(data$ActivityType,"'"),
                                         sep=": "),
                                 data$Activity == "CDC Form" ~ "Other CDC Form",
                                 .default = data$Activity)

# Remove activities with the same detail which happen at the same time for this
# attendance.  
# Done to treat multiple "EObs Patient Observation" at the same
# time as one set of observations.  Hope no unintended consequences.  If start
# to look at Staff may need to revisit
data <- data %>% 
  distinct(AttendanceIdentifier, 
           ActivityDetail,
           TimeStamp, .keep_all = TRUE)

# Create an Event Log (validate=FALSE due to issue where ActivityIdentifier
# covers more than one activity-e.g. triage&CDC Form both 650019178927)
event <- data %>% 
  bupaR::eventlog(case_id = "AttendanceIdentifier", 
                  activity_id = "ActivityDetail",
                  activity_instance_id = "ActivityIdentifier",
                  lifecycle_id = "RegistrationType",
                  timestamp = "TimeStamp",
                  resource_id = "Staff",
                  validate = FALSE)

# Define UI for application that draws a process map
ui <- fluidPage(

    # Application title
    titlePanel("Final Activities before (and after) Departure"),

    # Sidebar with a slider input for frequency 
    sidebarLayout(
        sidebarPanel(width = 1,
            sliderInput("freq",
                        "Frequency of traces to include:",
                        min = 0,  max = 1, value = 1),
#        hr(), # Add a horizontal rule
#        checkboxInput("rem_no_dis", "Remove undischarged", FALSE),
#        checkboxInput("rem_loops", "Simplify multiple days on pathway", FALSE),
#        checkboxInput("sim_discharge", "Simplify discharges", FALSE),
        ),
        # Show the map and also some sliders to zoom
        mainPanel(width = 11,
           uiOutput("d"),
           sliderInput(inputId = "height", label = "Height", 
                       min = 0, max = 2000, value = 1200, step = 200),
           sliderInput(inputId = "width", label = "Width", 
                       min = 0, max = 2000, value = 2000, step = 200)
        )
    )
)

# Define server logic required to draw process map
server <- function(input, output) {

  filter1 <- reactive({filter_trace_frequency(event, 
                                              percentage = input$freq)
  })
  
      output$d <- renderUI({
        grVizOutput(
          "process", height = input$height, width = input$width
        )
      })  
  
      output$process <- renderGrViz({
        plot <- process_map(filter1(), 
                            type_nodes= frequency(),
                            sec_nodes=performance(units="mins"),
                            type_edges=frequency(),
                            sec_edges=performance(units="mins", 
                                                  flow_time="inter_start_time"),
                            render=FALSE)
        render_graph(plot)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
