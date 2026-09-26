
# library ---------------------------------------------------------------------
library(shiny)
library(shinyMobile)
library(tidyverse)
library(httr2)
library(jsonlite)
library(purrr)
library(readxl)
library(writexl)
library(webr)
library(rsconnect)

# API -------------------------------------------------------------------------

# api configuration
api_token <- readLines("API/toggl_API.txt")
base_url <- "https://toggl.com"

# API request - time entries
# will eventually need a way to iterate this on a weekly/monthly basis, as I believe it limits out at 1,000 entries.
resp_time <- request("https://api.track.toggl.com/api/v9/me/time_entries") |>
  req_url_query(
    start_date = "2026-07-31",
    end_date = paste0(as.character(Sys.Date()), "T23:59:59Z")
  ) |> 
  req_auth_basic(api_token, "api_token") |>
  req_perform()

entries <- fromJSON(resp_body_string(resp_time))

# API request - project names
resp_projects <- request("https://api.track.toggl.com/api/v9/me/projects") |>
  req_auth_basic(api_token, "api_token") |>
  req_perform()

projects <- fromJSON(resp_body_string(resp_projects))

# data cleaning ---------------------------------------------------------------

project_names <- projects |> 
  rename(
    project = name,
    project_id = id
  ) |> 
  select(project, project_id)

# merge project names with entries data
entries_clean <- entries |> 
  # merge project names
  left_join(project_names, by = join_by("project_id")) |> 
  # select vars
  select(
    billable,
    start,
    stop,
    duration,
    description,
    project
  ) |> 
  # clean time vars
  mutate(
    start = with_tz(ymd_hms(start),"America/Detroit"),
    stop = with_tz(ymd_hms(stop), "America/Detroit"),
    date = date(with_tz(date(stop), "America/Detroit")),
    week = week(date),
    week_date = floor_date(date, unit = "week"),
    year = year(date),
    duration_hrs = round(duration / 60 / 60, 2)
  )

# read levels table
levels <- read_csv("data/levels_v2.csv")

# collapse entries data
entries_alltime <- entries_clean |> 
  group_by(project) |> 
  summarize(
    sum_hours = sum(duration_hrs, na.rm = TRUE)
  ) |> 
  arrange(desc(sum_hours))

# join levels data
status <- entries_alltime |> 
  left_join(levels, join_by(sum_hours >= hours)) |> 
  rename(level_hours = hours) |> 
  # keep highest match from levels
  group_by(project) |> 
  slice_max(level, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  # calculate next level hours 
  mutate(
    next_level_hours = map_dbl(
      sum_hours, # take sum_hous as an input
      ~ min(levels$hours[levels$hours > .x], na.rm = TRUE) # lookup the minimum value of levels$hours, where sum_hours is > levels$hours
    ),
    hours_to_next_level = next_level_hours - sum_hours
  ) |> 
  # generate labels
  mutate(
    short_label = if_else(!is.na(level), paste0(guild, " ", rank), NA_character_),
    long_label = if_else(!is.na(level), paste0("Lvl. ", level, " ", guild, " ", rank), NA_character_)
  )
  
  
status_output <- status |> 
  # drop unranked
  filter(!is.na(level)) |> 
  # generate image filepath
  mutate(
    image_file = paste0(
      tolower(gsub(" ", "_", guild)), 
      "_", 
      tolower(gsub(" ", "_", rank)), 
      ".png"
    )
  ) |> 
  arrange(project)
  

# shiny build -----------------------------------------------------------------

## ui --------------------------------

ui <- f7Page(
  title = "Projects",
  
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Projects"
    ),
    
    #### tabs ------------
    
    f7Tabs(
      id = "tabs",
      
      # tab - projects list
      
      f7Tab(
        tabName = "projects",
        active = TRUE,
        uiOutput("project_cards")
      ),
      
      # tab - detail page
      
      f7Tab(
        tabName = "project_detail",
        hidden = TRUE,
        
        ## navigation bar
        
        f7Navbar(
          title = "Project Details",
          backLink = TRUE
        ),
        
        ## plot
        
        f7Block(
          h2(textOutput("selected_project")),
          plotOutput(
            "effort_trend",
            height = "180px"
          )
        )
      ) # end detail page
    ) # end tabs
  ) # end layout
) # end ui


## server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$project_cards <- renderUI({
    
    cards <- lapply(
      seq_len(nrow(status_output)),
      function(i) {
        
        # level 1: card settings ------------
        
        f7Card(
          
          div(
            
            onclick = sprintf(
              "Shiny.setInputValue(
                'selected_project',
                %d,
                {priority: 'event'}
              )",
              i
            ),
            
            style = "
              display:flex;
              justify-content:space-between;
              align-items:center;
              width:100%;
              gap:20px;
              cursor:pointer;
            ",
            
            ## level 2: left side - text -------------
            
            div(
              style = "
                flex:1;
                display:flex;
                flex-direction:column;
                justify-content:center;
              ",
              
              ### level 3: project name ------------
              div(
                style = "
                  display:flex;
                  align-items:baseline;
                  margin-bottom:6px;
                ",
                
                h2(
                  status_output$project[i],
                  style = "margin:0;"
                )
              ),
              
              ### level 3: project hours
              div(
                style = "
                  display:flex;
                  align-items:baseline;
                  margin-bottom:6px;
                ",
                
                span(
                  sprintf(
                    "%.1f hours",
                    status_output$sum_hours[i]
                  ),
                  
                  style = "
                    font-weight:600;
                    color:#666;
                  "
                )
              ),
              
              ### level 3: long name -------------
              div(
                style = "
                  display:flex;
                  align-items:center;
                  gap:6px;
                ",
                
                status_output$long_label[i]
              ),
              
              ### level 3: hours to next level -------------
              div(
                style = "
                  display:flex;
                  align-items:center;
                  gap:6px;
                ",
                
                paste0(
                  floor(
                    status_output$hours_to_next_level[i]
                  ),
                  "h ",
                  round(
                    (
                      status_output$hours_to_next_level[i] %% 1
                    ) * 60
                  ),
                  "m to next level"
                )
              )
            ), # end level 2: left side - text
            
            ## level 2: right side - image ---------------
            
            div(
              style = "
                flex:0 0 140px;
                display:flex;
                justify-content:center;
                align-items:center;
              ",
              
              tags$img(
                src = paste0(
                  "images/",
                  status_output$image_file[i]
                ),
                
                style = "
                  width:140px;
                  height:140px;
                  object-fit:contain;
                "
              )
            ) # end level 2: right side - image
          ) # end level 1: card settings
        ) # end f7Card
      } # end function
    ) # end lapply
    tagList(cards)
  }) # end renderUI
  
  
  # ==============================================================
  # WHEN A CARD IS CLICKED
  # ==============================================================
  
  observeEvent(input$selected_project, {
    
    selected <- input$selected_project
    
    # Display selected project name
    output$selected_project <- renderText({
      
      status_output$project[selected]
      
    })
    
    # Navigate to detail tab
    updateF7Tabs(
      id = "tabs",
      selected = "project_detail",
      session = session
    )
  })
  
  
  # ==============================================================
  # EFFORT TREND
  # ==============================================================
  
  output$effort_trend <- renderPlot(
    
    bg = "transparent", 
    
    {
    
    req(input$selected_project)
    
    selected <- status_output$project[
      input$selected_project
    ]
    
    entries_clean |>
      filter(
        project == selected,
        week >= week(Sys.Date()) - 12,
        week <= week(Sys.Date())
      ) |>
      dplyr::group_by(week_date) |>
      dplyr::summarize(
        hours = sum(duration_hrs),
        .groups = "drop"
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = week_date,
          y = hours
        )
      ) +
      ggplot2::geom_line(
        color = "orange2",
        linewidth = 1
      ) +
      ggplot2::geom_point(
        color = "orange2",
        size = 3
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x, "h")
      ) +
      ggplot2::theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey50"),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(color = "white"),
        axis.line.y.left = element_line(color = "white"),
        axis.text = element_text(color = "white")
      ) +
      labs(
        x = "",
        y = ""
      )
  })
}


shinyApp(ui, server)