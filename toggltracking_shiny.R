
# library

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

# api configuration
api_token <- readLines("API/toggl_API.txt")
base_url <- "https://toggl.com"

# API request - time entries
resp_time <- request("https://api.track.toggl.com/api/v9/me/time_entries") |>
  req_auth_basic(api_token, "api_token") |>
  req_perform()

entries <- fromJSON(resp_body_string(resp_time))

# API request - project names
resp_projects <- request("https://api.track.toggl.com/api/v9/me/projects") |>
  req_auth_basic(api_token, "api_token") |>
  req_perform()

projects <- fromJSON(resp_body_string(resp_projects))

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

# join entries and levels
status <- entries_alltime |> 
  left_join(levels, join_by(sum_hours >= hours)) |> 
  # keep highest match from levels
  group_by(project) |> 
  slice_max(level, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  # drop unranked
  filter(!is.na(level)) |> 
  # generate label
  mutate(
    rank = case_when(
      is.na(level) ~ "Unranked",
      .default = rank
    ),
    guild = case_when(
      is.na(level) ~ "Unranked",
      .default = guild
    ),
    label = case_when(
      !is.na(level) ~ paste0(guild, " ", rank),
      .default = "No Rank"
    )
  ) |> 
  arrange(project)

# shiny build

ui <- f7Page(
  title = "Projects",
  
  f7SingleLayout(
    navbar = f7Navbar(title = "Projects"),
    
    uiOutput("project_cards")
  )
)

server <- function(input, output, session) {
  
  output$project_cards <- renderUI({
    
    cards <- lapply(seq_len(nrow(status)), function(i) {
      
      f7Card(
        
        div(
          style = "
            display:flex;
            justify-content:space-between;
            align-items:baseline;
            margin-bottom:8px;
          ",
          
          h3(
            status$project[i],
            style = "margin:0,"
          ),
          
          span(
            sprintf("%.1f hours", status$sum_hours[i]),
            style = "
              font-weight:600;
              color:#666;
            "
          )
        ),
        
        # Tier row
        div(
          style = "
              display:flex;
              align-items:center;
              margin-bottom:4px;
              gap:6px;
            ",
          
          span(sprintf("%s Guild", status$guild[i])),
          f7Icon("star_fill")
        ),
        
        # Rank row
        div(
          style = "
              display:flex;
              align-items:center;
              margin-bottom:8px;
              gap:6px
            ",
          
          span(sprintf("Rank: %s", status$rank[i])),
          f7Icon("person_fill")
        ),
      )
      
    })
    
    tagList(cards)
    
  })
  
}

shinyApp(ui, server)
