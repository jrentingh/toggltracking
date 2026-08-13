
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
resp_time <- request("https://api.track.toggl.com/api/v9/me/time_entries") |>
  req_auth_basic(api_token, "api_token") |>
  req_perform()

entries <- fromJSON(resp_body_string(resp_time))

# API request - project names
resp_projects <- request("https://api.track.toggl.com/api/v9/me/projects") |>
  req_auth_basic(api_token, "api_token") |>
  req_perform()

# data cleaning ---------------------------------------------------------------

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

# join levels data
status <- entries_alltime |> 
  left_join(levels, join_by(sum_hours >= hours)) |> 
  rename(level_hours = hours) |> 
  # keep highest match from levels
  group_by(project) |> 
  slice_max(level, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
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

ui <- f7Page(
  title = "Projects",
  
  f7SingleLayout(
    navbar = f7Navbar(title = "Projects"),
    
    uiOutput("project_cards")
  )
)

server <- function(input, output, session) {
  
  output$project_cards <- renderUI({
    
    cards <- lapply(seq_len(nrow(status_output)), function(i) {
      
      f7Card(
        
        # ============================================================
        # LEVEL 1: Card
        # ============================================================
        div(
          style = "
            display:flex;
            justify-content:space-between;
            align-items:center;
            width:100%;
            gap:20px;
          ",
          
          # ----------------------------------------------------------
          # LEVEL 2: Left side — text/content
          # ----------------------------------------------------------
          div(
            style = "
              flex:1;
              display:flex;
              flex-direction:column;
              justify-content:center;
            ",
            
            # LEVEL 3: Project name
            div(
              style = "
                display:flex;
                align-items:baseline;
                margin-bottom:6px;
              ",
              h3(
                status_output$project[i],
                style = "margin:0;"
                )
              ),
            # LEVEL 3: Project hours
            div(
              style = "
                display:flex;
                align-items:baseline;
                margin-bottom:6px;
              ",
              span(
                sprintf("%.1f hours", status_output$sum_hours[i]),
                style = "
                  font-weight:600;
                  color:#666;
                "
                )
              ),
            # LEVEL 3: Long label (Lvl. x [Guild] [Rank])
            div(
              style = "
                display:flex;
                align-items:center;
                gap:6px;
              ",
              status_output$long_label[i]
              )
            ), # ------------END LEVEL 2: Left side ------------------
          # ----------------------------------------------------------
          # LEVEL 2: Right side — image
          # ----------------------------------------------------------
          div(
            style = "
              flex:0 0 140px;
              display:flex;
              justify-content:center;
              align-items:center;
            ",
            # LEVEL 3: Image
            tags$img(
              src = paste0("images/", status_output$image_file[i]),
              style = "
                width:140px;
                height:140px;
                object-fit:contain;
              "
              )
            ) # ------------ END LEVEL 2: Right side ----------------
          ) # ================ END LEVEL 1: CARD ====================
        )
      })
    tagList(cards)
    })
}



shinyApp(ui, server)
