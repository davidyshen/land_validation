# Required Libraries
library(shiny)
library(leaflet)
library(sf)
library(terra)
library(stringi)

current_filename <- NULL

# Make folder if it doesn't already exist for storing points
if (!dir.exists("~/landcover_validation_shiny/")) {
  dir.create("~/landcover_validation_shiny/")
}
dir <- "~/landcover_validation_shiny/"

land <- vect("land_simp_no_ant.geojson")

# User Interface
ui <- fluidPage(
  titlePanel("Global Point Labeller (for land cover validation data)"),
  sidebarLayout(
    sidebarPanel(
      h3("Start here:"),
      p("Click Generate Random Location to generate a random point on land."),
      p("Then click the land cover of the point based on the satellite imagery."),
      h4("Info:"),
      p("If its unclear, its better to just make a new point than guess (this is to generate a ground validation dataset for the internal land cover dataset)."),
      p("Sometimes the generated point will be broken, so just click Generate Random Location to make a new one."),
      p("If you make a mistake, you can click the Undo button to delete the previous [1] point."),
      "Point info:",
      verbatimTextOutput("ptText"),
      p(),
      "Point filename:",
      verbatimTextOutput("current_filename"),
      p(),
      "Previous point filename:",
      verbatimTextOutput("previous_filename"),
      p("Every point that has been classified will be saved and compiled into the land cover validation dataset."),
      p("The points are saved here:"),
      verbatimTextOutput("save_loc")
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      actionButton("generate", "Generate Random Location"),
      p(),
      actionButton("water", "Water"),
      actionButton("snow_ice", "Snow/Ice"),
      p(),
      actionButton("shrub_scrub", "Shrub/Scrub"),
      actionButton("flooded_vegetation", "Flooded Vegetation"),
      actionButton("trees", "Trees"),
      actionButton("grass_natural", "Grass/Natural"),
      p(),
      actionButton("bare_ground", "Bare Ground"),
      actionButton("built_up", "Built-Up"),
      actionButton("crops_not_trees", "Crops Not Trees"),
      actionButton("crops_trees", "Crops Trees"),
      p(),
      p("Made a mistake?"),
      # Delete the previous point
      actionButton("undo", "Undo last point")
    )
  )
)

# Server function
server <- function(input, output) {
  output$save_loc <- renderText(normalizePath(dir))

  pt <- reactiveValues(data = NULL)

  generate_filename <- function() {
    paste0(dir, "point_", stringi::stri_rand_strings(1, 15), ".geojson")
  }

  generate_point <- function() {
    # Save the previous filename
    previous_filename <<- current_filename
    output$previous_filename <- renderText(previous_filename)

    # Generate a new filename
    current_filename <<- generate_filename()
    output$current_filename <- renderText(current_filename)

    # Create terra spatial point
    pt$data <- terra::spatSample(land, 1)
    sf_point <- st_as_sf(pt$data)

    # Check if 'points' directory exists, if not, create it
    if (!dir.exists("points")) {
      dir.create("points")
    }

    output$map <- renderLeaflet({
      leaflet(width = "100%") %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addMarkers(data = sf_point)
    })
  }

  observeEvent(input$generate, {
    generate_point()
  })

  observeEvent(input$water, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "water"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$snow_ice, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "snow_ice"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$shrub_scrub, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "shrub_scrub"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$flooded_vegetation, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "flooded_vegetation"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$trees, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "trees"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$grass_natural, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "grass_natural"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$bare_ground, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "bare_ground"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$built_up, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "built_up"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$crops_not_trees, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "crops_not_trees"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$crops_trees, {
    # Save the generated point in a GeoJSON file
    pt$data$landcover <- "crops_trees"
    terra::writeVector(pt$data, filename = current_filename, filetype = "GeoJSON", overwrite = TRUE)
    generate_point()
  })

  observeEvent(input$undo, {
    # Delete the previous point
    file.remove(previous_filename)
    generate_point()
  })

  output$ptText <- renderPrint({
    pt$data
  })
}

# Running the app
shinyApp(ui = ui, server = server)
