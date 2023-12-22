# Load required libraries
pacman::p_load(shiny, tidyverse, ggplot2, leaflet,sf,rnaturalearth,httr)

# Load your data (replace this with your actual data loading code)
populations_LUT <- read_csv("results/populations_LUT.csv") %>%
  dplyr::rename(Population = ESA_Population_Name)

indicators <- read_csv("results/indicators_by_NOAA_pop.csv") %>%
  dplyr::rename(Population = ESA_Population_Name) %>%
  left_join(populations_LUT)


state_map <- ne_states (country = 'United States of America', returnclass = 'sf')%>% 
  filter (name %in% c('Washington','Oregon'))

# The input file geodatabase
fgdb <- "spatial_data/WCR_Salmon_Steelhead_gdb_2015.gdb"
if (file.exists(fgdb)) {
  print("Geodatabase already downloaded!")
} else {
  dir_path <- here::here("spatial_data")
  if (!dir.exists(dir_path)) {
    # If not, create the directory
    dir.create(dir_path, recursive = TRUE)
  }
  print("Attempting to download Geodatabase...may take a few mins!")
  url<-"https://www.webapps.nwfsc.noaa.gov/portal/sharing/rest/content/items/097239ff29b44a8b87acc048f0363229/data"
  response <- GET(url, timeout(600))
  content <- content(response, as = "raw")
  writeBin(content, "spatial_data/WCR_Salmon_Steelhead_gdb_2015.zip")
  #download.file(, destfile = "data/WCR_Salmon_Steelhead_gdb_2015.zip", mode = "wb",timeout = 300)
  unzip("spatial_data/WCR_Salmon_Steelhead_gdb_2015.zip", exdir = "spatial_data")
}

noaa_polygons<-st_read(fgdb, layer = "fish")%>%
  st_set_crs(st_crs("+proj=longlat +datum=NAD83 +units=m"))

FallCoho <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, coho (Lower Columbia River ESU)",
      "Salmon, coho (Lower Columbia River ESU) - Outside legal area"
    ),
    SPECIES == "CO" & !is.na(NWFSC_POP_ID)
  ) %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

WinterSteelhead <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Steelhead (Lower Columbia River DPS)",
      "Steelhead (Lower Columbia River DPS) - Outside legal area"
      #could add in Steelhead (Middle Columbia River DPS) and su wi for klickitat and white salmon
    )
  ) %>%
  filter(SPECIES == "ST" & RUN_TIMING == "wi") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  bind_rows(noaa_polygons%>%
              filter(
                DPS %in% c(
                  "Salmon, coho (Lower Columbia River ESU)",
                  "Salmon, coho (Lower Columbia River ESU) - Outside legal area"
                ),
                SPECIES == "CO" & NWFSC_POP_ID%in%c(134,135,140)
              ) %>%
              group_by(NWFSC_POP_ID)%>%
              mutate(NWFSC_POP_ID=ifelse(NWFSC_POP_ID==135,9991,NWFSC_POP_ID),#Grays
                     NWFSC_POP_ID=ifelse(NWFSC_POP_ID==134,9992,NWFSC_POP_ID),#elochoman
                     NWFSC_POP_ID=ifelse(NWFSC_POP_ID==140,9993,NWFSC_POP_ID),#Mill -abernathy-germany
              )%>%
              summarise()
  )

SummerSteelhead <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Steelhead (Lower Columbia River DPS)",
      "Steelhead (Lower Columbia River DPS) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "ST" & RUN_TIMING == "su") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

SpringChinook <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, Chinook (Lower Columbia River ESU)",
      "Salmon, Chinook (Lower Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CK" & RUN_TIMING %in% c("sp","ss")) %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

FallChinook <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, Chinook (Lower Columbia River ESU)",
      "Salmon, Chinook (Lower Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CK" & RUN_TIMING == "fa") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

LateFallChinook <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, Chinook (Lower Columbia River ESU)",
      "Salmon, Chinook (Lower Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CK" & RUN_TIMING == "lf") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

FallChum <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, chum (Columbia River ESU)",
      "Salmon, chum (Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CM" & RUN_TIMING == "fa") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

SummerChum <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, chum (Columbia River ESU)",
      "Salmon, chum (Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CM" & RUN_TIMING == "su") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

server = function(input, output, session) {
  # Set initial values to NULL
  updateSelectInput(session, "Species", selected = "None")
  updateSelectInput(session, "Run", selected = "None")
  updateSelectInput(session, "Population", selected = "None")
  
  # Reactive values
  subset_data_reactive <- reactive({
    indicators %>%
      filter(
        if (input$Species != "None") Species == input$Species else TRUE,
        if (input$Run != "None") Run == input$Run else TRUE,
        if (input$Population != "None") Population == input$Population else TRUE
      ) %>%
      dplyr::select(
        Brood_Year, Species, Run, Population, NMFS_POPID, NOSA, TSA,
        pNOB, pHOS, pNI, Total_Releases
      ) %>%
      pivot_longer(
        values_to = "value",
        names_to = "indicator",
        cols = c(NOSA, TSA, pHOS, pNOB, pNI, Total_Releases)
      ) %>%
      mutate(indicator = factor(indicator, levels = c("NOSA", "TSA", "pHOS", "pNOB", "pNI", "Total_Releases")))
  })
  
  observe({
    species_population_filtered <- indicators %>%
      filter(if (input$Species != "None") Species == input$Species else TRUE) %>%
      filter(if (input$Population != "None") Population == input$Population else TRUE)
    updateSelectInput(session, "Run", choices = c("None", unique(species_population_filtered$Run)), selected = if (input$Run != "None") input$Run else "None")
  })
  
  observe({
    run_population_filtered <- indicators %>%
      filter(if (input$Run != "None") Run == input$Run else TRUE) %>%
      filter(if (input$Population != "None") Population == input$Population else TRUE)
    updateSelectInput(session, "Species", choices = c("None", unique(run_population_filtered$Species)), selected = if (input$Species != "None") input$Species else "None")
  })
  
  observe({
    run_species_filtered <- indicators %>%
      filter(if (input$Run != "None") Run == input$Run else TRUE) %>%
      filter(if (input$Species != "None") Species == input$Species else TRUE)
    updateSelectInput(session, "Population", choices = c("None", unique(run_species_filtered$Population)), selected = if (input$Population != "None") input$Population else "None")
  })
  
  #map
  # Render Coho Plot
  output$range_map <- renderPlot({
    if (input$Population != "None") {
      Run <- tools::toTitleCase(subset_data_reactive()$Run[1])
      Species <- tools::toTitleCase(subset_data_reactive()$Species[1])
      
      range_dat <- get(paste0(Run, Species))
      
      range_map <- ggplot() +
        geom_sf(data = state_map, color = "black", fill = "lightgrey") + # Washington state map
        geom_sf(data = range_dat, mapping = aes(geometry = SHAPE), fill = "lightblue") +
        geom_sf(data = range_dat, mapping = aes(geometry = SHAPE), fill = "lightblue") +
        geom_sf(data = range_dat %>%
                  filter(NWFSC_POP_ID == subset_data_reactive()$NMFS_POPID[1]),
                mapping = aes(geometry = SHAPE), fill = "darkblue") +
        geom_sf(data = state_map, color = "black", fill = NA) + # Washington state map
        #geom_sf(data=release_locs,mapping=aes(geometry=geometry),size=3)+
        labs(title = paste0(Run, " ", Species, " Populations"), fill = NA) +
        ylim(44.75, 46.8) +
        xlim(-124, -121) +
        theme(
          plot.title = element_text(size = 18),
          text = element_text(size = 12),  # Adjust the main text size
          axis.text = element_text(size = 12)  # Adjust the axis text size
          # You can add more elements like legend.text, title, etc., if needed
        ) +
        theme_minimal()
      
      print(range_map)
    }
  })
  
  # Render plot
  output$plot <- renderPlot({
    cat("Selected Species:", input$Species, "\n")
    cat("Selected Run:", input$Run, "\n")
    cat("Selected Population:", input$Population, "\n")
    
    # Plotting code
    filtered_data <- subset_data_reactive()
    # Check the number of unique populations in the filtered data
    unique_populations <- unique(filtered_data$Population)
    
    if (length(unique_populations) == 1) {
      facet_limits <- filtered_data %>%
        group_by(indicator, Species) %>%
        mutate(
          minBY = min(Brood_Year),
          maxBY = max(Brood_Year),
          min = 0,
          max = max(value, na.rm = TRUE, nan.rm = TRUE)
        ) %>%
        distinct(minBY, maxBY, min, max) %>%
        pivot_longer(cols = c(minBY, maxBY), values_to = "Brood_Year", names_to = "type") %>%
        pivot_longer(cols = c(min, max), values_to = "value", names_to = "type2") %>%
        dplyr::select(-c(type, type2))
      
      current_plot <- ggplot(filtered_data, aes(x = Brood_Year, y = value, color = Species)) +
        geom_line() +
        geom_point() +
        ylim(0, NA) +
        theme_bw() +
        theme() +
        facet_wrap(~indicator, nrow = 6, scales = "free_y") +
        ggtitle(filtered_data$population[1]) +
        geom_blank(data = facet_limits, aes(x = Brood_Year, y = value)) +
        theme(
          plot.title = element_text(size = 18),
          text = element_text(size = 16),  # Adjust the main text size
          axis.text = element_text(size = 14)  # Adjust the axis text size
          # You can add more elements like legend.text, title, etc., if needed
        )
      
      print(current_plot)
    } else {
      print("Please select only one population to generate the plot.")
    }
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("salmon_indicator_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(subset_data_reactive())
      write.csv(subset_data_reactive(), file, row.names = FALSE)
    }
  )
}
