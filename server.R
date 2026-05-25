
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  session = getDefaultReactiveDomain()
  
  pspl_status <- reactiveVal("")
  rv <- reactiveValues(r = NULL, r2 = NULL)
  
  observeEvent(input$species, {
    
    if (is.null(input$species) || input$species == "") return()
    
    pspl_status("Downloading raster...")
    species <- input$species
    pspl <- rast(paste0("https://www.for.gov.bc.ca/ftp/HTS/external/!publish/Provincial_Site_Productivity_Layer/version_9/Rasters/",
                        species,"_si_pspl_9.tif")
    )
    rv$r <- pspl   # or direct rast()
    
  })
  
  observeEvent(rv$r, {
    
    pspl_status("Downloading raster...")
    
    rv$r2 <- aggregate(rv$r, fact = 10)
    rv$r2 <- leaflet::projectRasterForLeaflet(rv$r2, method = "near")
    
  })
  
  output$pspl_status <- renderText({
    pspl_status()
  })
  
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      setView(-121.7476, 53.7267, 5) %>%
      setMaxBounds(lng1 = -142, lat1 = 46, lng2 = -112, lat2 = 62) %>%
      addTiles() %>%
      #addProviderTiles("Esri.WorldTopoMap", group = "WorldTopo") %>%
      addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
      #addLayersControl(
      #  baseGroups = c("WorldImagery", "WorldTopo"),
      #  options = layersControlOptions(collapsed = FALSE)
      #) %>%
      addScaleBar(position = "bottomright")
  })
  
  
  #observeEvent(input$species, {
  observeEvent(rv$r2, {
    
    
    proxy <- leafletProxy("map")
    
    # always remove previous raster + legend safely
    proxy %>%
      clearGroup("pspl") %>%
      clearGroup("imp") %>%
      clearControls()
    
    # if nothing selected → just show basemap
    if (is.null(input$species) || input$species == "") {
      return()
    }
    
    # palette
    pal <- colorNumeric(
      palette = hcl.colors(256, "viridis"),
      domain = values(rv$r2),
      na.color = "transparent"
    )
    
    # add raster layer
    proxy %>%
      addRasterImage(
        rv$r2,
        colors = pal,
        opacity = 0.9,
        group = "pspl"
      ) %>%
      addLegend(
        pal = pal,
        values = values(rv$r2, na.rm = TRUE),
        title = paste0(toupper(input$species), " PSPL v9"),
        position = "bottomright"
      )
    
    pspl_status("")
  })
  
  
  observeEvent(impShp(), {
    
    proxy <- leafletProxy("map")
    
    # remove old polygons
    proxy %>%
      clearGroup("imp")
    
    shp <- impShp()
    
    # guard clause
    if (is.null(shp) || nrow(shp) == 0) return()
    
    proxy %>%
      addPolygons(
        data = shp,
        group = "imp",
        color = "#FF0000",
        weight = 2,
        fillOpacity = 0.2
      )
  })
  
  #source(file.path("C:/Users/HYEWOO/OneDrive/Documents/FAIB/Documents/Worklog/250220_reactive.R"), local = TRUE) 
  
  
  entered_coord <- reactive({
    req(input$species, input$crs, input$latitude, input$longitude)
    
    df <- data.frame(x = input$longitude, y = input$latitude)
    
    crs_df <- case_when(input$crs == "EPSG:4326" ~ 4326,
                        input$crs == "EPSG:3005" ~ 3005,
                        input$crs == "BC Albers" ~ 3005,
                        input$crs == "UTM Zone 7" ~ 3154,
                        input$crs == "UTM Zone 8" ~ 3155,
                        input$crs == "UTM Zone 9" ~ 3156,
                        input$crs == "UTM Zone 10" ~ 3157,
                        input$crs == "UTM Zone 11" ~ 2955,
                        TRUE ~ 3005)
    
    coords_sf <- st_as_sf(df, coords = c("x", "y"), crs = crs_df)
    return(coords_sf)
  })
  
  
  entered_coord_leaf <- reactive({
    req(input$species, input$crs, input$latitude, input$longitude)
    
    coords_sf <- entered_coord()
    entered_coord_leaf <- st_transform(coords_sf, crs = 4326)
    return(entered_coord_leaf)
  })  
  
  
  entered_coord_pspl <- reactive({
    coords_sf <- entered_coord_leaf()
    pspl <- rv$r
    
    coord_pspl <- st_transform(coords_sf, crs = 3005)
    
    entered_coord_pspl <- extract(pspl, coord_pspl, sp = T)
    return(entered_coord_pspl)
    
  })
  
  observe({
    
    df <- df_pspl()
  
    coord <- if (
      !is.null(df) 
    ) {
      
      crs_df <- case_when(input$crs_batch == "EPSG:4326" ~ 4326,
                          input$crs_batch == "EPSG:3005" ~ 3005,
                          input$crs_batch == "BC Albers" ~ 3005,
                          input$crs_batch == "UTM Zone 7" ~ 3154,
                          input$crs_batch == "UTM Zone 8" ~ 3155,
                          input$crs_batch == "UTM Zone 9" ~ 3156,
                          input$crs_batch == "UTM Zone 10" ~ 3157,
                          input$crs_batch == "UTM Zone 11" ~ 2955,
                          TRUE ~ 3005)
      
      df_sf <- st_as_sf(df,  coords = c("x", "y"), crs = crs_df)
    df_leaf <- st_transform(df_sf, crs = 4326)
    df_leaf <- df_leaf %>%
      dplyr::rename(SI = dplyr::matches("si_pspl_9")) %>%
      mutate(SI = round(SI, 1))
    df_leaf
    } else {
      merge(entered_coord_leaf(), entered_coord_pspl()) %>%
        dplyr::rename(SI = dplyr::matches("si_pspl_9")) %>%
        mutate(SI = round(SI, 1))
    }
    
    proxy <- leafletProxy("map")
    
    # always clear previous coordinate marker
    proxy %>%
      clearGroup("coords")
    
    # stop here if unchecked
    if (!isTRUE(input$show_coords)) {
      return()
    }
    
    # add marker
    proxy %>%
      addMarkers(
        data = coord,
        group = "coords",
        popup =  ~as.character(SI)
      )
  })
  
  
  output$preview <- renderTable({
    
    coord_pspl <- entered_coord_pspl()
    
    df = data.frame("x" = paste0(input$longitude),
                    "y" = paste0(input$latitude),
                    "CRS" = paste0(input$crs),
                    "Species" = paste0(toupper(input$species)),
                    "PSPLv9_SI" = paste0(round(coord_pspl[1,2], 1))
    )
    return(df)
  })
  
  preview_data <- reactive({
    coord_pspl <- entered_coord_pspl()
    
    df = data.frame("x" = paste0(input$longitude),
                    "y" = paste0(input$latitude),
                    "CRS" = paste0(input$crs),
                    "Species" = paste0(toupper(input$species)),
                    "PSPLv9_SI" = paste0(round(coord_pspl[1,2], 1))
    )
    return(df)
  })
  
  
  output$preview_ui <- renderUI({
    
    req(preview_data()) 
    
    box(
      title = "PSPL from entered coordinate",
      width = 6,
      tableOutput("preview")
    )
  })
  
  
  #source(file.path("C:/Users/HYEWOO/OneDrive/Documents/FAIB/Documents/Worklog/250306_mod.R"), local = TRUE) 
  
  
  # Reactive block to read the file based on the extension
  uploaded_data <- reactive({
    req(input$upload) # Stop execution if no file is uploaded
    
    ext <- tools::file_ext(input$upload$name)
    
    switch(ext,
           csv = fread(input$upload$datapath),
           txt = read.delim(input$upload$datapath),
           geojson = read_sf(input$upload$datapath),
           validate("Invalid file type; please upload a .csv, .txt, or .geojson file.")
    )
  })
  
  #Observe file being selected
  observeEvent(input$upload, {
    #session = getDefaultReactiveDomain(),
    
    #Store loaded data in reactive
    uploaded <- uploaded_data()
    
    selectif <- function(varname){
      ifelse(varname %in% colnames(uploaded), varname, "")
    }
    
    updateSelectInput(session, inputId = 'idcol', label = 'Select ID column:', 
                      choices  = c("Choose" = "", colnames(uploaded)), selected = selectif("ID"))
    updateSelectInput(session, inputId = 'xcol', label = 'Select x coord column:', 
                      choices  = c("Choose" = "", colnames(uploaded)), selected = selectif("Longitude"))
    updateSelectInput(session, inputId = 'ycol', label = 'Select y coord column:', 
                      choices  = c("Choose" = "", colnames(uploaded)), selected = selectif("Latitude"))
  })
  
  
  
  df_pspl <- reactive({
    req(input$upload, input$idcol, input$xcol, input$ycol, input$crs_batch)
    
    #Store loaded data in reactive
    df <- uploaded_data()
    
    pspl <- rv$r
    
    crs_df <- case_when(input$crs_batch == "EPSG:4326" ~ 4326,
                        input$crs_batch == "EPSG:3005" ~ 3005,
                        input$crs_batch == "BC Albers" ~ 3005,
                        input$crs_batch == "UTM Zone 7" ~ 3154,
                        input$crs_batch == "UTM Zone 8" ~ 3155,
                        input$crs_batch == "UTM Zone 9" ~ 3156,
                        input$crs_batch == "UTM Zone 10" ~ 3157,
                        input$crs_batch == "UTM Zone 11" ~ 2955,
                        TRUE ~ 3005)
    
    df <- df %>% select(input$idcol, x = input$xcol, y = input$ycol)
    
    df$ID <- 1:nrow(df)
    
    df_sf <- st_as_sf(df, coords = c("x", "y"), crs = crs_df)
    
    df_sf_conv <- st_transform(df_sf, crs = 3005)
    
    df_si <- extract(pspl, df_sf_conv, sp = T)
    
    df <- merge(df, df_si, by = "ID")
    
    df <- setDT(df)[, ID := NULL]
    
    return(df)
  })
  
  
  output$uploaded_preview <- renderTable({
    df <- df_pspl()
    return(head(df))
  })
  
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("PSPLv9_", input$species, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(df_pspl(), file)
    }
  )

  output$uploaded_preview_ui <- renderUI({
    
    req(df_pspl())  # replace with your reactive
    
    box(
      title = "PSPL from uploaded file",
      width = 6,
      tableOutput("uploaded_preview"),
      downloadButton(
        "download",
        "Download output",
        style = "color: #fff; background-color: #27ae60;
               border-color: #fff;padding: 5px 14px;margin: 5px;"
      )
    )
  })
  
  # Modal for labeling the drawn polygons
  warningModal = modalDialog(
    title = "Important message",
    "Shapefile not valid")
  
  #Import Shapefile as df
  impShp <- reactive({
    shpValid <- FALSE
    outShp <- NULL
    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
    if (!is.null(input$filemap)){
      shpValid <- TRUE
      shpdf <- input$filemap
      tempdirname <- dirname(shpdf$datapath[1])
      fileList <- list()
      i <- 1
      for (file in shpdf$datapath) {
        fileExt <- strsplit(file, "\\.")
        fileExt <-fileExt[[1]][length(fileExt[[1]])]
        fileList[[i]] <- fileExt
        i <- i + 1
        if (fileExt %in% c("shp","dbf", "shx", "sbn", "sbx", "prj", "xml","cpg"))
        {
          print ("shp ext is good")
          }
        else{
          shpValid <- FALSE
          showModal(warningModal)}
      }
      
      if(!"shp" %in% fileList | !"shp" %in% fileList | !"dbf" %in% fileList | !"shx" %in% fileList )
      { shpValid <- FALSE
      showModal(warningModal)}
      
      if (shpValid){
        # Rename files
        for(i in 1:nrow(shpdf)){
          file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
        }
        tryCatch(
          {outShp <-  st_transform(st_read(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/")), 4326)}
          ,
          error=function(cond) {
            shpValid <- FALSE
            showModal(warningModal)
            outShp <- NULL
            message("Here's the original error message:")
            
          }
          ,
          finally ={print ("shape done")}
        )
      }
      
    }
    if (!shpValid) {
      outShp = NULL
      
    }
    else{outShp}
    outShp
  })
  
  
  poly_pspl <- reactive({
    
    req(input$filemap)
    
    poly1 <- impShp()
    
    pspl <- rv$r
    
    poly1_conv <- st_transform(poly1, crs = 3005)
    
    poly_si <- extract(pspl, poly1_conv, fun = mean, na.rm = TRUE)
    
    return(poly_si)
  })
  
  
  output$polygon_preview <- renderTable({
    df <- poly_pspl()
  })
  
  output$polygon_preview_ui <- renderUI({
    
    req(poly_pspl())  # or nrow(...) > 0 logic
    
    box(
      title = "Mean PSPL from uploaded polygon",
      width = 6,
      tableOutput("polygon_preview")
    )
  })
  
}


