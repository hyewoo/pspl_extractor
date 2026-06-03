
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  session = getDefaultReactiveDomain()
  
  pspl_status <- reactiveVal("")
  
  rv <- reactiveValues(
    r = NULL,
    r2 = NULL,
    raster_cache = list(),
    agg_cache = list()
  )
  
  observeEvent(input$species, {
    
    req(input$species)
    species <- input$species
    
    # -------------------------
    # 1. GET / CACHE RAW RASTER
    # -------------------------
    if (is.null(rv$raster_cache[[species]])) {
      
      withProgress(message = "Downloading raster...", value = 0, {
        
        incProgress(0.3, detail = "Building URL...")
        
        url <- paste0(
          "https://www.for.gov.bc.ca/ftp/HTS/external/!publish/Provincial_Site_Productivity_Layer/version_9/Rasters/",
          species, "_si_pspl_9.tif"
        )
        
        incProgress(0.5, detail = "Loading raster...")
        
        rv$raster_cache[[species]] <- terra::rast(url)
        
        incProgress(0.6, detail = "Done")
      })
    }
    
    rv$r <- rv$raster_cache[[species]]
    
    # -------------------------
    # 2. GET / CACHE AGGREGATED
    # -------------------------
    if (is.null(rv$agg_cache[[species]])) {
      
      withProgress(message = "Aggregating raster...", value = 0, {
        
        incProgress(0.5, detail = "Aggregating...")
        
        rv$agg_cache[[species]] <- leaflet::projectRasterForLeaflet(terra::aggregate(rv$r, fact = 10), method = "near")
        
        incProgress(0.8, detail = "Done")
      })
    }
    
    rv$r2 <- rv$agg_cache[[species]]
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
      ) %>%
      addControl(
        html = HTML(
          paste0(
            "<div style='
        background: rgba(255,255,255,0.85);
            padding: 8px 12px;
            font-size: 16px;
            max-width: 250px;
      border: none;
      box-shadow: none;
          border-radius: 6px;
        font-weight: bold;'>
        Species: ", toupper(input$species), "
      </div>"
          )
        ),
        position = "topright",
        className = "info-text"
      )
    
    #pspl_status("")
  })
  
  
  observe({
    
    req(input$map_zoom)
    
    proxy <- leafletProxy("map")
    
    proxy %>%
      removeControl(layerId = "resample_note")
    
    if (input$map_zoom >= 9) {
      
      proxy %>%
        addControl(
          html = HTML(
            "<div style='
             background: rgba(255,255,255,0.5);
             padding: 8px 12px;
             font-size: 12px;
             max-width: 250px;
       border: none;
       box-shadow: none;
           border-radius: 6px;'>
            PSPL layers displayed on the map are resampled to a coarser resolution to
            improve performance and reduce loading time. All PSPL value extractions
            and downloads are derived from the original-resolution data and are
            not affected by the display resampling.
          </div>"
          ),
          position = "topright",
          layerId = "resample_note"
        )
      
    }
    
  })
  
  
  observeEvent(impShp(), {
    
    proxy <- leafletProxy("map")
    
    # remove old polygons
    proxy %>%
      clearGroup("imp")
    
    shp <- impShp()
    
    # guard clause
    if (is.null(shp) || nrow(shp) == 0) return()
    
    shp <- cbind(shp, poly_pspl())%>%
      dplyr::rename(SI = dplyr::matches("si_pspl_9")) %>%
      dplyr::mutate(SI = round(SI, 1))
    
    # Create popup text
    shp$popup_text <- paste0(
      "<b>ID:</b> ", shp$ID, ", <b>SI: </b> ", shp$SI
    )
    
    proxy %>%
      addPolygons(
        data = shp,
        group = "imp",
        color = "#FF0000",
        weight = 2,
        fillOpacity = 0.2,
        popup = ~ popup_text
      )
  })
  
  #source(file.path("C:/Users/HYEWOO/OneDrive/Documents/FAIB/Documents/Worklog/250220_reactive.R"), local = TRUE) 
  
  
  entered_coord <- reactive({
    req(input$species, input$crs, input$latitude, input$longitude)
    
    df <- data.frame(x = input$longitude, y = input$latitude)
    
    crs_df <- get_crs(input$crs)
    
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
    
    req(coords_sf)   
    req(rv$r)      
    
    coord_pspl <- st_transform(coords_sf, crs = 3005)
    
    withProgress(message = "Extracting values...", value = 0, {
      
      incProgress(0.6)
    
    entered_coord_pspl <- terra::extract(pspl, coord_pspl)
    
    incProgress(0.8)
    })
    return(entered_coord_pspl)
  })
  
  observe({
    
    pspl1 <- entered_coord_pspl()
    leaf1 <- entered_coord_leaf()
    
    req(pspl1, leaf1)
    if (NROW(pspl1) == 0) return()
    
    coord <- cbind(leaf1, pspl1) %>%
      dplyr::rename(SI = dplyr::matches("si_pspl_9")) %>%
      dplyr::mutate(SI = round(SI, 1))
    
    proxy <- leafletProxy("map") %>% clearGroup("coords")
    
    if (!isTRUE(input$show_coords)) return()
    
    proxy %>% addMarkers(
      data = coord,
      group = "coords",
      popup = ~as.character(SI)
    )
  })
  
  observe({
    
    df1 <- df_pspl()
    
    req(df1)
    if (NROW(df1) == 0) return()
    
    crs_df <- get_crs(input$crs_batch)
    
    coord <- st_as_sf(df1, coords = c("x", "y"), crs = crs_df) %>%
      st_transform(4326) %>%
      dplyr::rename(SI = dplyr::matches("si_pspl_9")) %>%
      dplyr::mutate(SI = round(SI, 1))
    
    proxy <- leafletProxy("map") %>% clearGroup("coords")
    
    if (!isTRUE(input$show_coords)) return()
    
    proxy %>% addMarkers(
      data = coord,
      group = "coords",
      popup = ~as.character(SI)
    )
  })
  
  output$preview <- renderTable({
    preview_data()
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
      title =  tags$span(
        style = "color:black; font-weight:700; font-size:16px;",
        "PSPL value of entered coordinate"
      ),
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
    req(input$upload, input$idcol, input$xcol, input$ycol, input$crs_batch, rv$r)
    
    #Store loaded data in reactive
    df <- uploaded_data()
    
    pspl <- rv$r
    
    crs_df <- get_crs(input$crs_batch)
    
    df <- df %>% dplyr::select(input$idcol, x = input$xcol, y = input$ycol) 
    
    df$ID <- 1:nrow(df)
    
    df_sf <- st_as_sf(df, coords = c("x", "y"), crs = crs_df)
    
    df_sf_conv <- st_transform(df_sf, crs = 3005)
    
    withProgress(message = "Extracting values...", value = 0, {
      
      incProgress(0.4)
      
    df_si <- terra::extract(pspl, df_sf_conv)
    
    df <- merge(df, df_si, by = "ID")
    
    incProgress(0.8)
    
    df <- setDT(df)[, ID := NULL]
    
    last_col <- names(df)[ncol(df)]
    
    df[, (last_col) := round(get(last_col), 1)]
    })
    return(df)
  })
  
  
  output$uploaded_preview <- renderDT({
    
    df <- df_pspl() %>% dplyr::select(-x, -y)
    return(df)
  })
  
  
  output$download1 <- downloadHandler(
    
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
      title =  tags$span(
        style = "color:black; font-weight:700; font-size:16px;",
        "PSPL of batch coordinates"
      ),
      width = 6,
      DTOutput("uploaded_preview"),
      downloadButton(
        "download1",
        "Download output",
        style = "color: #fff; background-color: #27ae60;
               border-color: #fff;padding: 5px 14px;margin: 5px;"
      )
    )
  })
  
  
  output$upload_ui <- renderUI({
    fileInput("upload", "Upload a file (.csv, .txt, .geojson)",
              accept = c(".csv", ".txt", ".geojson"))
  })
  
  
  observeEvent(input$clear_batch, {
    
    output$upload_ui <- renderUI({
      fileInput("upload", "Upload a file (.csv, .txt, .geojson)",
                accept = c(".csv", ".txt", ".geojson"))
    })
    
    # reset selects
    updateSelectInput(session, "idcol", choices = "", selected = "")
    updateSelectInput(session, "xcol", choices = "", selected = "")
    updateSelectInput(session, "ycol", choices = "", selected = "")
    updateSelectInput(session, "crs_batch", selected = "")
    
    leafletProxy("map") %>%
      clearGroup("coords")
  })
  
  
  observeEvent(input$clear_single, {
    
    # reset selects
    updateSelectInput(session, "crs", selected = "")
    updateNumericInput(session, "longitude", value = NA)
    updateNumericInput(session, "latitude", value = NA)
    
  })
  
  output$file_ui <- renderUI({
    fileInput("filemap", "Upload Shapefile", multiple = TRUE)
  })
  
  
  # Modal for labeling the drawn polygons
  warningModal = modalDialog(
    title = "Important message",
    "Shapefile not valid")
  
  #Import Shapefile as df
  impShp <- reactive({
    
    if (is.null(values$upload_state)) {
      
      return(NULL)
      
    } else if (values$upload_state == 'uploaded') {
      
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
      
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
    
  })
  
  
  poly_pspl <- reactive({
    
    req(input$filemap, impShp())
    
    poly1 <- impShp()
    
    pspl <- rv$r
    
    poly1_conv <- st_transform(poly1, crs = 3005)
    
    withProgress(message = "Extracting values...", value = 0, {
      
      incProgress(0.2)
      
    poly_si <- terra::extract(pspl, poly1_conv, fun = mean, na.rm = TRUE)
    
    incProgress(0.8)
    
    })
    
    return(poly_si)
  })
  
  
  
  output$download2 <- downloadHandler(
    
    filename = function() {
      paste0("PSPLv9_", input$species, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(poly_pspl(), file)
    }
  )
  
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$filemap, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$clear_poly, {
    values$upload_state <- 'reset'
  })
  
  
  observeEvent(input$clear_poly, {
    
    output$file_ui <- renderUI({
      fileInput("filemap", "Upload Shapefile", multiple = TRUE)
    })
    
    leafletProxy("map") %>%
      clearGroup("imp")
    
  })
  
  output$polygon_preview <- renderDT({
    
    req(input$filemap)
    req(poly_pspl())
    
    
    df <- poly_pspl()
    
    df <- setDT(df)
    
    last_col <- names(df)[ncol(df)]
    
    df[, (last_col) := round(get(last_col), 1)]
  })
  
  output$polygon_preview_ui <- renderUI({
    req(poly_pspl())
    
    box(
      title =  tags$span(
        style = "color:black; font-weight:700; font-size:16px;",
        "Mean PSPL from uploaded polygon"
      ),
      width = 6,
      DTOutput("polygon_preview"),
      downloadButton(
        "download2",
        "Download output",
        style = "color: #fff; background-color: #27ae60;
               border-color: #fff;padding: 5px 14px;margin: 5px;"
      )
    )
  })
  
  output$about_page <- renderUI({
    
    includeHTML("about.html")
  })
  
}
