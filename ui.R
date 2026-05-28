
ui <- dashboardPage(
  
  dashboardHeader(
    title = "PSPL v9 Extractor",
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320, 
    
    sidebarMenu(
      id = "tabs",
      
      HTML(paste0(
        "<a href='https://www2.gov.bc.ca/gov/content/governments/organizational-structure/ministries-organizations/ministries/forests' target='_blank'>
          <img style='display:block; margin-left:auto; margin-right:auto;' 
          src='BC_FOR_V_RGB_rev.png' width='163'></a>"
      )),
      tags$head(
        tags$style(HTML("
    .box { font-size: 14px;}
    .small-box { font-size: 16px;font-color: #007bff;  background-color: #3D474C;  !important; }
    .main-sidebar .box {
          background-color: #3D474C; 
          border-top-color: white;
          color: white;  
    }
  "))
      ),
      
      menuItem(text = "About", icon = icon("circle-info"), tabName = "about"),
      menuItem(text = "Application", icon = icon("map"), tabName = "app",
               selected = TRUE),
      
      tags$div(
          style = "
    width: 100%;
    box-sizing: border-box;

    margin-top: 3px;
    margin-bottom: 3px;
    padding: 6px 3px;

    border: 1px solid #fcba19;
    border-radius: 6px;
    background-color: #3D474C;

    /* important: prevent sidebar overflow */
    max-width: 100%;
    overflow: visible;
    
  ",
      selectizeInput(
        inputId = "species",
        label = "Select Species:",
        selected = NULL,
        choices = c(
          "Choose" = "",
          "AT" = "At", "BA" = "Ba",
          "BL" = "Bl", "CW" = "Cw",
          "EP" = "Ep", "FD" = "Fd",
          "HW" = "Hw", "LW" = "Lw",
          "PL" = "Pl", "PY" = "Py",
          "SB" = "Sb", "SE" = "Se",
          "SS" = "Ss", "SW" = "Sw"
        )
      )
      ),
      br(),
      tags$div(
        style = "margin-top:10px; font-size:13px; color:#2C7FB8;",
        textOutput("pspl_status")
      ),
      br(),
      
      
      tags$div(
        id = "coord_single",
        class = "box box-solid box-default collapsed-box",
        style = "width: 100%;
    border-color:#3D474C;",
        
        # header
        tags$div(
          class = "box-header with-border",
          style = "background-color:#3D474C; color:white; cursor:pointer;",
          
          tags$h3(
            class = "box-title",
            style = "font-size:14px; font-weight:bold;",
            "Enter Coordinate"
          ),
          
          # collapse button
          tags$div(
            class = "box-tools pull-right",
            
            tags$button(
              type = "button",
              class = "btn btn-box-tool",
              `data-widget` = "collapse",
              
              icon("plus"),
              style = "color:white;"
            )
          )
        ),
        
        # body
        tags$div(
          class = "box-body",
          style = "display:none;",
          
          selectInput(
            "crs",
            "CRS:",
            choices = c(
              "Choose" = "",
              "BC Albers",
              "EPSG:3005",
              "EPSG:4326",
              "UTM Zone 7N",
              "UTM Zone 8N",
              "UTM Zone 9N",
              "UTM Zone 10N",
              "UTM Zone 11N"
            ),
            selected = "Choose"
          ),
          
          numericInput(
            "longitude",
            "X or Longitude:",
            value = NA
          ),
          
          numericInput(
            "latitude",
            "Y or Latitude:",
            value = NA
          ),
          
          tags$div(
            style = "display:flex; justify-content:flex-end; margin-top:8px;",
            
            actionButton(
              "clear_single",
              "Clear inputs",
              icon = icon("trash"),
              style = "color:#3D474C; background-color:#F4F4F4; border:none;"
            )
          )
        )
      ),
      
      tags$div(
        id = "box_batch",
        class = "box box-solid box-default collapsed-box",
        style = "width: 100%;
    border-color:#3D474C;",
        
        # header
        tags$div(
          class = "box-header with-border",
          style = "background-color:#3D474C; color:white; cursor:pointer;",
          
          tags$h3(
            class = "box-title",
            style = "font-size:14px; font-weight:bold;",
            "Upload Batch Coordinates"
          ),
          
          # collapse button
          tags$div(
            class = "box-tools pull-right",
            
            tags$button(
              type = "button",
              class = "btn btn-box-tool",
              `data-widget` = "collapse",
              
              icon("plus"),
              style = "color:white;"
            )
          )
        ),
        
        # body
        tags$div(
          class = "box-body",
          style = "display:none;",
          
          #fileInput(
          #  "upload",
          #  "Upload a file (.csv, .txt, .geojson)",
          #  accept = c(".csv", ".txt", ".geojson")
          #),
          uiOutput("upload_ui"),
          
          tags$div(
            style = "margin-top: -35px; margin-bottom: 0px;",
            checkboxInput(
              "header",
              label = "Column headers",
              value = TRUE
            )
          ),
          
          selectInput(
            "idcol",
            "Select ID column:",
            ""
          ),
          
          selectInput(
            "xcol",
            "Select x coord column:",
            ""
          ),
          
          selectInput(
            "ycol",
            "Select y coord column:",
            ""
          ),
          
          selectInput(
            "crs_batch",
            "CRS:",
            choices = c(
              "Choose" = "",
              "BC Albers",
              "EPSG:3005",
              "EPSG:4326",
              "UTM Zone 7N",
              "UTM Zone 8N",
              "UTM Zone 9N",
              "UTM Zone 10N",
              "UTM Zone 11N"
            ),
            selected = "Choose"
          ),
          
          tags$div(
            style = "display:flex; justify-content:flex-end; margin-top:8px;",
            
            actionButton(
              "clear_batch",
              "Clear inputs",
              icon = icon("trash"),
              style = "color:#3D474C; background-color:#F4F4F4; border:none;"
            )
          )
        )
      ),
      
      #checkboxInput(
      #  "show_coords",
      #  label = "Display on map",
      #  value = FALSE
      #),
      
      tags$div(
        style = "margin-top: -30px; margin-bottom: 0px;",
        checkboxInput(
          "show_coords",
          label = "Display on map",
          value = FALSE
        )
      ),
      
      br(),
      
      tags$div(
        id = "polygon",
        class = "box box-solid box-default collapsed-box",
        style = "width: 100%;
    border-color:#3D474C;",
        
        # header
        tags$div(
          class = "box-header with-border",
          style = "background-color:#3D474C; color:white; cursor:pointer;",
          
          tags$h3(
            class = "box-title",
            style = "font-size:14px; font-weight:bold;",
            "Upload Polygon"
          ),
          
          # collapse button
          tags$div(
            class = "box-tools pull-right",
            
            tags$button(
              type = "button",
              class = "btn btn-box-tool",
              `data-widget` = "collapse",
              
              icon("plus"),
              style = "color:white;"
            )
          )
        ),
        
        # body
        tags$div(
          class = "box-body",
          style = "display:none;",
          
          #fileInput(placeholder = "shp,dbf,shx", #width = 185 ,
          #          inputId = "filemap", 
          #          label = "Upload Shapefile", multiple = TRUE, 
          #          accept = c("shp","dbf", "shx", "sbn", "sbx", "prj", "xml","cpg")),
          uiOutput("file_ui"),
          
          tags$div(
            style = "display:flex; justify-content:flex-end; margin-top:8px;",
            
            actionButton(
              "clear_poly",
              "Clear inputs",
              icon = icon("trash"),
              style = "color:#3D474C; background-color:#F4F4F4; border:none;"
            )
          )
        )
      ),
      
      br(),
      
      #menuItem(text = "About", icon = icon("circle-info"), tabName = "about"),
      menuItem(text = "Example", icon = icon("glasses"), tabName = "example")#,
      #menuItem(text = "Read Me", icon = icon("book"))
    )
  ),
  
  
  dashboardBody(
    
    includeCSS("bcgov_01.css"),
    #use_waiter(),
    tabItems(
      
      tabItem(
        tabName = "app",
        
    fluidRow(
      box(
        id = "mapbox",
        width = 12,
        div(
          id = "map_container",
          leafletOutput("map", height = 550)
        )
      ),
      
      uiOutput("preview_ui"),
      uiOutput("uploaded_preview_ui"),
      uiOutput("polygon_preview_ui"),
      br()
    ),
    
    br(),
    br(),
      
    ),
    
    
    tabItem(
      tabName = "about",
      #htmlOutput("about_page")
      tags$iframe(
        src = "about.html",
        style = "
    width: 100%;
    height: 900px;
    border: none;
    background-color: white;
  "
      )
    ),
    
    tabItem(
      tabName = "example",
      h3("Example page goes here")
    )
    ),
    
 #   tags$footer(
 #     class = "main-footer",
 #     style = "
 #   background-color: #036;
 #   color: #fff;
 #   border-top: 2px solid #fcba19;
 #   position:absolute;
 #             bottom:0;
 #             width:100%;
 #             height:50px;   /* Height of the footer */
 #             
 #             padding: 10px;
 #             
 #             z-index: 1000;
 #             left: 0px;
 # ",
 #     div(
 #       style = "display: flex;
 #     justify-content: center;
 #     align-items: center;
 #     width: 100%;
 #     white-space: nowrap;
 #     font-size: 16px;",
 #       HTML(
 #         "<strong>PSPL Extractor</strong> | Provincial Site Productivity Layer v9"
 #       )
 #     )
 #   )
    
    tags$footer(
      class = "main-footer",
      style = "
    background-color: #036;
    color: #fff;
    border-top: 2px solid #fcba19;
    position: absolute;
    bottom: 0;
    width: 100%;
    min-height: 50px;
    padding: 10px 20px;
    z-index: 1000;
    left: 0;
    box-sizing: border-box;
  ",
      
      div(
        class = "container",
        style = "
      display: flex;
      justify-content: space-between;
      align-items: flex-start;
      gap: 5px;
      width: 100%;
    ",
        
        # Footer links
        tags$ul(
          style = "
        list-style: none;
        display: flex;
        flex-wrap: wrap;
        gap: 15px;
        margin: 0;
        padding: 0;
        font-size: 14px;
      ",
          
          tags$li(
            tags$a(
              href = "//www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories",
              target = "_blank",
              style = "color: #fff; text-decoration: none;",
              "Home"
            )
          ),
          
          tags$li(
            tags$a(
              href = "//www2.gov.bc.ca/gov/content/home/disclaimer",
              target = "_blank",
              style = "color: #fff; text-decoration: none;",
              "Disclaimer"
            )
          ),
          
          tags$li(
            tags$a(
              href = "//www2.gov.bc.ca/gov/content/home/privacy",
              target = "_blank",
              style = "color: #fff; text-decoration: none;",
              "Privacy"
            )
          ),
          
          tags$li(
            tags$a(
              href = "//www2.gov.bc.ca/gov/content/home/accessibility",
              target = "_blank",
              style = "color: #fff; text-decoration: none;",
              "Accessibility"
            )
          ),
          
          tags$li(
            tags$a(
              href = "//www2.gov.bc.ca/gov/content/home/copyright",
              target = "_blank",
              style = "color: #fff; text-decoration: none;",
              "Copyright"
            )
          ),
          
          tags$li(
            tags$a(
              href = "//www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories",
              target = "_blank",
              style = "color: #fff; text-decoration: none;",
              "Contact Us"
            )
          )
        ),
      #  # Left text
      #  div(
      #    style = "
      #  white-space: nowrap;
      #  font-size: 16px;
      #",
      #    HTML("<strong>Provincial Site Productivity Layer v9</strong>")
      #  ),
        
      )
    )
)
)
