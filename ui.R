
ui <- dashboardPage(
  
  dashboardHeader(
    title = "PSPL v9 Extractor",
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320, 
    
    sidebarMenu(
      
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
      
      #menuItem(text = "PSPL v9", icon = icon("file-import")),
      
      tags$div(
          style = "
    width: 100%;
    box-sizing: border-box;

    margin-top: 3px;
    margin-bottom: 3px;
    padding: 6px 3px;

    border: 1px solid #d2d6de;
    border-radius: 6px;
    background-color: #3D474C;

    /* important: prevent sidebar overflow */
    max-width: 100%;
    overflow: visible;
    
  ",
      selectizeInput(
        inputId = "species",
        label = "Choose Species:",
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
          
          fileInput(
            "upload",
            "Upload a file (.csv, .txt, .geojson)",
            accept = c(".csv", ".txt", ".geojson")
          ),
          
          checkboxInput(
            "header",
            "Column headers",
            TRUE
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
          )
        )
      ),
      
      checkboxInput(
        "show_coords",
        label = "Display on map",
        value = FALSE
      ),
      
      
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
          
          fileInput(placeholder = "shp,dbf,shx", #width = 185 ,
                    inputId = "filemap", 
                    label = "Upload Shapefile", multiple = TRUE, 
                    accept = c("shp","dbf", "shx", "sbn", "sbx", "prj", "xml","cpg"))
        )
      ),
      
      br(),
      br(),
      
      menuItem(text = "About", icon = icon("circle-info")),
      menuItem(text = "Example", icon = icon("glasses")),
      menuItem(text = "Read Me", icon = icon("book"))
    )
  ),
  
  
  dashboardBody(
    
    includeCSS("bcgov_01.css"),
    #use_waiter(),
    
    tags$head(
      tags$style(HTML("
      /* Sidebar menu font size */
    .sidebar-menu > li > a {
      font-size: 16px !important;
    }
    
    /* Optional: make icons align better with larger text */
    .sidebar-menu > li > a > i {
      font-size: 16px !important;
    }
        /* Header */
        .main-header .logo {
          font-size: 22px;
          font-weight: bold;
        }
        /* Footer */
        .main-footer {
          position: fixed;
          bottom: 0;
      left: 0;
      right: 0;
          width: 100%;
          z-index: 1000;
          background: white;
          border-top: 1px solid #d2d6de;
          padding: 10px;
      box-sizing: border-box;
      overflow-x: auto;
      white-space: nowrap;
        }

        /* Prevent footer overlap */
        .content-wrapper,
        .right-side {
          padding-bottom: 50px;
        }
      "))
    ),
    
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
      uiOutput("polygon_preview_ui")
      
      #box(
      #  title = "PSPL from entered coordinate",
      #  width = 12,
      #  tableOutput("preview")
      #),
      #box(
      #  title = "PSPL from uploaded file",
      #  width = 12,
      #  tableOutput("uploaded_preview"),
      #  downloadButton("download", "Download output",
      #                 style = "color: #fff; background-color: #27ae60; 
      #                 border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; ")
      #),
      #box(
      #  title = "Mean PSPL Uploaded polygon",
      #  width = 12,
      #  tableOutput("polygon_preview")
      #)
      
    ),
    
    tags$footer(
      class = "main-footer",
      style = "
    background-color: #036;
    color: #fff;
    border-top: 2px solid #fcba19;
    position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              
              padding: 10px;
              
              z-index: 1000;
              left: 0px;
  ",
      div(
        style = "display: flex;
      justify-content: center;
      align-items: center;
      width: 100%;
      white-space: nowrap;
      font-size: 16px;",
        HTML(
          "<strong>PSPL Extractor</strong> | Provincial Site Productivity Layer v9"
        )
      )
    )
  )
)
