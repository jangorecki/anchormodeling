library(shinydashboard)

AM <- function() getOption("am.share")
if(!anchormodeling::is.AM(AM())) stop("you must use `monitor` function which sets reference for anchor model.")

ui <- dashboardPage(
    dashboardHeader(title = "Anchor Modeling"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Anchor Model", tabName = "am", icon = icon("dashboard")),
            menuItem("3NF views", tabName = "data", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "am",
                    fluidRow(DT::dataTableOutput("am")),
                    fluidRow(downloadLink("export_xml", "Model XML")#, downloadLink("export_data", "Model data")
                             )
            ),
            tabItem(tabName = "data",
                    fluidRow(
                        selectInput("view",
                                    label = "3NF views",
                                    choices = list(Anchors = am$read(class="anchor")[, setNames(code, name)],
                                                   Ties = am$read(class="tie")[, setNames(code, name)])),
                        DT::dataTableOutput("data")
                    )
            )
        )
    )
)

server <- function(input, output) {

    output$am <- DT::renderDataTable(DT::datatable(AM()$process(pretty=TRUE), rownames=FALSE))

    output$export_xml <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_%Y%m%d_%H%M%S.xml")
        },
        content = function(file){
            am$xml(file)
        },
        contentType = "text/xml"
    )

    view <- reactive({
        validate(need(is.character(input$view), message = "Invalid view name"))
        validate(need(input$view %in% am$read(class=c("anchor","tie"))$code, message = "Provided view name does not exists in the model"))
        validate(need(nrow(AM()$read(input$view)$obj[[1L]]$data) > 0L, message = paste("No data loaded for", input$view)))
        AM()$view(input$view)
    })

    output$data <- DT::renderDataTable(DT::datatable(view(), rownames=FALSE))

}

shinyApp(ui, server)
