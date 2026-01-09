#' Shiny UI for the Zenodo network explorer
#'
#' @importFrom shiny fluidPage titlePanel tags HTML sidebarLayout sidebarPanel
#' @importFrom shiny mainPanel tabsetPanel tabPanel textInput numericInput
#' @importFrom shiny passwordInput fluidRow column actionButton selectInput
#' @importFrom shiny checkboxGroupInput selectizeInput checkboxInput sliderInput numericInput helpText verbatimTextOutput
#' @importFrom shiny div fileInput
#' @importFrom shinyFiles shinySaveButton
#' @importFrom visNetwork visNetworkOutput
#' @importFrom DT dataTableOutput
#'
#' @return A Shiny UI object.
ui <- shiny::fluidPage(
  shiny::titlePanel("Zenodo Network Explorer"),
  shiny::tags$style(shiny::HTML("\
    .data-tabs .tab-content {
      min-height: 330px;
      height: 330px;
      overflow-y: auto;
    }
    .sidebar-separator {
      border: 0;
      height: 2px;
      background: #b5b5b5;
      margin: 12px 0;
    }
    .relations-disabled {
      color: #9a9a9a;
    }
    #relations .shiny-options-group {
      column-count: 3;
      column-gap: 12px;
    }
  ")),
  shiny::tags$script(shiny::HTML(
    "Shiny.addCustomMessageHandler('toggleRelationOptions', function(message) {\n",
    "  var container = document.getElementById('relations');\n",
    "  if (!container) { return; }\n",
    "  var available = message.available || [];\n",
    "  var inputs = container.querySelectorAll('input[type=checkbox]');\n",
    "  inputs.forEach(function(input) {\n",
    "    var wrapper = input.closest('label') || input.parentElement;\n",
    "    var enabled = available.indexOf(input.value) !== -1;\n",
    "    input.disabled = !enabled;\n",
    "    if (wrapper) {\n",
    "      wrapper.classList.toggle('relations-disabled', !enabled);\n",
    "    }\n",
    "  });\n",
    "});"
  )),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tags$h4("Data"),
      shiny::div(
        class = "data-tabs",
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Fetch",
            shiny::textInput("community", "Community id", "ipbes"),
            shiny::textInput("query", "Additional query (optional)", ""),
            shiny::numericInput(
              "max_records",
              "Max records (initial search)",
              100,
              min = 5,
              max = 1000,
              step = 5
            ),
            shiny::passwordInput("token", "Zenodo API token (optional, allows size up to 100)", ""),
            shiny::fluidRow(
              shiny::column(6, shiny::actionButton("fetch", "Fetch metadata")),
              shiny::column(
                6,
                shinyFiles::shinySaveButton(
                  "save_rds",
                  "Save data (RDS)",
                  "Save",
                  filename = paste0("zenodo_records_", Sys.Date(), ".rds")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Upload",
            shiny::fileInput("upload_rds", "Upload data (RDS)", accept = ".rds")
          )
        )
      ),
      shiny::tags$hr(class = "sidebar-separator"),
      shiny::tags$h4("Graph"),
      shiny::selectInput(
        "depth",
        "Expansion depth",
        choices = c("0" = 0, "1" = 1, "2" = 2),
        selected = 1
      ),
      shiny::checkboxInput(
        "community_only",
        "Only community-to-community links",
        FALSE
      ),
      shiny::checkboxGroupInput(
        "relations",
        "Relation types",
        choices = c(
          "All",
          "IsCitedBy",
          "Cites",
          "IsSupplementTo",
          "IsSupplementedBy",
          "IsContinuedBy",
          "Continues",
          "IsNewVersionOf",
          "IsPreviousVersionOf",
          "IsPartOf",
          "HasPart",
          "IsReferencedBy",
          "References",
          "IsDocumentedBy",
          "Documents",
          "IsCompiledBy",
          "Compiles",
          "IsVariantFormOf",
          "IsOriginalFormOf",
          "IsIdenticalTo",
          "IsReviewedBy",
          "Reviews",
          "IsDerivedFrom",
          "IsSourceOf",
          "IsRequiredBy",
          "Requires",
          "IsObsoletedBy",
          "Obsoletes",
          "IsDescribedBy",
          "Describes",
          "HasMetadata",
          "IsMetadataFor"
        ),
        selected = "All"
      ),
      shiny::selectizeInput(
        "keywords",
        "Keyword filter",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Choose keywords")
      ),
      shiny::sliderInput(
        "physics_stabilization",
        "Physics stabilization (iterations)",
        min = 0,
        max = 2000,
        value = 500,
        step = 50
      ),
      shiny::checkboxInput(
        "physics_enabled",
        "Enable physics",
        TRUE
      ),
      shiny::numericInput(
        "physics_max_time",
        "Max stabilization time (seconds)",
        value = 5,
        min = 0,
        max = 60,
        step = 1
      ),
      shiny::actionButton("refresh_graph", "Refresh graph"),
      shiny::helpText("Expansion uses Zenodo DOIs only and is capped for performance.")
    ),
    shiny::mainPanel(
      shiny::verbatimTextOutput("status"),
      shiny::tabsetPanel(
        shiny::tabPanel("Metadata", DT::dataTableOutput("metadata_table")),
        shiny::tabPanel("Graph", visNetwork::visNetworkOutput("graph", height = "700px"))
      )
    )
  )
)
