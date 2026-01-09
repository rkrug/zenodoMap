# Entry point for shinyapps.io deployment.
library(shiny)

source(file.path("R", "api.R"))
source(file.path("R", "identifiers.R"))
source(file.path("R", "records.R"))
source(file.path("R", "relations.R"))
source(file.path("R", "graph.R"))
source(file.path("R", "utils.R"))
source(file.path("R", "query.R"))
source(file.path("R", "ui.R"))
source(file.path("R", "server.R"))
source(file.path("R", "run_app.R"))

run_app()
