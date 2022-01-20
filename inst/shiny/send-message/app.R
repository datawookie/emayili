library(shiny)
library(httr)
library(emayili)

# Setup:
#
# 1. Create an .Renviron file in the same directory as the app and provide
#    values for:
#
#    - SMTP_SERVER
#    - SMTP_PORT
#    - SMTP_USERNAME
#    - SMTP_PASSWORD
#
# 2. Deploy using rsconnect::deployApp().

SMTP_SERVER <- Sys.getenv("SMTP_SERVER")
SMTP_PORT <- Sys.getenv("SMTP_PORT")
SMTP_USERNAME <- Sys.getenv("SMTP_USERNAME")

IP <- content(GET("https://api.ipify.org?format=json"))$ip

smtp <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT,
  username = SMTP_USERNAME,
  password = Sys.getenv("SMTP_PASSWORD"),
  max_times = 1
)

ui <- fluidPage(
  titlePanel("{emayili} & Shiny"),

  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
      td {
        padding-left: 10px;
      }
    "))
  ),

  fluidRow(
    column(
      width = 6,
      tags$table(
        tags$tr(
          tags$th("{emayili version}:"),
          tags$td(as.character(packageVersion("emayili")))
        ),
        tags$tr(
          tags$th("IP address:"),
          tags$td(IP)
        ),
        tags$tr(
          tags$th("SMTP server:"),
          tags$td(SMTP_SERVER)
        ),
        tags$tr(
          tags$th("SMTP port:"),
          tags$td(SMTP_PORT)
        )
      )
    ),
    column(
      width = 6,
      class = "text-center",
      actionButton(
        "send",
        "Send Message",
        icon = icon("envelope"),
        class = "btn-primary"
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$send, {
    message('Sending message to ', SMTP_USERNAME,'.')
    envelope(
      to = Sys.getenv("SMTP_USERNAME"),
      from = Sys.getenv("SMTP_USERNAME"),
      subject = strftime(Sys.time(), "{emayili} & Shiny [%F %X]")
    ) %>%
      text("This message was sent from Shiny on {{ IP }} using {emayili}.") %>%
      smtp()
  })
}

shinyApp(ui = ui, server = server)
