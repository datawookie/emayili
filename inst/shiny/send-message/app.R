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

smtp_server <- Sys.getenv("SMTP_SERVER")
smtp_port <- Sys.getenv("SMTP_PORT")
smtp_username <- Sys.getenv("SMTP_USERNAME")

IP <- content(GET("https://api.ipify.org?format=json"))$ip

smtp <- server(
  host = smtp_server,
  port = smtp_port,
  username = smtp_username,
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
          tags$td(smtp_server)
        ),
        tags$tr(
          tags$th("SMTP port:"),
          tags$td(smtp_port)
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
    message("Sending message to ", smtp_username, ".")
    envelope(
      to = smtp_username,
      from = smtp_username,
      subject = strftime(Sys.time(), "{emayili} & Shiny [%F %X]")
    ) %>%
      text("This message was sent from Shiny on {{ IP }} using {emayili}.") %>%
      smtp()
  })
}

shinyApp(ui = ui, server = server)
