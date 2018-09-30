library(shinyjs)
library(shiny)



ui1 <- function(){
  tagList(
    tags$script('
              $(document).on("keypress", function (e) {
              Shiny.onInputChange("enter_key", e.which);
              });
              '), 
    div(id = "login",
        wellPanel(textInput("email", "Email Address"),
                  passwordInput("passwd", "Password"),
                  br(),
                  actionButton("Login", "Log in"),
                  uiOutput("login_message")
                  )),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}