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
        wellPanel(h2("Thermozymes"),
                  br(),
                  br(),
                  h3("Login"),
                  textInput("email", "Email Address"),
                  passwordInput("passwd", "Password"),
                  br(),
                  actionButton("Login", "Log in"),
                  uiOutput("login_message")
                  )),
    tags$style(type="text/css", "body {background-color: rgb(29, 39, 51)} .well {background-color: rgb(56, 75, 92); border: none;} #login { background-color: rgb(56, 75, 92); padding-left: 50px; padding-right: 50px; color: rgb(255, 255, 255); font-size:10px; text-align: left; position:absolute;top: 40%;left: 50%;  margin-top: -100px;margin-left: -150px;}
               #Login {background-color: rgb(50, 172, 237); 
                border-bottom-color: rgb(255, 255, 255);
              border-bottom-left-radius: 0px;
               border-bottom-right-radius: 0px;
               border-bottom-style: none;
               border-bottom-width: 0px;
               border-image-outset: 0;
               border-image-repeat: stretch stretch;
               border-image-slice: 100%;
               border-image-source: none;
               border-image-width: 1;
               border-left-color: rgb(255, 255, 255);
               border-left-style: none;
               border-left-width: 0px;
               border-right-color: rgb(255, 255, 255);
               border-right-style: none;
               border-right-width: 0px;
               border-top-color: rgb(255, 255, 255);
               border-top-left-radius: 0px;
               border-top-right-radius: 0px;
               border-top-style: none;
               border-top-width: 0px;
               box-sizing: border-box;
               color: rgb(255, 255, 255);
               width: 100%;}
               #Login:hover {background-color: #57bff7")
  )}

#.form-control {border: none; background-color: rgba(0, 0, 0, 0); border-bottom-color: rgb(165, 177, 190)'; border-right-style: none; border-right-width: 0px; border-left-style: none; border-left-width: 0px; border-top-style: none; border-top-width: 0px;}