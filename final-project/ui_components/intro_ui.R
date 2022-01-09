library(shiny)

library("styler")
library("lintr")


intro <- tabPanel(
  "Home Page",
  h1("Are Prestige Universities Still Worth It?"),
  img(src = "https://s.hdnux.com/photos/35/10/35/7636851/3/920x1240.jpg"),
  p(
    paste("Our team decided to research on college and student debt.
    We decided on this because it is something all of us will eventually
    find troublesome and we thought that it would help some of our
    audience members if they were to see our research. With the enourmous
    amount of debt college students are left with after they graduate,
    our team decided to look at whether or not college is still worth it.
    Now just to be clear, our research isn't going to"), em("tell"),
    "you what to do. We're aiming to give", em("insights"),
    "that might help you reach", strong("your own conclusion.")
  ),
  h2("Background Information"),
  p(
    paste("The two datasets we used were from the same"),
    a(
      href = "https://collegescorecard.ed.gov/data/?fbclid=IwAR2
              NLbrO9LAzn0NdTaLpS9_GWicrwm4ve0O8p_u7sj7bDttPHEzpLj5FI9Y",
      "source."
    ),
    "One dataset focuses on the debt by each university, while the other
    focuses on debt by each major. Both of these datasets include
    important and interesting information about student debt. The reason
    why we picked certain topics to graphs was because we thought it
    would be interesting to dive deeper
    and expand in those topics."
  ),
  h2("Questions:"),
  p("Our main questions are as follows:"),
  tags$ul(
    tags$li("Are potential earnings of prestige colleges
            worth the cost?"),
    tags$li("Does income level and institution type affect
            the ability to pay off debt?"),
    tags$li("Do certain states correlate with higher student debt?")
  )
)