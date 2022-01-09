library(shiny)


fist_question <- fluidPage()


conclusion <- tabPanel(
  "Conclusion",
  titlePanel("Conclusion"),
  h3("Are Potential Earnings of Prestige Colleges Worth the Cost?"),
  p("Many high school students are pushed by their aspirations and parents
    to go to the prestige college like UC Berkley, Harvard, or another
    college of the similar caliber. These types of colleges boast tuition
    costs up to $60,000 dollars per year. To explore if these prestige
    univerisities pay off, we can compare the yearly cost (tuition + books),
    years to get the degree, mean yearly earnings of the student six, eight,
    and ten years after enrollement, and admission rate. Important to note,
    the data on earnings comes from students who have filed for federal
    finiancial aid. This data comes from College Scorecard which is ran
    by the U.S. Department of Education, so all of this data federal.
    According to our study,",
    strong("in the long term the high cost prestigous colleges will pay off.
           However, it will take around 10 years after enrollment to
           overtake it."
         )
    ),
  h3("Do Certain States Correlate With Higher Student Debt?"),
  p("From the map, we can see that there is a large grouping of student debt
     in the southeastern states. Other notables states are South Dakota and
     Vermont. The large grouping of southeastern states can most likely be
     due to high college tuition costs, and a higher poverty level. Most
     colleges around the U.S. hold the same tuition, but the amount of money
     the students in a state have changes drastically. Other reasoning could
     be that more students are persuing higher education, which has a much
     higher cost than community colleges and trade schools. This could
     explain why states like New Mexico have lower student debt.
     In conclusion, ",
     strong("the reason why certain states have higher tuition and debt
             than others is potientially due to the presence of prestige
             schools and some underlying factors like poverty."
     )
  ),
  h3("Does Income Level and Institution Type Affect the Ability
     to Pay Off Debt?"),
  p(strong("Our study shows that we can infer that income level does affect
           a student's earnings post-graduation."),
    (" Although \"private\" colleges have the largest average debt, all income
    level earnings (when used effectively) are high enough to pay off the
    debts faster/easier than \"private for-profit\" colleges. Some could
    argue that with the most expensive colleges in the US for an academic
    year surpassing $75,000 for some \"private\" schools, the cumulative
    average debts seem relatively low. That is why max load debt is included
    in this chart to show that the variation between all the debts are wide,
    and can possibly explain the existance of students who have high loan
    debt in such colleges. Contextual variables such as financial aid can
    also tentatively conclude the significantly lower averages in comparison
    to such high max values for debt. Households with a large enough
    income would not qualify for financial aid nor need to take loans and
    low-level income households that quality for financial grants
     (as mentioned in the first paragraph of this section) would
     also direly need loans.")
  ),
  h3("Final Verdict"),
  p("In conclusion, there are many factors that are important regarding
     whether or not college is worth it. First, we took a look at the
     admission rate and saw how the the graduates suffer from debt.
     We saw how different admission rates affect the revenue of
     graduates over the years. Then we saw the different states and
     saw how people who went to school in those states suffer from
     tuition, debt, and the default rate. Finally, we saw how income
     level and institution type affect the ability to pay off debt.
     After reviewing all these factors, it really depends on the
     individual student if they think college is worth the risk.
     It is true that through college, students will have a better
     oppurtunity in furthering their studies and building their career,
     but they will also be plagued with debt for a large part of their
    lives.")
)