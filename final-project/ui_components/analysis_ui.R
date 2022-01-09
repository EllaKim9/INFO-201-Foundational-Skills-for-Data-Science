library(shiny)

earnings_intro <- fluidPage(
  h2("Is Potential Earnings of Prestige Colleges Worth the Cost?"))


earnings_analysis <- fluidPage(
  h3("The Importance of this Graph"),
  p(em("Note: Per College Scorecard, Earnings are defined as the
        sum of wages and deferred compensation from all non-duplicate W-2
        forms received for each individual (from both full- and part-time
        employment), plus positive self-employment earnings from Schedule
        SE.")),
  p("This graph has a lot of information to digest. We can take a look at
    each year individually and the trend over each year. Six years after
    enrollment, we can see that colleges that have over 75% admission rate
    (not-prestigous) are extremely valuable for the cost. The mean earnings are
    around $18,000, which is great intial value. We can compare this to the
    other end where acceptance rate is around 0%, and we can see that the mean
    earnings are $5,000 which is significantly less. When we take a look at
    the trend of yearly earnings over six, eight, and ten years, we can see
    that earnings increase across the board. The line shifts up nearly $10,000
    for each sdmisson rate. However, one section increases much more than the
    others. Ten years after enrollment, we can see that the earnings for
    prestige, low admission college increased drastically. The earnings
    increased to around $37,000 dollars! This is much higher compared than
    the low prestige, high admission rate which did increase to around
    $25,000 dollars."),
  strong("From these insights, we can infer than in the long term the high
         cost prestigous colleges will pay off. However, it will take around
         10 years after enrollment to overtake it.")
)

map_analysis <- fluidPage(
  h3("Importance"),
  p("A question we want to answer is, Do certain states correlate with higher
    student debt/tuition cost/default rates? We can take a look at these
    correlations and think about what relates these factors and the state.
    e.g poverty, institution cost, public / private colleges."),
  h4("Average Student Debt Map"),
  p("From this map, we can see that there is a large grouping of student debt
    in the southeastern states. Other notables states are South Dakota and
    Vermont. The large grouping of southeastern states can most likely be due
    to high college tuition costs, and a higher poverty level. Most colleges
    around the U.S. hold the same tuition, but the amount of money the students
    in a state have changes drastically. Other reasoning could be that more
    students are persuing higher education, which has a much higher cost than
    community colleges and trade schools. This could explain why states like
    New Mexico have lower student debt."),
  h4("Average Tuition Cost Map"),
  p("This map shows that the average tuition cost is slightly correlated with
    the previous map. The State with the lowest average tuition is Wyoming,
    which also had the lowest average graduation debt. Similarly, in the
    New England area, there seems to be a much higher average tuition cost
    than the surrounding states. This is most likely due to the large
    number of high prestige schools with much lower admission rates.
    These schools are able to charge more and without the risk of
    a reduction in applicants."),
  h4("Average Default Rate"),
  p("This map shows that the default rate is not correlated as much as the
    other two. High default rates are especially prevelent in the southern
    states, despite these states having relatively lower student debt and
    tuition costs. This is most likely due to relatively higher poverty rates
    or that graduates are unable to obtain jobs and pay off their
    student loans.")
)

type_intro <- fluidPage(
  h2(
    "Does Income Level and Institution
    Type Affect the Ability to Pay Off Debt?")
)

type_analysis <- fluidPage(
  h3("The Importance of this Graph"),
  p(em("Note: The income level is determined by FAFSA data: low = $0-$30,000,
       medium = $30,001-$75,000, high = $75,001+). Loan debt in this variable
       is calculated by the median cumulative debt each student has borrowed
       during their time enrolled in the institution.")),
  p(em("Legend: max represents the highest loan debt from the type of
       institution and avg represents the average loan debt from the type of
       institution. Low, medium, and high represent the levels of income in the
       \"Note\" section above.")),
  p("Taking an initial scan, the average amount of debt depends on
    the type of instiution, where \"public\" colleges have the minimum debt of
    approx(~). $16,000, and \"private\" colleges have the max of ~$24,000.
    There is a pattern of the income level (low, medium, and high) resulting in
    the lowest to highest (respectively) average earnings 10 years
    post-graduation for all types of institutions. All the income level
    earnings for \"private for-profit\" colleges are the minimum values when
    comparing the corresponding levels across all the types of schools, versus
    \"private\" institutions and all their earnings being the maximum for each
    level."),
  p("Average debt accumilated at graduation is significantly less than the
    earnings for all income levels, especially for students graduating from
    \"public\" and \"private\" schools. The difference between low-income
    earnings and the average debt for \"private for-profit\" is only ~$13,000,
    whereas \"private\" and \"public\" institutions have a difference of
    ~$23,000 and ~$24,000 respectively. With \"public\" and \"private\"
    institutions' low-level income earnings still surpassing their own max
    cumulative loan debt, private for-profit's max is only surpassed by the
    high-level income earnings."),
  strong("From these insights, we can infer that income level does affect a
         student's earnings post-graduation and that amount is dependent on the
         type of institution they graduate from. Regardless of the amount of
         average debt, all income level earnings (when used effectively) from
         \"private\" and \"public\" schools are high enough to pay off the debt
         faster/easier than \"private for-profit\" colleges.")
)