# Brainstorm

## Domain of Interest
 For our field of interest, we have chosen data around the **cost of college**. Specifically the cost of college and the student loans.
 We chose this field has it has a significant impact on our lives as some of us will be graduating with 100k+ in debt,
 it is a serious problem in the U.S. with many politicians vowing to combat the cost of college,
 and our findings can potentially help kids in the future decide the right path for their career.

## Some Data Driven Projects

### Student Debt Map
See project [here](https://www.policymap.com/2018/06/24350/). 
This project visualizes the average student debt of graduating seniors across the state in 2015-2016. 
It provides an interactives map of the U.S. that has a purple color gradient over each state representing 
the average amount of student debt.

### Report of Student Debt
See report [here](https://www.urban.org/sites/default/files/publication/97751/where_is_student_debt_highest_1.pdf). 
This research project answers two questions  
 - In which regions and states are college students most likely to borrow to pay for their postsecondary studies?
 - How does student debt relate to the cost of attending college in those states?

The research report collected their data through credit bureau analysis and the Department of
Education’s College Scorecard. They highlight correlation between cost of college and amount of debt.

### Mapping Student Debt
See map [here](https://mappingstudentdebt.org/#/map-3-labor-markets).
This map visualization correlates student debt with unemployment rates. It highlights the correlation between
employment rates and student debt. This map color codes counties in the U.S. though % of debt burden, average loan balance,
and unemployment rate

## What Questions Do We Hope To Answer?

- Are students better off going to trades schools / vocational schools instead of college?
    - We can answer this by comparing salaries of career paths, cost of college, and student debt of graduating students.
- Is there a correlation between high student debt and certain majors?
    - We can answer this by aggregating student debt data by major and visualizing the differences between majors
- Does acceptance rate have a correlation with student debt?
    - We can answer this by aggregating student debt data by college
    and comparing it to their admission rates.
- Has student debt increased over the last few years?
    - We can answe this by aggregating student debt by year and creating a visualization to represent the percent change.

## Finding Data
For each identified data source, 
- Link to URL where data is from, and download the data into the project repo in a `data/` folder (**1 point**)?
- Description of how was the data collected or generated (**1 point**)?
- How many observations (rows) and features (columns) are in your data(**1 point**)?
- What questions (from above) can be answered using the data in this dataset(**1 point**)?

## Student Debt Data: College Scorecard
Find data [here](https://collegescorecard.ed.gov/data/). See documentation [here](https://collegescorecard.ed.gov/assets/FieldOfStudyDataDocumentation.pdf)
The data is from the Department of Education.
As per the college scorecard website, 
> The methodology used to calculate earnings and cumulative debt by field of study was based, in part, on discussions from the spring 2019 Scorecard Technical Review Panel. These data are provided through federal reporting from institutions, data on federal financial aid, and tax information.

There are **216639** rows and **18** columns. Some question we can answer are: 
 - Is there a correlation between high student debt and certain majors?

## Student Debt Data: Federal Reserve Bank of New York

Find data here [here](https://data.world/finance/student-loan-debt).
This data set is from the Federal Reserve Bank of New York.
This data was collected by through the federal reserve bank calculating the student loans the federal government has each year.
There are two datasets. One is _balance_by_age_ and it has **7** columns and **11** rows. The other is _non_mort_balance_ which has **6** columns and **44** rows. We can answer:
- Has student debt increased over the last few years?

## Institution Level Data: College Scorecard

Find data [here](https://collegescorecard.ed.gov/data/). See documentation [here](https://collegescorecard.ed.gov/assets/FullDataDocumentation.pdf)
The data is from the Department of Education.
They collected this data through the universities / trade schools
reporting their own data to them.
This data set has **1982** columns and **6807** rows. This dataset will require lots of cleaning for columns, but it has very specfic design documention that will easily allow us to select the columns of interest.
Some questions from above we can answer are: 
- Are students better off going to trades schools / vocational schools instead of college?
- Does acceptance rate have a correlation with student debt?
- Has student debt increased over the last few years?
