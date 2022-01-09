library("dplyr")
library("styler")

all_data <- read.csv(file = "data/Most-Recent-Cohorts-All-Data-Elements.csv", stringsAsFactors = FALSE)
# col of interest
# ABOUT THE INSTITUTION

# INSTNM (institution name)
# CITY, STABBR, ZIP, LATITUDE, LONGITUDE (city, state, zip code, latitude, longitude)
# HIGHDEG (years for degree)
# CONTROL (public, private, non-profit, private for-profit)
# RELAFFIL (if they are religious)
# DISTANCEONLY (if its an online school)
# AVGFACSAL (average faculty salary)
# TUITFTE (net tuition revenue per full-time equivalent) INEXPFTE (Instructional expenditures per FTE (1-year) student)

# ACADEMICS
# PRGMOFR (number of programs offered)
# Most specifics are in other data file

# ADMISSIONS
# OPENADMP (open admission)
# ADM_RATE (admission rate)

# COSTS
# TUITIONFEE_IN (tuition in state) TUITIONFEE_OUT (tuition out of state) COSTT4_A (total cost-tuition, books and fees)
# NPT4_PUB, NPT4_PRIV (net price - housing, tuition, books, etc.)

# STUDENT BODY
# UGDS (# of undergrad students)
# UGDS_MEN, UGDS_WOMEN, UGDS_WHITE, UGDS_BLACK, UGDS_ASIAN, UGDS_HISP (demographics)
# INC_PCT_LO $0-$30,000, INC_PCT_M1 $30,001-$48,000, INC_PCT_M2 = $48,001-$75,000
# INC_PCT_H1 $75,001-$110,000, INC_PCT_H2 $110,001+ (income levels for students who have filled out FAFSA)

# FINANCIAL AID
# FTFTPCTFLOAN (perentage of fulltime students on student loans)
# DEBT_MDN (culmulative median debt)
# LO_INC_DEBT_MDN = $0-$30,000, MD_INC_DEBT_MDN = $30,001-$75,000, HI_INC_DEBT_MDN = $75,001+ (debt by income level)

# EARNINGS
# MN_EARN_WNE_P* (mean earnings for are for the institutional aggregate of all federally aided 
# students who enroll in an institution each year and who are employed but not enrolled)
# (starts from 6 years after enrollment to 10 years after enrollment)
# (MN_EARN_WNE_INC1_P*, MN_EARN_WNE_INC2_P*, and MN_EARN_WNE_INC3_P* mean earnings by income  
# (low-income: $30,000 or less; middle-income: $30,001-$75,000; and high-income: $75,001+)

# REPAYMENT
# CDR3 (The three-year cohort default rate)
# (RPY_1YR_RT), 3 (RPY_3YR_RT), 5 (RPY_5YR_RT), and 7 RPY(_7YR_RT) (laon repayment rate at different years past grad)


# CLEAN
cleaned <- all_data %>%
  select(NAME = INSTNM,
                  CITY, STATE = STABBR, ZIP, LAT = LATITUDE, LON = LONGITUDE,
                  HIGHDEG, TYPE = CONTROL, RELIGIOUS = RELAFFIL, ONLINE = DISTANCEONLY,
                  AVGFACSAL, TUIREVSTU = TUITFTE, 
                  INCOSTSTU = INEXPFTE, PRGMOFR, OPENADMP, 
                  ADM_RATE, TUITIONFEE_IN, TUITIONFEE_OUT, COST = COSTT4_A,
                  NPT4_PUB, NPT4_PRIV, UGDS, UGDS_MEN, UGDS_WOMEN, UGDS_WHITE, UGDS_BLACK, UGDS_ASIAN, UGDS_HISP,
                  INC_PCT_LO, INC_PCT_M1, INC_PCT_M2, INC_PCT_H1, INC_PCT_H2, FTFTPCTFLOAN, GRAD_DEBT_MDN,
                  LO_INC_DEBT_MDN, MD_INC_DEBT_MDN, HI_INC_DEBT_MDN, MN_EARN_WNE_P6,
                  MN_EARN_WNE_P7,MN_EARN_WNE_P8, 
                  MN_EARN_WNE_P9, MN_EARN_WNE_P10,
                  MN_EARN_WNE_INC1_P6, MN_EARN_WNE_INC2_P6,MN_EARN_WNE_INC3_P6,
                  MN_EARN_WNE_INC1_P10, MN_EARN_WNE_INC2_P10,MN_EARN_WNE_INC3_P10,
                  CDR3, RPY_1YR_RT, RPY_3YR_RT, RPY_5YR_RT, RPY_7YR_RT)


write.csv(cleaned, file="insitution-data-cleaned.csv")