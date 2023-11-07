library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)

df <- read.csv("E:/studyMaterials/BigDataAnalysis/project/Training Dataset.csv")

str(df)

getMode <- function(x) {
  tbl <- table(x)
  names(tbl)[which.max(tbl)]
}


getPercentageOfColumn <- function(df, colname) {
  df_pct <- df %>%
    group_by({{colname}}) %>%
    summarize(count = n()) %>%
    mutate(percent = count / sum(count) * 100)
  return(df_pct)
}


drawHistogram <- function(df, colname){
  ggplot(df, aes(x = colname, fill = Loan_Status)) +
    geom_bar(position = "dodge", aes(y=(count)/sum(..count..))) +
    scale_y_continuous(labels = scales::percent_format()) + ylim(0, 1)
}



###### checking null values ######
sum(is.na(df))
colSums(is.na(df))


# loan amount is right skewed
ggplot(df, aes(x=LoanAmount)) + geom_histogram(color="darkblue", fill="lightblue", bins = 80) + 
  scale_x_continuous(labels=scales::comma) 

# removing nulls
summary(df$LoanAmount)
df$LoanAmount[is.na(df$LoanAmount)] <- median(df$LoanAmount, na.rm = TRUE)
summary(df$LoanAmount)


# loan amount term
ggplot(df, aes(x=Loan_Amount_Term)) + geom_histogram(color="darkblue", fill="lightblue")
unique(df$Loan_Amount_Term)

summary(df$Loan_Amount_Term)
df$Loan_Amount_Term[is.na(df$Loan_Amount_Term)] <- getMode(df$Loan_Amount_Term)
summary(df$Loan_Amount_Term)

# credit history
ggplot(df, aes(x=Credit_History)) + geom_bar(color="darkblue", fill="lightblue")

# check unique values
unique(df$Credit_History)
credit_history_mode <- getMode(df$Credit_History)

df$Credit_History[is.na(df$Credit_History)] <- getMode(df$Credit_History)
summary(df$Credit_History)

# double check for nulls
colSums(is.na(df))
str(df)



categorical_vars = c("Gender", "Married", "Dependents", "Education", "Self_Employed", "Credit_History", "Property_Area", "Loan_Status")

# checking unique values in categorical vars
# there are inputs that are left as empty strings
lapply(df[, categorical_vars], unique)

# removing wrong entries in categorical data
df$Gender[df$Gender == ""] <- getMode(df$Gender)
df$Married[df$Married == ""] <- getMode(df$Married)
df$Dependents[df$Dependents=="3+"] <- "3" #######---------- substitute 3+ by 3 --------##########
df$Dependents[df$Dependents == ""] <- getMode(df$Dependents)
df$Self_Employed[df$Self_Employed == ""] <- getMode(df$Self_Employed)

lapply(df[, categorical_vars], unique)


######## checking target variable ########
loan_status_pct <- getPercentageOfColumn(df, df$Loan_Status)
names(loan_status_pct)[names(loan_status_pct) == "df$Loan_Status"] <- "Loan_Status"


# accepted status is over 60 percent of the data
drawHistogram(df, df$Loan_Status) + labs(x = "Loan Status", y = "Percent")

######## Visualizing Categorical Variables ########



# Gender
gender_pct <- getPercentageOfColumn(df, df$Gender)
gender_pct
names(gender_pct)[names(gender_pct) == "df$Gender"] <- "Gender"
gender_pct
gender_pct$Gender <- as.factor(gender_pct$Gender)
gender_pct

# Percentage of men is much greater than women, they are more likely to get approved loans

drawHistogram(df, df$Gender) + labs(x = "Gender", y = "Percent") + ggtitle("")



# Married
married_pct <- getPercentageOfColumn(df, df$Married)
married_pct
names(married_pct)[names(married_pct) == "df$Married"] <- "Married"


# married applicants are more likely to take loans and get approved for them
drawHistogram(df, df$Married) + labs(x = "Married", y = "Percent") + labs(x = "Married", y = "Percent")



# Dependents
dependents_pct <- getPercentageOfColumn(df, df$Dependents)
dependents_pct
names(dependents_pct)[names(dependents_pct) == "df$Dependents"] <- "Dependents"


# Zero dependents are the majority and are more likely to get approved loans
drawHistogram(df, df$Dependents) + labs(x = "Dependents", y = "Percent") 




# Education
education_pct <- getPercentageOfColumn(df, df$Education)
dependents_pct
names(education_pct)[names(education_pct) == "df$Education"] <- "Education"


# about 80% of the population are graduates
drawHistogram(df, df$Education) + labs(x = "Education", y = "Percent")




# Self Employed
self_employed_pct <- getPercentageOfColumn(df, df$Self_Employed)
self_employed_pct
names(self_employed_pct)[names(self_employed_pct) == "df$Self_Employed"] <- "Self_Employed"

# most of the population are not self employed
drawHistogram(df, df$Self_Employed) + labs(x = "Self Employed", y = "Percent")



# Property Area
property_area_pct <- getPercentageOfColumn(df, df$Property_Area)
property_area_pct
names(property_area_pct)[names(property_area_pct) == "df$Property_Area"] <- "Property_Area"

# Semi urban area are more likely to take loans
# is it because low income or high price for properties in this area
drawHistogram(df, df$Property_Area) + labs(x = "Property Area", y = "Percent") + ggtitle("")

# visualizing areas separately
applicantIncome_semiUrban <- df[df$Property_Area == "Semiurban", ]
applicantIncome_rural <- df[df$Property_Area == "Rural", ]
applicantIncome_urban <- df[df$Property_Area == "Urban", ]

plot1 <- ggplot(applicantIncome_semiUrban, aes(x = ApplicantIncome)) +
  geom_histogram(binwidth=500, position = "dodge", color="darkblue", fill="lightblue") +
  ggtitle("Applicant Income in Semiurban area") +
  geom_vline(aes(xintercept=mean(ApplicantIncome))) +
  annotate("text", x = 20000, y = 30, 
           label = paste("Average Applicant income: ", round(mean(applicantIncome_semiUrban$ApplicantIncome), 2)))


plot2 <- ggplot(applicantIncome_rural, aes(x = ApplicantIncome)) +
  geom_histogram(binwidth=1000, position = "dodge", color="darkblue", fill="lightblue") +
  ggtitle("Applicant Income in Rural area") +
  geom_vline(aes(xintercept=mean(ApplicantIncome))) +
  annotate("text", x = 20000, y = 30, 
           label = paste("Average Applicant income: ", round(mean(applicantIncome_rural$ApplicantIncome), 2)))


plot3 <- ggplot(applicantIncome_urban, aes(x = ApplicantIncome)) +
  geom_histogram(binwidth=1000, position = "dodge", color="darkblue", fill="lightblue") +
  ggtitle("Applicant Income in Urban area") +
  geom_vline(aes(xintercept=mean(ApplicantIncome))) +
  annotate("text", x = 20000, y = 30, 
           label = paste("Average Applicant income: ", round(mean(applicantIncome_urban$ApplicantIncome), 2)))

grid.arrange(plot1, plot2, plot3, ncol=3)

# it seems that applicant income for different areas doesn't affect the 
# number of applicants in a certain area
sprintf("Average Incoome in Semiurban Areas: %f", mean(applicantIncome_semiUrban$ApplicantIncome))
sprintf("Average Incoome in rural Areas: %f", mean(applicantIncome_rural$ApplicantIncome))
sprintf("Average Incoome in urban Areas: %f", mean(applicantIncome_urban$ApplicantIncome))


# Credit History
credit_history_pct <- getPercentageOfColumn(df, df$Credit_History)
credit_history_pct
names(credit_history_pct)[names(credit_history_pct) == "df$Credit_History"] <- "Credit_History"

# applicants with credit history are more likely to get approved loans
drawHistogram(df, df$Credit_History) + labs(x = "Credit History", y = "Percent") + ggtitle("")



# Loan Amount Term
loan_amount_term_pct <- getPercentageOfColumn(df, df$Loan_Amount_Term)
loan_amount_term_pct
names(loan_amount_term_pct)[names(loan_amount_term_pct) == "df$Loan_Amount_Term"] <- "Loan_Amount_Term"

# most applicants take loans for 360 months
drawHistogram(df, df$Loan_Amount_Term) + labs(x = "Loan Amount Term", y = "Percent") + ggtitle("")



###### Numerical variables ######
numerical_vars = c("ApplicantIncome", "CoapplicantIncome", "LoanAmount")

# Applicant Income

app_income_box <- ggplot(df, aes(x=Loan_Status, y=ApplicantIncome, fill=Loan_Status)) + 
    geom_boxplot() +
    guides(fill = FALSE) +
    theme(legend.position = "none")

app_income_hist <- ggplot(df, aes(x=ApplicantIncome, fill=Loan_Status)) +
  geom_histogram(binwidth = 1000, position = 'dodge') +
  labs(title = "Distribution of Applicant Income by Loan Status", x = "Applicant Income", y = "Count")

grid.arrange(app_income_box, app_income_hist, ncol=2)

# Coapplicant Income
coapp_income_box <- ggplot(df, aes(x=Loan_Status, y=CoapplicantIncome, fill=Loan_Status)) + 
  geom_boxplot() +
  guides(fill = FALSE) +
  theme(legend.position = "none")

coapp_income_hist <- ggplot(df, aes(x=CoapplicantIncome, fill=Loan_Status)) +
  geom_histogram(binwidth = 800, position = 'dodge') +
  labs(title = "Distribution of Coapplicant Income by Loan Status", x = "Copplicant Income", y = "Count")


grid.arrange(coapp_income_box, coapp_income_hist, ncol=2)

# Loan Amount
loan_amount_box <- ggplot(df, aes(x=Loan_Status, y=LoanAmount, fill=Loan_Status)) + 
  geom_boxplot() +
  guides(fill = FALSE) +
  theme(legend.position = "none")

loan_amount_hist <- ggplot(df, aes(x=LoanAmount, fill=Loan_Status)) +
  geom_histogram(binwidth=20, position = 'dodge') +
  labs(title = "Distribution of Loan Amount by Loan Status", x = "Loan Amount", y = "Count")


grid.arrange(loan_amount_box, loan_amount_hist, ncol=2)



ggplot(df, aes(x=df$ApplicantIncome, y=df$LoanAmount))+geom_point(color="darkblue")

# normalizing Applicant Income, Co applicant Income, Loan Amount

# adding pseudocount
df$ApplicantIncome = df$ApplicantIncome + 1
df$CoapplicantIncome = df$CoapplicantIncome + 1
df$LoanAmount = df$LoanAmount + 1

df$ApplicantIncome = log(df$ApplicantIncome)
df$CoapplicantIncome = log(df$CoapplicantIncome)
df$LoanAmount = log(df$LoanAmount)

ggplot(df, aes(x=ApplicantIncome, fill=Loan_Status)) +
  geom_histogram( position = 'dodge') +
  labs(title = "Distribution of Applicant Income by Loan Status", x = "Applicant Income", y = "Count")

ggplot(df, aes(x=CoapplicantIncome, fill=Loan_Status)) +
  geom_histogram( position = 'dodge') +
  labs(title = "Distribution of Coapplicant Income by Loan Status", x = "Coapplicant Income", y = "Count")

ggplot(df, aes(x=LoanAmount, fill=Loan_Status)) +
  geom_histogram( position = 'dodge') +
  labs(title = "Distribution of Loan Amount by Loan Status", x = "Loan Amount", y = "Count")


################################################################
###################### Dropping Loan ID ########################
df <- subset(df, select = -Loan_ID)


################################################################
#####################  Transform to numeric ####################

################## Encoding non numeric Columns ################
d <- df[, c("Gender", "Married", "Dependents", "Education", "Self_Employed","Credit_History", "Property_Area", "Loan_Status")]

# Convert the categorical variables to factors
d_factor <- lapply(d, factor)

# Encode the factors with numeric values starting from 0
d_numeric <- lapply(d_factor, function(x) as.numeric(x) - 1)

# Replace the original categorical variables with the encoded numeric values
df[, c("Gender", "Married", "Education", "Self_Employed", "Dependents","Credit_History" ,"Property_Area", "Loan_Status")] <- d_numeric


###### Loan Amount Term ############
df$Loan_Amount_Term <- as.numeric(df$Loan_Amount_Term)
hist(df$Loan_Amount_Term)


################# Scaling Applicant Income and coApplicant Income ###############

#df$ApplicantIncome = scale(df$ApplicantIncome)
#df$CoapplicantIncome = scale(df$CoapplicantIncome)
#df$LoanAmount = scale(df$LoanAmount)
#df$Loan_Amount_Term = scale(df$Loan_Amount_Term)











