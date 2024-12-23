# Authors: Bazzan Sofia, Pasin Diletta
# Description: Preprocessing, Exploratory Data Analysis, and Train/Test Split for Spam Detection Project

# First, here there is a brief explanation of each variable
\begin{itemize}
\item  \textbf{Word frequency}: There are 48 continuous real attributes between 0 and 100  of the type word\_freq\_WORD that represents the percentage of words in the e-mail that match WORD, i.e. 100 * (number of times the WORD appears in the e-mail) / total number of words in e-mail.

(The variables of this type are : word\_freq\_make, word\_freq\_address, 
word\_freq\_all, word\_freq\_3d, word\_freq\_our, word\_freq\_over,
 word\_freq\_remove, word\_freq\_internet, word\_freq\_order, word\_freq\_mail, word\_freq\_receive, word\_freq\_will, word\_freq\_people, word\_freq\_report, word\_freq\_addresses, word\_freq\_free, word\_freq\_business, word\_freq\_email, word\_freq\_you, word\_freq\_credit, word\_freq\_your, word\_freq\_font,
  word\_freq\_000, word\_freq\_money, word\_freq\_hp, word\_freq\_hpl, word\_freq\_george, word\_freq\_650, word\_freq\_lab, word\_freq\_labs, word\_freq\_telnet, word\_freq\_857, word\_freq\_data, word\_freq\_415, 
  word\_freq\_85, \sloppy
  word\_freq\_technology, word\_freq\_1999, word\_freq\_parts, 
  word\_freq\_pm, word\_freq\_direct, word\_freq\_cs, word\_freq\_meeting, word\_freq\_original,
  word\_freq\_project, word\_freq\_re, word\_freq\_edu, word\_freq\_table, word\_freq\_conference)
\item  \textbf{Char frequency}: 6 of the attributes are frequency of characters in the mail. In particular their type could be defined as char\_freq\_CHAR that describe the percentage of characters in the e-mail that match CHAR, i.e. 100 * (number of CHAR occurences) / total characters in e-mail. Like the word frequencies also this variables are continuous real attributes between 0 and 100.

(To be more precise, this type of variables are the following: char\_freq\_.3B (;), char\_freq\_.28 ((), char\_freq\_.5B ([), char\_freq\_.21 (!), char\_freq\_.24 (\$), char\_freq\_.23 (\# )

\item  \textbf{capital\_run\_length\_average} 1 continuous real attribute of type capital\_run\_length\_average which denotes the average length of uninterrupted sequences of capital letters
\item  \textbf{capital\_run\_length\_longest} 1 continuous real attribute of type capital\_run\_length\_longest that is equal to the length of longest uninterrupted sequence of capital letters
\item  \textbf{capital\_run\_length\_total}  1 continuous real attribute that indicate the total number of capital letters in the e-mail
\item  \textbf{class} 1 nominal {0,1} class attribute of type spam that  denotes whether the e-mail was considered spam (1) or not (0)
\end{itemize}
                                                                                                                                                    
#Preprocessing                                                                                                                                                                
#Before we start analyzing this dataset, we want to understand how it's organized. To get the data ready for analysis, we begin by looking for any missing information. 
#To do that, we used the following line:
                                                                                                                                                                                          
{r, echo = TRUE}
has_missing <- any(is.na(df))
print(has_missing)
#We don't have missing values.
#Moving on, we also need to determine if there are any duplicate entries:    
{r, echo = TRUE}
has_duplicates <- any(duplicated(df))
print(has_duplicates)
#We found some duplicates, so our next task is to identify the number of duplicated rows and remove them. To achieve this, we utilize the unique command, which retains only the distinct rows of the dataframe:
{r, echo = TRUE}
duplicated_rows <- df[duplicated(df), ]
num_duplicates <- sum(duplicated(df))
print(num_duplicates)
df<- unique(df)
has_duplicates <- any(duplicated(df))
print(has_duplicates)
#Now, our focus shifts to verifying that the variable types match the descriptions we outlined earlier. Additionally, we want to ensure that the data doesn't contain inconsistencies. Specifically, we're checking for rows where the average frequency of a word is larger than its longest frequency, or where any word's frequency surpasses the maximum value of 100:
{r, echo=TRUE}
var_types <- sapply(df, class)
print(var_types)

result <- df[df$word_freq_average > df$word_freq_longest, ]

if (nrow(result) > 0) {
  print("There are lines where word_freq_average is longer than word_freq_longest.")
} else {
  print("There are no lines where word_freq_average is longer than word_freq_longest.")
}
# Our examination reveals that the variable types are as expected, and there are no instances where the average frequency exceeds the longest frequency. Next, we assess whether any word frequency values go beyond 100. To narrow our focus to just the frequency columns, we exclude other irrelevant columns:                                                                                                                                                                                         
{r, echo=TRUE}
columns_to_exclude <- c("capital_run_length_average", "capital_run_length_longest",
                        "capital_run_length_total","class")

all_column_names <- colnames(df)
columns_to_check <-all_column_names[!all_column_names %in% columns_to_exclude]

all_column_names <- colnames(df)
columns_to_check <- all_column_names[!all_column_names %in% columns_to_exclude]

has_greater_than_100 <- any(df[,columns_to_check] > 100)
has_greater_than_100
#In this investigation too, our dataset seems to have valid values without any clear anomalies.

#Now, since the variables representing frequencies range between 0 and 100 while others do not, we need to rescale the entire dataframe. Specifically, we transform all variables to a common scale between 0 and 1 (for this step we created a vector with the names of the variable to scale which include all the variables except for 'class' that can already assume just the values 0 or 1)                                                                                                                                                                                          
{r, echo=TRUE}
variables_to_scale <- names(df)[1:(ncol(df)-1)]
scaled_dataset <- df %>%
  mutate(across(all_of(variables_to_scale), rescale))
df<-scaled_dataset
# boxplot visualization      
{r, echo=TRUE}
boxplot(df$word_freq_telnet)
boxplot(df$word_freq_telnet[df$word_freq_telnet>0])
boxplot(log10(df$word_freq_telnet[df$word_freq_telnet>0]))

boxplot(df$word_freq_make[df$word_freq_make>0])
boxplot(log10(df$word_freq_make[df$word_freq_make>0])) 
#  Our box plot analysis revealed an interesting observation: some data points have a frequency of 1, which, after the logarithmic transformation, becomes equivalent to 0. However, we refrain from classifying these data points as outliers. This is because a frequency of 1 can be considered a strong signal that the email is likely spam.
#  Now we applied the logarihmic transformation to the whole dataframe, except to the column class, to keep the original binary classification that denotes as 0 the non spam content and 1 for spam.As part of the transformation we also added a small constant to prevent the possibility of obtaining a value of -Infinity when taking the logarithm of 0
{r, echo=TRUE}
no_class_columns <- names(df)[1:(ncol(df)-1)]
constant_value <- 0.0001
df[, no_class_columns] <- df[, no_class_columns] + constant_value
df[, no_class_columns] <- log10(df[, no_class_columns])
#  Next, we wanted to check if the data had a good balance between the two classes. After noticing that one class had around $40 \%$ representation while the other had about $60 \%$, we considered the option of keeping the data as it was.
{r, echo=TRUE, fig.width=5, fig.height=3}
class_proportions <- table(df$class) / nrow(df) * 100
class_proportions
# Create a bar plot
barplot(class_proportions, names.arg = c("Class 0", "Class 1"),
        main = "Class Distribution", xlab = "Class",
        ylab = "Percentage",col=c("blue","green"))  
#  The last step in our data preprocessing involved computing the correlation matrix to identify highly correlated variables. For our definition of high correlation, we set a threshold of $0.6.$
{r, echo=TRUE, fig.width=10, fig.height=8}
cor_matrix <- cor(df,method = "spearman")
image(cor_matrix, main = "Matrice di correlazione", 
      col = colorRampPalette(c("navyblue", "white", "firebrick3"))(100))
text(1:nrow(cor_matrix), 1:ncol(cor_matrix), round(cor_matrix, 2), 
{r, echo=TRUE, fig.width=10, fig.height=8}
cor_matrix <- cor(df,method = "spearman")
image(cor_matrix, main = "Matrice di correlazione", 
      col = colorRampPalette(c("navyblue", "white", "firebrick3"))(100))
text(1:nrow(cor_matrix), 1:ncol(cor_matrix), round(cor_matrix, 2),    
# eclipse plot to visualize the higly correlated variables:
{r, echo=TRUE}
plotcorr(cor_matrix[25:26,25:26])

plotcorr(cor_matrix[28:34,28:34])

plotcorr(cor_matrix[55:57, 55:57])
# retain just one variable for each group of higly correlated variables
{r, echo=TRUE}
vars_to_remove <- c()
for (i in 1:nrow(high_corr_pairs)) {
  var1 <- rownames(cor_matrix)[high_corr_pairs[i, "row"]]
  var2 <- colnames(cor_matrix)[high_corr_pairs[i, "col"]]
  if (!(var2 %in% vars_to_remove) && !(var1 %in% vars_to_remove)) {
    vars_to_remove <- c(vars_to_remove, var2)
  }
}
{r, echo=TRUE}
df <- df[, -which(colnames(df) %in% vars_to_remove)]

# DATA VISUALIZATION
# box plot for selected variables
{r, echo=TRUE}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
boxplot(df$word_freq_make~df$class, xlab='', ylab='', main='Word_freq_make')
boxplot(df$word_freq_address~df$class, xlab='', ylab='', main='word_freq_address')
boxplot(df$word_freq_all~df$class, xlab='', ylab='', main='word_freq_all')
boxplot(df$word_freq_415~df$class, xlab='', ylab='', main='word_freq_415')
boxplot(df$char_freq_.3B~df$class, xlab='', ylab='', main='char_freq_.3B')
# application of a logaritmic transformation
{r, echo=TRUE}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
boxplot(df$word_freq_make[df$word_freq_make>-4]
        ~df$class[df$word_freq_make>-4], xlab='', ylab='',
        main='Word_freq_make')
boxplot(df$word_freq_address[df$word_freq_address>-4]
        ~df$class[df$word_freq_address>-4], xlab='', ylab='',   
        main='word_freq_address')
boxplot(df$word_freq_all[df$word_freq_all>-4]
        ~df$class[df$word_freq_all>-4], xlab='', ylab='',
        main='word_freq_all')
boxplot(df$word_freq_415[df$word_freq_415>-4]
        ~df$class[df$word_freq_415>-4], xlab='', ylab='', 
        main='word_freq_415')
#the class one is limited to a certain range in this case
boxplot(df$char_freq_.3B[df$char_freq_.3B>-4]~
          df$class[df$char_freq_.3B>-4], xlab='', ylab='', 
        main='char_freq_.3B')
boxplot(df$word_freq_cs[df$word_freq_cs>-4]~
          df$class[df$word_freq_cs>-4], xlab='', ylab='', 
        main='word_freq_cs')
# Density plot
{r, echo=TRUE}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#all data
density_plot <- density(df$word_freq_make)
plot(density_plot, main = "Density Plot word_freq_make",
     xlab = "Value", ylab = "Density")


density_plot <- density(df$word_freq_address)
plot(density_plot, main = "Density Plot word_freq_address", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$word_freq_all)
plot(density_plot, main = "Density Plot word_freq_all", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$char_freq_.3B)
plot(density_plot, main = "Density Plot char_freq_.3B", 
     xlab = "Value", ylab = "Density")
{r, echo=TRUE}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#non zero data
density_plot <- density(df$word_freq_make[df$word_freq_make>-4])
plot(density_plot, main = "Density Plot word_freq_make",
     xlab = "Value", ylab = "Density")


density_plot <- density(df$word_freq_address[df$word_freq_address>-4])
plot(density_plot, main = "Density Plot word_freq_address", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$word_freq_all[df$word_freq_all>-4])
plot(density_plot, main = "Density Plot word_freq_all", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$char_freq_.3B[df$char_freq_.3B>-4])
plot(density_plot, main = "Density Plot char_freq_.3B",
     xlab = "Value", ylab = "Density")
# Bar plots:
{r, echo=TRUE}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#non zero data
density_plot <- density(df$word_freq_make[df$word_freq_make>-4])
plot(density_plot, main = "Density Plot word_freq_make",
     xlab = "Value", ylab = "Density")


density_plot <- density(df$word_freq_address[df$word_freq_address>-4])
plot(density_plot, main = "Density Plot word_freq_address", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$word_freq_all[df$word_freq_all>-4])
plot(density_plot, main = "Density Plot word_freq_all", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$char_freq_.3B[df$char_freq_.3B>-4])
plot(density_plot, main = "Density Plot char_freq_.3B",
     xlab = "Value", ylab = "Density")
{r, echo=TRUE}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#non zero data
density_plot <- density(df$word_freq_make[df$word_freq_make>-4])
plot(density_plot, main = "Density Plot word_freq_make",
     xlab = "Value", ylab = "Density")


density_plot <- density(df$word_freq_address[df$word_freq_address>-4])
plot(density_plot, main = "Density Plot word_freq_address", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$word_freq_all[df$word_freq_all>-4])
plot(density_plot, main = "Density Plot word_freq_all", 
     xlab = "Value", ylab = "Density")

density_plot <- density(df$char_freq_.3B[df$char_freq_.3B>-4])
plot(density_plot, main = "Density Plot char_freq_.3B",
     xlab = "Value", ylab = "Density")
# Bar plots
{r, echo=TRUE, fig.width=5, fig.height=3}
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
mean_by_class <- tapply(df$word_freq_make, df$class, mean)

# Create a bar plot
barplot(mean_by_class, beside = TRUE, names.arg = c("Class 0", "Class 1"),
        main = "word_freq_make", xlab = "Class", 
        ylab = "Mean Variable Value",
        col = c("blue", "green"))

mean_by_class <- tapply(df$word_freq_address, df$class, mean)

# Create a bar plot
barplot(mean_by_class, beside = TRUE, names.arg = c("Class 0", "Class 1"),
        main = "word_freq_address", xlab = "Class",
        ylab = "Mean Variable Value",
        col = c("blue", "green"))


mean_by_class <- tapply(df$word_freq_all, df$class, mean)

# Create a bar plot
barplot(mean_by_class, beside = TRUE, names.arg = c("Class 0", "Class 1"),
        main = "word_freq_all", xlab = "Class", 
        ylab = "Mean Variable Value",
        col = c("blue", "green"))


mean_by_class <- tapply(df$char_freq_.3B, df$class, mean)

# Create a bar plot
barplot(mean_by_class, beside = TRUE, names.arg = c("Class 0", "Class 1"),
        main = "char_freq_.3B", xlab = "Class",
        ylab = "Mean Variable Value",
        col = c("blue", "green"))
# Train test split
{r, echo=TRUE}
set.seed(123)
index <- sample(1:nrow(df), size = round(0.8*nrow(df)), replace = FALSE)
train <- df[index, ]
test <- df[-index, ]

index_train <- sample(1:nrow(train), size = round(0.8 * nrow(train)), 
                      replace = FALSE)
train_new <- train[index_train, ]
