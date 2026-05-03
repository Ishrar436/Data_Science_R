

##############       A.Data Understanding          ##############       


# List of required packages

# PACKAGE SETUP

packages <- c("dplyr", "readr", "infotheo", "ggplot2", "scales", "tidyr", "GGally", "e1071", "reshape2", "VIM", "curl")

# remove duplicates just in case
packages <- unique(packages)

# install only missing packages
installed_packages <- rownames(installed.packages())
missing_packages <- packages[!(packages %in% installed_packages)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# load all packages
invisible(lapply(packages, library, character.only = TRUE))

# HELPER FUNCTION

get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


  
# Load the dataset into R using a link to my Google Drive
url <- "https://drive.google.com/uc?export=download&id=1RHzNTDQ-Rcn9h5Cr3Wx75cbfiOWGjTPv"
dataset <- read_csv(url)

# Load and inspect the dataset

#Display the first few rows of the dataset

head(dataset)


#Show shape

dim(dataset)

nrow(dataset)   

ncol(dataset)

cat("Shape of dataset:", nrow(dataset), "rows ×", ncol(dataset), "columns")


# Display data types of each column

str(dataset)


# Generate basic descriptive statistics 

colnames(dataset)



numerical_features <- c("math score", "reading score", "writing score")

for (col in numerical_features) {
    
    cat("\n===== ", col, " =====\n")
    
    cat("Mean:", mean(dataset[[col]], na.rm = TRUE), "\n")
    
    cat("Median:", median(dataset[[col]], na.rm = TRUE), "\n")
    
    cat("Count:", sum(!is.na(dataset[[col]])), "\n")
    
    cat("Mode:", get_mode(dataset[[col]]), "\n")
    
    cat("Standard Deviation:", sd(dataset[[col]], na.rm = TRUE), "\n")
    
    cat("Min:", min(dataset[[col]], na.rm = TRUE), "\n")
    
    cat("Max:", max(dataset[[col]], na.rm = TRUE), "\n")
    
    cat("Skewness:", skewness(dataset[[col]], na.rm = TRUE), "\n")
}


      
##############       B.Data Exploration & Visualization         ##############   



# 1.Univariate Analysis

#Histogram

numerical_features <- c("math score", "reading score", "writing score")

for (col in numerical_features) {
  
  p <- ggplot(dataset, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(
      title = paste("Histogram of", col),
      x = col,
      y = "Frequency"
    ) +
    theme_minimal()
  
  print(p)  
}


# Box Plot
numerical_features <- c("math score", "reading score", "writing score")
colors <- c("skyblue", "lightpink", "lightgreen")

for (i in 1:length(numerical_features)) {
  
  col <- numerical_features[i]
  
  p <- ggplot(dataset, aes(y = .data[[col]])) +
    geom_boxplot(fill = colors[i]) +
    labs(
      title = paste("Boxplot of", col),
      y = col
    ) +
    theme_minimal()
  
  print(p)   # <- important
}


#Barchart

categorical_features <- c("gender", "race/ethnicity", "parental level of education",
                          "lunch", "test preparation course")

colors <- c("orange", "purple", "skyblue", "green", "pink")

for (i in 1:length(categorical_features)) {
  
  col <- categorical_features[i]
  
  p <- ggplot(dataset, aes(x = .data[[col]])) +
    geom_bar(fill = colors[i], color = "black") +
    
    # show count inside bar
    geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "white", size = 5) +
    
    labs(
      title = paste("Bar Chart of", col),
      x = col,
      y = "Count"
    ) +
    theme_minimal()
  
  print(p)
}

# Pie Chart

categorical_features <- c("gender", "race/ethnicity", "parental level of education",
                          "lunch", "test preparation course")

colors <- c("orange", "purple", "skyblue", "green", "pink")

for (i in 1:length(categorical_features)) {
  
  col <- categorical_features[i]
  
  # count data
  count_data <- as.data.frame(table(dataset[[col]]))
  colnames(count_data) <- c("Category", "Count")
  
  p <- ggplot(count_data, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    
    # show value inside pie
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              color = "white",
              size = 5) +
    
    labs(
      title = paste("Pie Chart of", col),
      fill = col
    ) +
    theme_void()
  
  print(p)
}

#Frequency of categorical variables

categorical_features <- c("gender", "race/ethnicity", "parental level of education",
                          "lunch", "test preparation course")

for (col in categorical_features) {
  cat("\n===== Frequency of", col, "=====\n")
  print(table(dataset[[col]]))
}

#  2. Bivariate Analysis

#Heatmap

numerical_features <- c("math score", "reading score", "writing score")
num_data <- dataset[, numerical_features]

cor_matrix <- cor(num_data, use = "complete.obs")
cor_long <- melt(cor_matrix)

ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Scores", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



#Scatter plots for numeric pairs

numerical_features <- c("math score", "reading score", "writing score")

pairs <- combn(numerical_features, 2, simplify = FALSE)

for (pair in pairs) {
  
  x_col <- pair[1]
  y_col <- pair[2]
  
  p <- ggplot(dataset, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(color = "blue", alpha = 0.6) +
    labs(title = paste("Scatter Plot:", x_col, "vs", y_col),
         x = x_col,
         y = y_col) +
    theme_minimal()
  
  print(p)
}


# Boxplots between categorical and numeric features

categorical_features <- c("gender", "race/ethnicity", "parental level of education",
                          "lunch", "test preparation course")
numerical_features <- c("math score", "reading score", "writing score")

# Loop through each combination
for (cat_col in categorical_features) {
  for (num_col in numerical_features) {
    
    p <- ggplot(dataset, aes(x = .data[[cat_col]], y = .data[[num_col]])) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Boxplot of", num_col, "by", cat_col),
           x = cat_col,
           y = num_col) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
  }
}

#  3. Identify patterns, skewness, and possible outliers

#Patterns

numerical_features <- c("math score", "reading score", "writing score")

for (col in numerical_features) {
  m <- mean(dataset[[col]], na.rm = TRUE)
  med <- median(dataset[[col]], na.rm = TRUE)
  cat("Feature:", col, "\n")
  cat("Mean:", m, " Median:", med, "\n\n")
}

#Skewness measure

numerical_features <- sapply(dataset, is.numeric)

skew_values <- sapply(dataset[, numerical_features], skewness, na.rm = TRUE)

skew_df <- data.frame(
  Feature = names(skew_values),
  Skewness = skew_values
)

ggplot(skew_df, aes(x = Feature, y = Skewness)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Skewness,2)), vjust = -0.5, size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Skewness of Numerical Features",
    x = "Features",
    y = "Skewness"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Outliers (Using Boxplot Statistics)

numerical_features <- c("math score", "reading score", "writing score")

for (col in numerical_features) {
  Q1 <- quantile(dataset[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataset[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outliers <- dataset[[col]][dataset[[col]] < lower | dataset[[col]] > upper]
  
  cat("\nFeature:", col, "\n")
  cat("Number of potential outliers:", length(outliers), "\n")
  if(length(outliers) > 0) cat("Outlier values:", outliers, "\n")
}




      
##############       C. Data Preprocessing         ##############  

# Feature lists
numerical_features <- c("math score", "reading score", "writing score")
categorical_features <- c("gender", "race/ethnicity", "parental level of education",
                          "lunch", "test preparation course")


# 1.Handling Missing Values



# Keep original dataset unchanged

dataset_original <- dataset

cat("\n ORIGINAL DATA CHECK \n")

# Missing values in original dataset
missing_original <- colSums(is.na(dataset_original))
print(missing_original)
cat("Total missing values in original dataset:", sum(is.na(dataset_original)), "\n")

# Duplicate rows in original dataset
dup_count <- sum(duplicated(dataset_original))
cat("Duplicate rows in original dataset:", dup_count, "\n")

if (dup_count == 0) {
  cat("No duplicate rows found. No fixing needed.\n")
}


# Create one demo copy with a small percentage of artificial missing values

set.seed(123)

demo_data <- dataset_original %>%
  mutate(row_id = dplyr::row_number())

n_rows <- nrow(demo_data)

# small percentage
num_missing_count <- max(1, floor(0.01 * n_rows))   # 1% for numeric
cat_missing_count <- max(1, floor(0.005 * n_rows))  # 0.5% for categorical

# log exactly where NAs were inserted
na_log <- data.frame(
  row_id = integer(),
  column = character(),
  type = character(),
  stringsAsFactors = FALSE
)

    
# Insert missing values into numeric columns
for (col in numerical_features) {
  idx <- sample(1:n_rows, num_missing_count, replace = FALSE)
  demo_data[idx, col] <- NA
  na_log <- bind_rows(
    na_log,
    data.frame(row_id = idx, column = col, type = "numeric", stringsAsFactors = FALSE)
  )
}

# Insert missing values into categorical columns
for (col in categorical_features) {
  idx <- sample(1:n_rows, cat_missing_count, replace = FALSE)
  demo_data[idx, col] <- NA
  na_log <- bind_rows(
    na_log,
    data.frame(row_id = idx, column = col, type = "categorical", stringsAsFactors = FALSE)
  )
}

cat("\n DEMO COPY WITH SYNTHETIC MISSING VALUES \n")
print(colSums(is.na(demo_data)))

affected_row_ids <- sort(unique(na_log$row_id))

cat("\n ROWS CONTAINING NA (BEFORE IMPUTATION) \n")
rows_with_na_before <- demo_data %>%
  filter(row_id %in% affected_row_ids) %>%
  arrange(row_id)

print(rows_with_na_before)

cat("\n EXACT NA POSITIONS  \n")
print(arrange(na_log, row_id, column))


# Helper: mode function

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}


# A1: Mean (numeric) + Mode (categorical)

A1_data <- demo_data

for (col in numerical_features) {
  A1_data[[col]][is.na(A1_data[[col]])] <- mean(A1_data[[col]], na.rm = TRUE)
}

for (col in categorical_features) {
  mode_val <- get_mode(A1_data[[col]])
  A1_data[[col]][is.na(A1_data[[col]])] <- mode_val
}

cat("\n A1: AFTER MEAN + MODE IMPUTATION \n")
cat("Remaining missing values:", sum(is.na(A1_data)), "\n")

A1_rows <- A1_data %>%
  filter(row_id %in% affected_row_ids) %>%
  arrange(row_id)

print(A1_rows)


# A2: Median (numeric) + Mode (categorical)

A2_data <- demo_data

for (col in numerical_features) {
  A2_data[[col]][is.na(A2_data[[col]])] <- median(A2_data[[col]], na.rm = TRUE)
}

for (col in categorical_features) {
  mode_val <- get_mode(A2_data[[col]])
  A2_data[[col]][is.na(A2_data[[col]])] <- mode_val
}

cat("\n A2: AFTER MEDIAN + MODE IMPUTATION \n")
cat("Remaining missing values:", sum(is.na(A2_data)), "\n")

A2_rows <- A2_data %>%
  filter(row_id %in% affected_row_ids) %>%
  arrange(row_id)

print(A2_rows)


# B: Regression (numeric) + KNN (categorical)

B_data <- demo_data

# Convert categorical columns to factor for modeling/KNN
for (col in categorical_features) {
  B_data[[col]] <- as.factor(B_data[[col]])
}

# Temporary complete copy for regression predictors
reg_base <- B_data

# Fill predictor-side numeric missing values with median
for (col in numerical_features) {
  reg_base[[col]][is.na(reg_base[[col]])] <- median(reg_base[[col]], na.rm = TRUE)
}

# Fill predictor-side categorical missing values with mode
for (col in categorical_features) {
  mode_val <- get_mode(reg_base[[col]])
  reg_base[[col]][is.na(reg_base[[col]])] <- mode_val
  reg_base[[col]] <- as.factor(reg_base[[col]])
}

# Helper for column names with spaces
bt <- function(x) paste0("`", x, "`")

# Regression imputation for numeric columns
all_predictors <- c(numerical_features, categorical_features)

for (target in numerical_features) {
  missing_idx <- is.na(B_data[[target]])
  
  if (any(missing_idx)) {
    predictors <- setdiff(all_predictors, target)
    
    formula_text <- paste(
      bt(target),
      "~",
      paste(bt(predictors), collapse = " + ")
    )
    
    model_df <- reg_base[, c(target, predictors), drop = FALSE]
    
    # train only on rows where original target was present
    train_df <- model_df[!is.na(B_data[[target]]), , drop = FALSE]
    test_df  <- model_df[missing_idx, , drop = FALSE]
    
    fit <- lm(as.formula(formula_text), data = train_df)
    preds <- predict(fit, newdata = test_df)
    
    # keep student scores realistic
    preds <- pmax(0, pmin(100, preds))
    
    B_data[[target]][missing_idx] <- preds
  }
}

# KNN imputation for categorical columns
B_data_knn <- VIM::kNN(
  B_data,
  variable = categorical_features,
  k = 5,
  imp_var = FALSE
)

# convert factors back to character for cleaner printing
for (col in categorical_features) {
  B_data_knn[[col]] <- as.character(B_data_knn[[col]])
}

cat("\n B: AFTER REGRESSION + KNN IMPUTATION \n")
cat("Remaining missing values:", sum(is.na(B_data_knn)), "\n")

B_rows <- B_data_knn %>%
  filter(row_id %in% affected_row_ids) %>%
  arrange(row_id)

print(B_rows)


# Optional comparison summary

cat("\n COMPARISON SUMMARY \n")
cat("A1 remaining missing values:", sum(is.na(A1_data)), "\n")
cat("A2 remaining missing values:", sum(is.na(A2_data)), "\n")
cat("B remaining missing values :", sum(is.na(B_data_knn)), "\n")



# 2. Handling Outliers


#  Outlier Detection using IQR 

numerical_features <- c("math score", "reading score", "writing score")

# create a copy with row id for easier tracking
dataset_outlier <- dataset %>%
  mutate(row_id = row_number())

# store summary info
outlier_summary <- data.frame(
  feature = character(),
  Q1 = numeric(),
  Q3 = numeric(),
  IQR = numeric(),
  lower_bound = numeric(),
  upper_bound = numeric(),
  outlier_count = integer(),
  stringsAsFactors = FALSE
)

# store row ids and values of outliers
outlier_details <- data.frame(
  row_id = integer(),
  feature = character(),
  value = numeric(),
  stringsAsFactors = FALSE
)

for (col in numerical_features) {
  
  Q1 <- quantile(dataset_outlier[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataset_outlier[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  outlier_rows <- dataset_outlier %>%
    filter(.data[[col]] < lower | .data[[col]] > upper) %>%
    select(row_id, all_of(col))
  
  outlier_summary <- rbind(
    outlier_summary,
    data.frame(
      feature = col,
      Q1 = as.numeric(Q1),
      Q3 = as.numeric(Q3),
      IQR = as.numeric(IQR_val),
      lower_bound = as.numeric(lower),
      upper_bound = as.numeric(upper),
      outlier_count = nrow(outlier_rows),
      stringsAsFactors = FALSE
    )
  )
  
  if (nrow(outlier_rows) > 0) {
    temp_details <- data.frame(
      row_id = outlier_rows$row_id,
      feature = col,
      value = outlier_rows[[col]],
      stringsAsFactors = FALSE
    )
    
    outlier_details <- rbind(outlier_details, temp_details)
  }
}

cat("\n OUTLIER SUMMARY USING IQR \n")
print(outlier_summary)

cat("\n OUTLIER DETAILS (ROW ID + FEATURE + VALUE \n")
print(outlier_details)

# all unique rows that contain at least one outlier
all_outlier_row_ids <- unique(outlier_details$row_id)

all_outlier_rows <- dataset_outlier %>%
  filter(row_id %in% all_outlier_row_ids) %>%
  arrange(row_id)

cat("\n ALL ROWS CONTAINING OUTLIERS \n")
print(all_outlier_rows)



# Remove: bad choice here, because those rows are real students and you would lose data
# Transform: not a great fit for exam scores, because scores are already bounded and easy to interpret
# Cap: safest compromise — you keep all rows, but reduce extreme influence
# Use IQR-based capping:
# if a value is below the lower bound, replace it with the lower bound
# if a value is above the upper bound, replace it with the upper bound
#  Outlier Capping using IQR  

numerical_features <- c("math score", "reading score", "writing score")

# make a copy so original dataset stays unchanged
dataset_capped <- dataset %>%
  mutate(row_id = row_number())

# create capped flag columns at the beginning
dataset_capped$math_score_capped_flag <- 0
dataset_capped$reading_score_capped_flag <- 0
dataset_capped$writing_score_capped_flag <- 0

# store cap summary
capping_summary <- data.frame(
  feature = character(),
  Q1 = numeric(),
  Q3 = numeric(),
  IQR = numeric(),
  lower_bound = numeric(),
  upper_bound = numeric(),
  capped_count = integer(),
  stringsAsFactors = FALSE
)

# store outlier rows before capping
outlier_before_details <- data.frame(
  row_id = integer(),
  feature = character(),
  original_value = numeric(),
  capped_value = numeric(),
  stringsAsFactors = FALSE
)

for (col in numerical_features) {
  
  Q1 <- quantile(dataset_capped[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataset_capped[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  # find outliers before capping
  outlier_idx <- which(dataset_capped[[col]] < lower | dataset_capped[[col]] > upper)
  
  if (length(outlier_idx) > 0) {
    
    temp <- data.frame(
      row_id = dataset_capped$row_id[outlier_idx],
      feature = col,
      original_value = dataset_capped[[col]][outlier_idx],
      capped_value = pmin(pmax(dataset_capped[[col]][outlier_idx], lower), upper),
      stringsAsFactors = FALSE
    )
    
    outlier_before_details <- rbind(outlier_before_details, temp)
    
    # set the correct capped flag column
    if (col == "math score") {
      dataset_capped$math_score_capped_flag[outlier_idx] <- 1
    } else if (col == "reading score") {
      dataset_capped$reading_score_capped_flag[outlier_idx] <- 1
    } else if (col == "writing score") {
      dataset_capped$writing_score_capped_flag[outlier_idx] <- 1
    }
  }
  
  # cap values
  dataset_capped[[col]] <- pmin(pmax(dataset_capped[[col]], lower), upper)
  
  capping_summary <- rbind(
    capping_summary,
    data.frame(
      feature = col,
      Q1 = as.numeric(Q1),
      Q3 = as.numeric(Q3),
      IQR = as.numeric(IQR_val),
      lower_bound = as.numeric(lower),
      upper_bound = as.numeric(upper),
      capped_count = length(outlier_idx),
      stringsAsFactors = FALSE
    )
  )
}

cat("\n CAPPING SUMMARY \n")
print(capping_summary)

cat("\n OUTLIER VALUES BEFORE AND AFTER CAPPING \n")
print(outlier_before_details)

# get all affected row ids
affected_rows <- sort(unique(outlier_before_details$row_id))

# rows before capping
rows_before_capping <- dataset %>%
  mutate(row_id = row_number()) %>%
  filter(row_id %in% affected_rows) %>%
  arrange(row_id)

cat("\n ROWS WITH OUTLIERS BEFORE CAPPING \n")
print(rows_before_capping)

# same rows after capping
rows_after_capping <- dataset_capped %>%
  filter(row_id %in% affected_rows) %>%
  arrange(row_id)

cat("\n SAME ROWS AFTER CAPPING \n")
print(rows_after_capping)

cat("\n CAPPED FLAG COUNTS \n")
cat("math_score_capped_flag:\n")
print(table(dataset_capped$math_score_capped_flag))

cat("\nreading_score_capped_flag:\n")
print(table(dataset_capped$reading_score_capped_flag))

cat("\nwriting_score_capped_flag:\n")
print(table(dataset_capped$writing_score_capped_flag))

# final working dataset after capping
new_dataset <- dataset_capped

cat("\n FIRST 10 ROWS OF NEW DATASET \n")
print(head(new_dataset, 10))



#  3. Data Conversion

# We will work on the capped dataset from now on
converted_dataset <- new_dataset


# SHOW UNIQUE VALUES OF ALL 5 CATEGORICAL COLUMNS
# This helps verify the categories before encoding.

cat("\n UNIQUE VALUES OF CATEGORICAL COLUMNS \n")

cat("\ngender:\n")
print(unique(converted_dataset$gender))

cat("\nrace/ethnicity:\n")
print(unique(converted_dataset$`race/ethnicity`))

cat("\nlunch:\n")
print(unique(converted_dataset$lunch))

cat("\ntest preparation course:\n")
print(unique(converted_dataset$`test preparation course`))

cat("\nparental level of education:\n")
print(unique(converted_dataset$`parental level of education`))


# ONE-HOT ENCODING FOR NOMINAL COLUMNS
# These columns are NOMINAL categorical variables.
# Nominal means the categories are just labels and have no natural ranking.

# Nominal columns in this dataset:
# 1. gender -> male, female
# 2. race/ethnicity -> group A, B, C, D, E
# 3. lunch -> standard, free/reduced
# 4. test preparation course -> none, completed

# We use one-hot encoding because label encoding would create a false order.

# gender
gender_ohe <- model.matrix(~ gender - 1, data = converted_dataset)
gender_ohe <- as.data.frame(gender_ohe)
colnames(gender_ohe) <- make.names(colnames(gender_ohe))

# race/ethnicity
race_ohe <- model.matrix(~ `race/ethnicity` - 1, data = converted_dataset)
race_ohe <- as.data.frame(race_ohe)
colnames(race_ohe) <- make.names(colnames(race_ohe))

# lunch
lunch_ohe <- model.matrix(~ lunch - 1, data = converted_dataset)
lunch_ohe <- as.data.frame(lunch_ohe)
colnames(lunch_ohe) <- make.names(colnames(lunch_ohe))

# test preparation course
testprep_ohe <- model.matrix(~ `test preparation course` - 1, data = converted_dataset)
testprep_ohe <- as.data.frame(testprep_ohe)
colnames(testprep_ohe) <- make.names(colnames(testprep_ohe))



# ORDINAL ENCODING FOR PARENTAL LEVEL OF EDUCATION
# This column is ORDINAL categorical.
# Ordinal means the categories have a natural order from lower to higher level.

# Rank used here (low -> high):
# 0 = some high school
# 1 = high school
# 2 = some college
# 3 = associate's degree
# 4 = bachelor's degree
# 5 = master's degree

# Since there are 6 levels, the correct coding is 0 to 5.

converted_dataset$parent_education_label <- dplyr::case_when(
  converted_dataset$`parental level of education` == "some high school"    ~ 0,
  converted_dataset$`parental level of education` == "high school"         ~ 1,
  converted_dataset$`parental level of education` == "some college"        ~ 2,
  converted_dataset$`parental level of education` == "associate's degree"  ~ 3,
  converted_dataset$`parental level of education` == "bachelor's degree"   ~ 4,
  converted_dataset$`parental level of education` == "master's degree"     ~ 5,
  TRUE ~ NA_real_
)



# BUILD FINAL NUMERIC DATASET
# Keep:
# original numeric score columns
# capped flag columns
# ordinal-encoded parental education column
# one-hot encoded nominal columns
# Do NOT keep the original text categorical columns.

final_numeric_dataset <- cbind(
  converted_dataset[, c(
    "math score", "reading score", "writing score",
    "math_score_capped_flag", "reading_score_capped_flag", "writing_score_capped_flag",
    "parent_education_label"
  )],
  gender_ohe,
  race_ohe,
  lunch_ohe,
  testprep_ohe
)



# CHECK RESULT
cat("\n FINAL NUMERIC DATASET \n")
print(head(final_numeric_dataset))

cat("\n STRUCTURE OF FINAL NUMERIC DATASET \n")
str(final_numeric_dataset)

cat("\n COLUMN NAMES \n")
print(colnames(final_numeric_dataset))

cat("\n TOTAL NUMBER OF COLUMNS \n")
print(ncol(final_numeric_dataset))



# 4. Data Transformation

# Work on the fully numeric dataset
transformed_dataset <- final_numeric_dataset

# Only continuous score columns should be scaled/transformed
score_cols <- c("math score", "reading score", "writing score")


#  Min-Max Scaling

# Rescales values to the range [0, 1].
# Useful when we want all score columns on the same bounded scale.

for (col in score_cols) {
  min_val <- min(transformed_dataset[[col]], na.rm = TRUE)
  max_val <- max(transformed_dataset[[col]], na.rm = TRUE)
  
  new_col_name <- paste0(make.names(col), "_minmax")
  
  transformed_dataset[[new_col_name]] <- 
    (transformed_dataset[[col]] - min_val) / (max_val - min_val)
}


#  Z-score Standardization

# Converts values to mean = 0 and standard deviation = 1.
# Useful when we want to compare deviations from the average.

for (col in score_cols) {
  mean_val <- mean(transformed_dataset[[col]], na.rm = TRUE)
  sd_val   <- sd(transformed_dataset[[col]], na.rm = TRUE)
  
  new_col_name <- paste0(make.names(col), "_zscore")
  
  transformed_dataset[[new_col_name]] <- 
    (transformed_dataset[[col]] - mean_val) / sd_val
}


#  Check Skewness

# We check skewness first before deciding on sqrt transformation.
# sqrt is mainly helpful for positively skewed numeric variables.

skewness_summary <- data.frame(
  feature = score_cols,
  skewness = sapply(score_cols, function(col) skewness(transformed_dataset[[col]], na.rm = TRUE))
)

cat("\n SKEWNESS SUMMARY \n")
print(skewness_summary)


#  Apply sqrt transformation only if needed

# Rule used here:
# If skewness > 0.5, we create a sqrt-transformed version.
# If skewness is mild or negative, we skip sqrt for that column.

sqrt_applied <- c()

for (col in score_cols) {
  s <- skewness(transformed_dataset[[col]], na.rm = TRUE)
  
  if (s > 0.5) {
    new_col_name <- paste0(make.names(col), "_sqrt")
    transformed_dataset[[new_col_name]] <- sqrt(transformed_dataset[[col]])
    sqrt_applied <- c(sqrt_applied, col)
  }
}

cat("\n SQRT TRANSFORMATION STATUS \n")
if (length(sqrt_applied) == 0) {
  cat("No score column required sqrt transformation based on the chosen skewness rule.\n")
} else {
  cat("Sqrt transformation applied to:\n")
  print(sqrt_applied)
}


#  Check result

cat("\n TRANSFORMED DATASET COLUMN NAMES \n")
print(colnames(transformed_dataset))

cat("\n FIRST 6 ROWS OF TRANSFORMED DATASET \n")
print(head(transformed_dataset))

cat("\n TOTAL NUMBER OF COLUMNS \n")
print(ncol(transformed_dataset))





# 5. Feature Selection

# Feature Engineering + Feature Selection 

#  START FROM THE TRANSFORMED DATASET

# This dataset already contains:
#  original numeric score columns
#  encoded categorical columns
#  capped flag columns
#  min-max columns
#  z-score columns

feature_engineered_dataset <- transformed_dataset


# CREATE SAFE ENGINEERED FEATURES

# These new features do NOT use math score, so they are non-leaking.
# 1. Reading writing total 
# it captures total score of that column
feature_engineered_dataset$rw_total <- (
  feature_engineered_dataset$`reading score` +
    feature_engineered_dataset$`writing score`
) 
# 2. Reading-writing average
# It captures overall language performance using only predictor columns.
feature_engineered_dataset$rw_avg <- (
  feature_engineered_dataset$`reading score` +
    feature_engineered_dataset$`writing score`
) / 2

# 3. Reading-writing gap
# It measures the difference between reading and writing performance.
# A small gap means the student is consistent in both subjects.
feature_engineered_dataset$rw_gap <- abs(
  feature_engineered_dataset$`reading score` -
    feature_engineered_dataset$`writing score`
)

# 4. Combined academic strength flag
# It gives 1 if both reading and writing are at or above their median values.
# Median is used instead of a random threshold because it is data-driven.
reading_median <- median(feature_engineered_dataset$`reading score`, na.rm = TRUE)
writing_median <- median(feature_engineered_dataset$`writing score`, na.rm = TRUE)

feature_engineered_dataset$rw_strength_flag <- ifelse(
  feature_engineered_dataset$`reading score` >= reading_median &
    feature_engineered_dataset$`writing score` >= writing_median,
  1, 0
)

cat("\n NEW ENGINEERED FEATURES ADDED \n")
print(c("rw_total", "rw_avg", "rw_gap", "rw_strength_flag"))

cat("\n FIRST 10 ROWS OF NEW FEATURES \n")
print(feature_engineered_dataset[1:10, c("reading score", "writing score","rw_total",
                                         "rw_avg", "rw_gap", "rw_strength_flag")])


#  DEFINE TARGET AND REMOVE LEAKAGE / REDUNDANT COLUMNS

# Target variable = math score
target_col <- "math score"

# These columns should NOT be used as predictors:
#  math score transformed versions -> leak the target
#  math_score_capped_flag -> also derived from the target
# reading/writing scaled copies -> redundant because original versions are already kept
# reading.score_minmax , reading.score_zscore , writing.score_minmax , writing.score_zscore beacuse they represent the reading and writing columns
drop_cols <- c(
  "math.score_minmax",
  "math.score_zscore",
  "reading.score_minmax",
  "reading.score_zscore",
  "writing.score_minmax",
  "writing.score_zscore",
  "math_score_capped_flag",
  "reading_score_capped_flag", 
  "writing_score_capped_flag"
)

# Keep target + candidate predictors only
feature_selection_dataset <- feature_engineered_dataset[, 
                                                        setdiff(colnames(feature_engineered_dataset), drop_cols),
                                                        drop = FALSE
]

cat("\n DATASET USED FOR FEATURE SELECTION \n")
print(colnames(feature_selection_dataset))
cat("\nTotal columns after removing leakage/redundant columns:", ncol(feature_selection_dataset), "\n")


#  SEPARATE TARGET AND PREDICTORS

predictor_names <- setdiff(colnames(feature_selection_dataset), target_col)

X <- feature_selection_dataset[, predictor_names, drop = FALSE]
y <- feature_selection_dataset[[target_col]]


#  VARIANCE THRESHOLDING
# Features with very low variance change very little across rows.
# Such features usually provide weak information for prediction.

variance_table <- data.frame(
  feature = predictor_names,
  variance = sapply(X, function(col) var(col, na.rm = TRUE)),
  stringsAsFactors = FALSE
)

# Threshold choice:
# 0.01 is a small threshold that removes only near-constant columns.
variance_threshold <- 0.01

variance_table$keep_variance <- variance_table$variance > variance_threshold

cat("\n VARIANCE TABLE \n")
print(variance_table[order(variance_table$variance), ])

kept_after_variance <- variance_table$feature[variance_table$keep_variance]

X_var <- X[, kept_after_variance, drop = FALSE]

cat("\n FEATURES KEPT AFTER VARIANCE THRESHOLD \n")
print(kept_after_variance)

cat("\nDropped by variance threshold:\n")
print(variance_table$feature[!variance_table$keep_variance])


#  CORRELATION ANALYSIS
# Since math score is numeric, correlation helps us measure linear relationship

# Remove one dummy column from each binary one-hot group
# Reason:
# For binary one-hot variables, the two dummy columns are perfect complements.
# Example: genderfemale and gendermale
# Their correlation becomes -1, which is redundant.
# So we keep only one dummy column from each binary group.

reference_dummy_cols <- c(
  "genderfemale",
  "lunchfree.reduced",
  "X.test.preparation.course.none"
)

feature_selection_dataset2 <- feature_selection_dataset[, 
                                                        setdiff(colnames(feature_selection_dataset), reference_dummy_cols),
                                                        drop = FALSE
]

predictor_names <- setdiff(colnames(feature_selection_dataset2), target_col)
X <- feature_selection_dataset2[, predictor_names, drop = FALSE]
y <- feature_selection_dataset2[[target_col]]



# between each numeric predictor and the target.

correlation_table <- data.frame(
  feature = colnames(X_var),
  correlation_with_math = sapply(X_var, function(col) cor(col, y, use = "complete.obs")),
  stringsAsFactors = FALSE
)

correlation_table$abs_correlation <- abs(correlation_table$correlation_with_math)
correlation_table <- correlation_table[order(-correlation_table$abs_correlation), ]

cat("\n CORRELATION WITH MATH SCORE \n")
print(correlation_table)

#  Feature vs Feature Correlation
# We use only unique pairs:


feature_corr_matrix <- cor(X, use = "complete.obs")

upper_idx <- which(upper.tri(feature_corr_matrix), arr.ind = TRUE)

feature_feature_table <- data.frame(
  feature_1 = colnames(feature_corr_matrix)[upper_idx[, 1]],
  feature_2 = colnames(feature_corr_matrix)[upper_idx[, 2]],
  correlation = feature_corr_matrix[upper.tri(feature_corr_matrix)],
  stringsAsFactors = FALSE
)

feature_feature_table$abs_correlation <- abs(feature_feature_table$correlation)
feature_feature_table <- feature_feature_table[order(-feature_feature_table$abs_correlation), ]

cat("\n FEATURE vs FEATURE CORRELATION \n")
print(feature_feature_table)



# Keep only highly correlated pairs

correlation_threshold <- 0.85

high_corr_pairs <- feature_feature_table %>%
  filter(abs_correlation > correlation_threshold)

cat("\n HIGHLY CORRELATED FEATURE PAIRS (|r| > 0.85 \n")
print(high_corr_pairs)


#  Drop one feature from each high-correlation pair
# Rule:
# drop the one with lower target correlation
# if equal, drop one randomly

target_corr_map <- setNames(correlation_table$abs_correlation, correlation_table$feature)

set.seed(123)   # for reproducible random choice
dropped_features <- c()

drop_log <- data.frame(
  feature_1 = character(),
  feature_2 = character(),
  pair_corr = numeric(),
  target_corr_f1 = numeric(),
  target_corr_f2 = numeric(),
  dropped_feature = character(),
  kept_feature = character(),
  reason = character(),
  stringsAsFactors = FALSE
)

if (nrow(high_corr_pairs) > 0) {
  for (i in 1:nrow(high_corr_pairs)) {
    
    f1 <- high_corr_pairs$feature_1[i]
    f2 <- high_corr_pairs$feature_2[i]
    pair_corr <- high_corr_pairs$correlation[i]
    
    # if one feature is already dropped earlier, skip this pair
    if (f1 %in% dropped_features || f2 %in% dropped_features) {
      next
    }
    
    corr_f1 <- target_corr_map[f1]
    corr_f2 <- target_corr_map[f2]
    
    if (corr_f1 < corr_f2) {
      drop_feature <- f1
      keep_feature <- f2
      reason <- "Dropped because it has lower correlation with math score"
    } else if (corr_f2 < corr_f1) {
      drop_feature <- f2
      keep_feature <- f1
      reason <- "Dropped because it has lower correlation with math score"
    } else {
      drop_feature <- sample(c(f1, f2), 1)
      keep_feature <- ifelse(drop_feature == f1, f2, f1)
      reason <- "Both had same target correlation, so one was dropped randomly"
    }
    
    dropped_features <- c(dropped_features, drop_feature)
    
    drop_log <- rbind(
      drop_log,
      data.frame(
        feature_1 = f1,
        feature_2 = f2,
        pair_corr = pair_corr,
        target_corr_f1 = corr_f1,
        target_corr_f2 = corr_f2,
        dropped_feature = drop_feature,
        kept_feature = keep_feature,
        reason = reason,
        stringsAsFactors = FALSE
      )
    )
  }
}

cat("\n DROP DECISIONS \n")
print(drop_log)

cat("\n DROPPED FEATURES \n")
print(dropped_features)


# MUTUAL INFORMATION
# Correlation captures linear relationships, but mutual information can also
# capture more general dependency patterns.

# Helper function:
# if a feature already has only a few unique values (binary/dummy), keep it as discrete
# if a feature is continuous, discretize it first

make_discrete <- function(x) {
  if (length(unique(x)) <= 10) {
    return(as.factor(x))
  } else {
    disc_x <- infotheo::discretize(data.frame(x), disc = "equalfreq", nbins = 5)
    return(as.factor(disc_x[, 1]))
  }
}

# Discretize target because mutual information works on discrete data
y_disc <- infotheo::discretize(data.frame(y), disc = "equalfreq", nbins = 5)
y_disc <- as.factor(y_disc[, 1])

mi_scores <- sapply(colnames(X_var), function(col_name) {
  x_disc <- make_discrete(X_var[[col_name]])
  infotheo::mutinformation(x_disc, y_disc)
})

mi_table <- data.frame(
  feature = colnames(X_var),
  mutual_information = mi_scores,
  stringsAsFactors = FALSE
)

mi_table <- mi_table[order(-mi_table$mutual_information), ]

cat("\n MUTUAL INFORMATION SCORES \n")
print(mi_table)


#  COMBINE ALL FEATURE SELECTION RESULTS

feature_selection_summary <- variance_table %>%
  filter(feature %in% colnames(X_var)) %>%
  select(feature, variance) %>%
  left_join(correlation_table, by = "feature") %>%
  left_join(mi_table, by = "feature") %>%
  arrange(desc(mutual_information), desc(abs_correlation))

cat("\n FEATURE SELECTION SUMMARY \n")
print(feature_selection_summary)


# FINAL FEATURE DROPPING + FINAL DATASET

# 1. Features dropped by variance threshold
low_variance_dropped <- variance_table$feature[!variance_table$keep_variance]

cat("\n FEATURES DROPPED BY VARIANCE \n")
print(low_variance_dropped)

# 2. Features dropped by high feature-feature correlation
corr_dropped <- dropped_features

cat("\n FEATURES DROPPED BY CORRELATION \n")
print(corr_dropped)

# 3. Combine all dropped features
all_dropped_features <- unique(c(low_variance_dropped, corr_dropped))

cat("\n ALL FEATURES TO DROP \n")
print(all_dropped_features)

# 4. Final predictor list
final_features <- setdiff(
  setdiff(colnames(feature_selection_dataset), target_col),
  all_dropped_features
)

cat("\n FINAL FEATURES KEPT \n")
print(final_features)

# 5. Build final dataset
final_selected_dataset <- feature_selection_dataset[, c(target_col, final_features), drop = FALSE]

cat("\n FINAL SELECTED DATASET COLUMNS \n")
print(colnames(final_selected_dataset))

cat("\n FINAL SELECTED DATASET DIMENSION \n")
print(dim(final_selected_dataset))

cat("\n FIRST 10 ROWS OF FINAL SELECTED DATASET \n")
print(head(final_selected_dataset, 10))
