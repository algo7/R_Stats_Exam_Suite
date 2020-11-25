# R_Stats_Exam_Suite
Terminal application for university statistic exams.

# Current Topics Covered :
**There are many typos in the actual menu in the application**
 1. Descriptive Statistic
 2. Probability
 3. Discrete Variables
 4. Continuous Variables
 5. Sampling Distributions
 6. Confidence Interval
 7. Hypothesis Testing
 8. Simple Linear Regression
 9. Multiple Linear Regression
10. Chi2 Test

# Example Screenshot:
1. Main Menu and Selection Options:
![Example](https://raw.githubusercontent.com/algo7/R_Stats_Exam_Suite/master/screenshots/example.png)

2. Multiple linear regression (MLR) Example:
* The sample files can be found in th `samples` folder
    1. `Multiple_Regression.xlsx` (the excercise file)
    2. `sanitized_data.csv` (the file with processed data)
* In a real estate agency, the manager wants to analyze the price of houses depending on the age of the house, the size of the land lot and the size of living space.
* MLR Summary Function:

* MLR Results:
* The table is truncated as it is too long
![MLR Results](https://raw.githubusercontent.com/algo7/R_Stats_Exam_Suite/master/screenshots/MLR.jpg)



# Requirements:
1. [R](https://www.r-project.org/)
2. [RStudio Desktop Edition](https://rstudio.com/products/rstudio/download/)
3. This Repo. Clone it using Git or download the zip file directly.
4. Run the code below in the R Console if it's your 1st time using the script.
```R
install.packages(c('ggplot2','ggpubr','digest'
                   ,'argparser','cli','openintro','ggfortify',
                   'PEIP','corrplot','ggiraphExtra'))
```

# Steps (RStudio)[Recommended]:
1. Open the `stats.R` file in RStudio.
2. Open the Console at the bottom.
3. Type in `source('stats.R')`.


# Steps (R from the Terminal)[Some graphs might not be available]:
1. Open your terminal
2. Navigate to the directory where the files are.
3. Open R in the terminal by typing in `R`
4. Type in `source('stats.R')`

# Note:
1. The CSV file should be **clean** when importing, otherwise the program will not function properly.
  - Check there is no hidden data is the blank cells (newline character / NAs)
  - Format all the data in Excel's **Genearl** format.
2. The program is still in the Beta stage and is not bug-free.
