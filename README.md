# R_Stats_Exam_Suite
Terminal application for univeristy statistic exams.

# Example Screenshot:
![Example](https://raw.githubusercontent.com/algo7/R_Stats_Exam_Suite/master/example.png)

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

# Steps (RStudio):
1. Open the `stats.R` file in RStudio.
2. Open the Console at the bottom.
3. Type in `source('stats.R')`.


# Steps (R from the Terminal):
1. Open your terminal
2. Navigate to the directory where the files are.
3. Open R in the terminal by typing in `R`
4. Type in `source('stats.R')`

# Note:
1. The CSV file should be **clean** when importing, otherwise the program will not function properly.
  - Check there is no hidden data is the blank cells (newline character / NAs)
  - Format all the data in Excel's **Genearl** format.
2. The program is still in the Beta stage and is not bug-free.
