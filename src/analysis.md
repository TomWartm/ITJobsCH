Data Analysis of IT jobs
================

Data Visualiization and Analysis from the data collected in the previous
steps from jobs.ch and itjobs.ch. Some of the job listings are rated
from a scale from 0,9 according to my interests and qualifications.

## Import Data

``` r
library(tidyverse)
library(jsonlite)
library(lubridate)
```

``` r
raw_df <- fromJSON("../data/jobs_processed.json")
raw_df <- bind_rows(raw_df)
raw_df <- tibble(raw_df)
glimpse(raw_df)
```

    ## Rows: 4,292
    ## Columns: 25
    ## $ company               <chr> NA, "BBT Software AG", "VAT Vakuumventile AG", "~
    ## $ contract_type         <chr> NA, NA, NA, "Unlimited employment", NA, NA, NA, ~
    ## $ descriptions          <list> [<data.frame[0 x 0]>], [<data.frame[1 x 1]>], [~
    ## $ downloaded            <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, ~
    ## $ job_title             <chr> "Bauingenieur*in (60-100%) Bereich Konstruktiver~
    ## $ language              <chr> NA, NA, NA, NA, NA, NA, NA, NA, "German (Interme~
    ## $ place_of_work         <chr> NA, "Root D4", NA, "Ringstrasse 39, 4106 Therwil~
    ## $ publication_date      <chr> NA, "06 February 2025", NA, "07 February 2025", ~
    ## $ rating                <dbl> NA, 2, NA, 6, NA, NA, NA, NA, 6, NA, NA, 4, NA, ~
    ## $ reviewed              <dbl> NA, 1, NA, 1, NA, NA, NA, NA, 1, NA, NA, 1, NA, ~
    ## $ salary                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    ## $ search_query          <chr> "data engineer", "all jobs", "software engineer"~
    ## $ url                   <chr> "https://www.jobs.ch/en/vacancies/detail/221d09c~
    ## $ website               <chr> NA, "ITJobs", "Jobs", NA, NA, NA, NA, NA, NA, "I~
    ## $ workload              <chr> NA, NA, NA, "100%", NA, NA, NA, NA, "80 – 100%",~
    ## $ career_stage_cleaned  <chr> NA, NA, NA, NA, NA, NA, NA, "senior", NA, NA, NA~
    ## $ canton                <chr> NA, NA, NA, NA, NA, NA, NA, NA, "Zürich", "Schwy~
    ## $ programming_languages <list> <>, <>, <>, <"MATLAB", "Python">, <>, <>, <>, <~
    ## $ frameworks            <list> <>, <>, <>, <>, <>, <>, <>, <>, <"Angular", "Sp~
    ## $ tools                 <list> <>, <>, <>, <>, <>, <>, <>, <>, <"Git", "Jenkin~
    ## $ operating_systems     <list> <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>,~
    ## $ years                 <list> <>, <>, <>, <>, <>, <>, <>, <>, 5, <>, <>, <>, ~
    ## $ education             <list> <>, <>, <>, "PhD", <>, <>, <>, <>, "Vocational"~
    ## $ job_title_cleaned     <chr> NA, "System Engineer", NA, "Research Engineer", ~
    ## $ job_category          <chr> NA, "Cloud/System Engineer", NA, "Software Engin~

## Clean Data

``` r
# convert datatypes
clean_df <- raw_df %>%
    mutate(publication_date = dmy(publication_date), 
        job_title_cleaned = factor(job_title_cleaned),
        career_stage_cleaned = factor(career_stage_cleaned),
        job_category = factor(job_category),
        canton = factor(canton),
        programming_languages = lapply(programming_languages, factor),
        frameworks = lapply(frameworks, factor),
        tools = lapply(tools, factor),
        operating_systems = lapply(operating_systems, factor),
        max_years = sapply(years, function(x) if (length(x) == 0) NA else max(as.numeric(x), na.rm = TRUE)), # get min number of years, and replace nan with 0
        programming_languages = ifelse(lengths(programming_languages) == 0, "No Programming Languages", programming_languages),
        frameworks = ifelse(lengths(frameworks) == 0, "No Frameworks", frameworks),
        tools = ifelse(lengths(tools) == 0, "No Tools", tools))
glimpse(clean_df)
```

    ## Rows: 4,292
    ## Columns: 26
    ## $ company               <chr> NA, "BBT Software AG", "VAT Vakuumventile AG", "~
    ## $ contract_type         <chr> NA, NA, NA, "Unlimited employment", NA, NA, NA, ~
    ## $ descriptions          <list> [<data.frame[0 x 0]>], [<data.frame[1 x 1]>], [~
    ## $ downloaded            <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, ~
    ## $ job_title             <chr> "Bauingenieur*in (60-100%) Bereich Konstruktiver~
    ## $ language              <chr> NA, NA, NA, NA, NA, NA, NA, NA, "German (Interme~
    ## $ place_of_work         <chr> NA, "Root D4", NA, "Ringstrasse 39, 4106 Therwil~
    ## $ publication_date      <date> NA, 2025-02-06, NA, 2025-02-07, NA, NA, NA, NA,~
    ## $ rating                <dbl> NA, 2, NA, 6, NA, NA, NA, NA, 6, NA, NA, 4, NA, ~
    ## $ reviewed              <dbl> NA, 1, NA, 1, NA, NA, NA, NA, 1, NA, NA, 1, NA, ~
    ## $ salary                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    ## $ search_query          <chr> "data engineer", "all jobs", "software engineer"~
    ## $ url                   <chr> "https://www.jobs.ch/en/vacancies/detail/221d09c~
    ## $ website               <chr> NA, "ITJobs", "Jobs", NA, NA, NA, NA, NA, NA, "I~
    ## $ workload              <chr> NA, NA, NA, "100%", NA, NA, NA, NA, "80 – 100%",~
    ## $ career_stage_cleaned  <fct> NA, NA, NA, NA, NA, NA, NA, senior, NA, NA, NA, ~
    ## $ canton                <fct> NA, NA, NA, NA, NA, NA, NA, NA, Zürich, Schwyz, ~
    ## $ programming_languages <list> "No Programming Languages", "No Programming Lan~
    ## $ frameworks            <list> "No Frameworks", "No Frameworks", "No Framework~
    ## $ tools                 <list> "No Tools", "No Tools", "No Tools", "No Tools",~
    ## $ operating_systems     <list> <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>,~
    ## $ years                 <list> <>, <>, <>, <>, <>, <>, <>, <>, 5, <>, <>, <>, ~
    ## $ education             <list> <>, <>, <>, "PhD", <>, <>, <>, <>, "Vocational"~
    ## $ job_title_cleaned     <fct> NA, System Engineer, NA, Research Engineer, NA, ~
    ## $ job_category          <fct> NA, Cloud/System Engineer, NA, Software Engineer~
    ## $ max_years             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA, N~

Check if some IT key words in listing title were missed during
preprocessing, and therefore have a NaN value in the `job_title_cleaned`
attribute.

``` r
clean_df %>%
  filter(is.na(job_title_cleaned)) %>%
  slice_head(n = 20) %>%
  pull(job_title) 
```

    ##  [1] "Bauingenieur*in (60-100%) Bereich Konstruktiver Wasserbau"                                  
    ##  [2] "Development Engineer Motion"                                                                
    ##  [3] "Responsable agence FS Vétroz"                                                               
    ##  [4] "2 PhD student positions in environmental soil chemistry (m/f/d)"                            
    ##  [5] "Wirtschaftsinformatikerin / Wirtschaftsinformatiker"                                        
    ##  [6] "Managing Director ETH Zurich | Space"                                                       
    ##  [7] "IT Operations Manager (w/m/d)"                                                              
    ##  [8] "Jurist:in Baurecht / Mandatsleitung 60-100%"                                                
    ##  [9] "Solution Architect:in 80–100 %"                                                             
    ## [10] "Quality Engineer"                                                                           
    ## [11] "217 .NET C# E ntwickler"                                                                    
    ## [12] "Data-Engineer/Data-Architect, 80–100 % (w/m/d)"                                             
    ## [13] "ProjektleiterIn in Schaffhausen"                                                            
    ## [14] "2 PhD Positions in Spine Biomechanics"                                                      
    ## [15] "Responsable du Service Bâtiments et Infrastructures"                                        
    ## [16] "Netzelektriker, Montage-Elektriker, Automatiker oder Polymechaniker (m/w/d) in Niedergösgen"
    ## [17] "Business Architekt/-in 80 - 100%"                                                           
    ## [18] "Responsable Qualité & Développement produits (h/f)"                                         
    ## [19] "IT Security Spezialist/-in 100%"                                                            
    ## [20] "Customer Support Engineer - Mexico, Brazil"

``` r
# drop NaN rows
df <- clean_df %>%
    filter(!is.na(job_title_cleaned) & !is.na(publication_date))%>%# drop rows with no cleaned job title, since they are probably no IT related jobs 
    filter(publication_date > as.Date("2025-01-01"))%>%
    filter(max_years <= 20) # outliers

    
glimpse(df)
```

    ## Rows: 337
    ## Columns: 26
    ## $ company               <chr> "ITech Consult AG", "Hug Engineering AG", "Persi~
    ## $ contract_type         <chr> "Freelance", "Unlimited employment", "Unlimited ~
    ## $ descriptions          <list> [<data.frame[3 x 3]>], [<data.frame[3 x 3]>], [~
    ## $ downloaded            <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, ~
    ## $ job_title             <chr> "Fullstack Applikationsentwickler(in) (Java, Ang~
    ## $ language              <chr> "German (Intermediate), English (Basic knowledge~
    ## $ place_of_work         <chr> "Zürich", "Im Geren 14, 8352 Elsau", "Sursee", "~
    ## $ publication_date      <date> 2025-02-05, 2025-01-08, 2025-02-03, 2025-01-16,~
    ## $ rating                <dbl> 6, NA, NA, NA, 7, NA, 2, NA, NA, NA, 4, NA, 8, N~
    ## $ reviewed              <dbl> 1, NA, NA, NA, 1, NA, 1, NA, NA, NA, 1, NA, 1, N~
    ## $ salary                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    ## $ search_query          <chr> "software engineer", "software engineer", "data ~
    ## $ url                   <chr> "https://www.jobs.ch/en/vacancies/detail/23d53c6~
    ## $ website               <chr> NA, NA, NA, NA, NA, NA, "Jobs", NA, NA, NA, "Job~
    ## $ workload              <chr> "80 – 100%", "100%", "100%", "80 – 100%", "80 – ~
    ## $ career_stage_cleaned  <fct> NA, NA, NA, NA, NA, NA, senior, NA, NA, NA, NA, ~
    ## $ canton                <fct> Zürich, NA, NA, NA, NA, NA, NA, NA, Schaffhausen~
    ## $ programming_languages <list> <Java, SQL>, <C, C++>, "No Programming Language~
    ## $ frameworks            <list> <Angular, Spring>, "No Frameworks", "No Framewo~
    ## $ tools                 <list> <Git, Jenkins>, "No Tools", "No Tools", <CI/CD,~
    ## $ operating_systems     <list> <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>,~
    ## $ years                 <list> 5, 2, 4, 3, 5, 3, <3, 5>, 2, 3, 2, <3, 5>, 5, 3~
    ## $ education             <list> "Vocational", <>, "Vocational", <>, <>, "PhD", ~
    ## $ job_title_cleaned     <fct> Applikationsentwickler, Automation Engineer, Sys~
    ## $ job_category          <fct> Software Engineer, Software Engineer, Cloud/Syst~
    ## $ max_years             <dbl> 5, 2, 4, 3, 5, 3, 5, 2, 3, 2, 5, 5, 3, 5, 3, 2, ~

## Visualize

``` r
df %>%
    count(publication_date) %>%
    ggplot(aes(publication_date, n)) +
    geom_line(aes(color = "All Listings"), linewidth = 1, alpha=0.5) +  # Line for all listings
    geom_line(data = df %>% filter(reviewed == TRUE) %>% count(publication_date), 
            aes(publication_date, n, color = "Reviewed Listings"), linewidth = 1, alpha=0.5) +  # Line for reviewed listings
    geom_point(aes(publication_date, n), alpha=0.5)+
    labs(title = "Number of scraped IT Job Listings per Day",
        x = "Date",
        y = "") +
    scale_color_manual(name = "Legend", values = c("All Listings" = "#1182ba", "Reviewed Listings" = "#71d171"))
```

![](analysis_files/figure-gfm/time-jobs-1.png)<!-- -->

``` r
df %>%
  ggplot() +
  geom_bar(mapping=aes(x=rating), alpha = 0.7)+
  labs(x = "Rating",
       y = "Count") 
```

    ## Warning: Removed 198 rows containing non-finite values (`stat_count()`).

![](analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
df %>%
  ggplot() +
  geom_bar(mapping=aes(y=job_category), alpha = 0.7)+
  labs(y = "Job Category",
       x = "Count") 
```

![](analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
p <- ggplot(df, aes(max_years, career_stage_cleaned))  +
    geom_boxplot() +
    labs(y = "Career Stage",
        x = "Min years of experience") 
print(p)
```

![](analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
df %>%
    unnest(programming_languages) %>%         # Unnest the list column
    count(programming_languages) %>%           # Count occurrences of each language
    ggplot(aes(x = n, y = programming_languages)) +  
    geom_bar(stat = "identity", alpha = 0.7) +
    labs(x = "Count", y = "Programming Language")
```

![](analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
df %>%
    unnest(programming_languages) %>% 
    ggplot(aes(x = rating , y = programming_languages)) +
    geom_boxplot() +
    labs(x = "Rating", y = "Programming Languages")
```

    ## Warning: Removed 338 rows containing non-finite values (`stat_boxplot()`).

![](analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
df %>%
    count(search_query) 
```

    ## # A tibble: 4 x 2
    ##   search_query          n
    ##   <chr>             <int>
    ## 1 all jobs             42
    ## 2 data engineer        86
    ## 3 data scientist        9
    ## 4 software engineer   200

``` r
df %>%
    count(job_title_cleaned) %>%
    arrange(desc(n))
```

    ## # A tibble: 64 x 2
    ##    job_title_cleaned        n
    ##    <fct>                <int>
    ##  1 Software Engineer       64
    ##  2 System Engineer         51
    ##  3 Software Developer      17
    ##  4 Data Engineer           11
    ##  5 DevOps Engineer         10
    ##  6 Softwareentwickler      10
    ##  7 Application Engineer     9
    ##  8 Automation Engineer      9
    ##  9 Entwickler:in            9
    ## 10 Security Engineer        9
    ## # i 54 more rows

``` r
df %>%
    count(job_category) %>%
    ggplot(aes(x = n, y = job_category)) +
    geom_bar(stat = "identity")
```

![](analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
df %>%
    ggplot(aes(x = rating , y = job_category)) +
    geom_boxplot(alpha = 0.7) +
    labs(x = "Rating", y = "Job Category")
```

    ## Warning: Removed 198 rows containing non-finite values (`stat_boxplot()`).

![](analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
df %>%
    ggplot(aes(x = rating , y = canton)) +
    geom_boxplot(alpha = 0.7) +
    labs(x = "Rating", y = "Canton")
```

    ## Warning: Removed 198 rows containing non-finite values (`stat_boxplot()`).

![](analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
df %>%
    ggplot(aes(x = max_years, y = rating )) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(x = "Max Years of Experience", y = " Rating", title = "Correlation between Rating and Max Years of Experience")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 198 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 198 rows containing missing values (`geom_point()`).

![](analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Fit Model

- Linear model is probably not the best for this usecase, classification
  with kNN / SVM might be more appropriate (TODO)

``` r
df_one_hot <- df %>%
    select(job_title, url, job_category, career_stage_cleaned, programming_languages, frameworks, max_years, rating ) %>% 
    # handle NA values as an "Non Value" factor
    mutate(
        job_category = factor(replace_na(as.character(job_category), "No Value")),
        career_stage_cleaned = factor(replace_na(as.character(career_stage_cleaned), "No Value")))
    
df_one_hot <- df_one_hot%>% 
    mutate(programming_languages = lapply(programming_languages, factor)) %>%
    unnest(programming_languages) %>%
    distinct() %>%
    mutate(temp = 1) %>%
    pivot_wider(names_from = programming_languages, values_from = temp, values_fill = list(temp = 0))


df_one_hot <- df_one_hot %>%   
    unnest(frameworks) %>%
    distinct() %>%
    mutate(temp = 1) %>%
    pivot_wider(names_from = frameworks, values_from = temp, values_fill = list(temp = 0))


glimpse(df_one_hot)
```

    ## Rows: 337
    ## Columns: 44
    ## $ job_title                  <chr> "Fullstack Applikationsentwickler(in) (Java~
    ## $ url                        <chr> "https://www.jobs.ch/en/vacancies/detail/23~
    ## $ job_category               <fct> Software Engineer, Software Engineer, Cloud~
    ## $ career_stage_cleaned       <fct> No Value, No Value, No Value, No Value, No ~
    ## $ max_years                  <dbl> 5, 2, 4, 3, 5, 3, 5, 2, 3, 2, 5, 5, 3, 5, 3~
    ## $ rating                     <dbl> 6, NA, NA, NA, 7, NA, 2, NA, NA, NA, 4, NA,~
    ## $ Java                       <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1~
    ## $ SQL                        <dbl> 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1~
    ## $ C                          <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ `C++`                      <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0~
    ## $ `No Programming Languages` <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0~
    ## $ `C#`                       <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0~
    ## $ Python                     <dbl> 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0~
    ## $ MATLAB                     <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Shell                      <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Lua                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0~
    ## $ CSS                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0~
    ## $ HTML                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1~
    ## $ TypeScript                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1~
    ## $ JavaScript                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1~
    ## $ Bash                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Go                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ PowerShell                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Kotlin                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ PHP                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Dart                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Perl                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Rust                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Swift                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Angular                    <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1~
    ## $ Spring                     <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1~
    ## $ `No Frameworks`            <dbl> 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0~
    ## $ React                      <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1~
    ## $ Next.js                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0~
    ## $ Vue                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1~
    ## $ .NET                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ ASP.NET                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Node.js                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ NestJS                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Svelte                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Django                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ FastAPI                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Symfony                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Laravel                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~

``` r
df_one_hot %>%
    mutate(has_rating = !is.na(rating)) %>%
    group_by(has_rating) %>%
    summarise(
        count = n())
```

    ## # A tibble: 2 x 2
    ##   has_rating count
    ##   <lgl>      <int>
    ## 1 FALSE        198
    ## 2 TRUE         139

``` r
df_model <- df_one_hot %>% filter(!is.na(rating))
df_to_predict <- df_one_hot %>% filter(is.na(rating))
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(df_model), replace=TRUE, prob=c(0.8,0.2))
df_train  <- df_model[sample, ]
df_test   <- df_model[!sample, ]

# Fit a linear regression model
model <- lm(rating ~ . , data = df_train[,!colnames(df_train) %in% c("url", "job_title")])

# Summary of the model
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = rating ~ ., data = df_train[, !colnames(df_train) %in% 
    ##     c("url", "job_title")])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8550 -0.7376  0.0000  0.6296  2.7065 
    ## 
    ## Coefficients: (7 not defined because of singularities)
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          6.68376    1.87355   3.567 0.000632 ***
    ## job_categoryConsulting & Management -0.14697    0.63669  -0.231 0.818072    
    ## job_categoryData Engineer            3.11745    0.58773   5.304  1.1e-06 ***
    ## job_categoryDesign                   0.47384    1.03067   0.460 0.647035    
    ## job_categoryInfrastructure           0.34121    0.55159   0.619 0.538057    
    ## job_categorySecurity Engineer       -0.01600    0.60940  -0.026 0.979127    
    ## job_categorySoftware Engineer        0.58267    0.49984   1.166 0.247427    
    ## career_stage_cleanedNo Value        -3.38026    0.84960  -3.979 0.000159 ***
    ## career_stage_cleanedsenior          -3.23778    0.90101  -3.594 0.000581 ***
    ## max_years                           -0.11796    0.07406  -1.593 0.115429    
    ## Java                                -0.09001    0.60392  -0.149 0.881919    
    ## SQL                                  0.23989    0.48364   0.496 0.621337    
    ## C                                    0.24970    0.77700   0.321 0.748827    
    ## `C++`                               -0.85576    0.70056  -1.222 0.225712    
    ## `No Programming Languages`          -1.16501    0.65127  -1.789 0.077678 .  
    ## `C#`                                 0.61832    0.53702   1.151 0.253229    
    ## Python                              -0.13675    0.60222  -0.227 0.820979    
    ## MATLAB                               0.67566    1.07482   0.629 0.531506    
    ## Shell                               -0.89564    1.44772  -0.619 0.538017    
    ## Lua                                       NA         NA      NA       NA    
    ## CSS                                 -6.18849    1.97582  -3.132 0.002475 ** 
    ## HTML                                 4.98106    2.05650   2.422 0.017846 *  
    ## TypeScript                          -1.38250    1.30521  -1.059 0.292902    
    ## JavaScript                           0.57837    1.03473   0.559 0.577859    
    ## Bash                                -0.98907    1.40362  -0.705 0.483203    
    ## Go                                   1.13744    1.09158   1.042 0.300750    
    ## PowerShell                          -1.64918    1.05774  -1.559 0.123170    
    ## Kotlin                               4.00209    2.02997   1.972 0.052355 .  
    ## PHP                                  1.34926    1.82929   0.738 0.463069    
    ## Dart                                 0.36863    1.07273   0.344 0.732081    
    ## Perl                                -2.32357    2.16567  -1.073 0.286751    
    ## Rust                                      NA         NA      NA       NA    
    ## Swift                                     NA         NA      NA       NA    
    ## Angular                              0.26282    0.78065   0.337 0.737306    
    ## Spring                               0.76476    1.06688   0.717 0.475710    
    ## `No Frameworks`                     -0.16502    1.34260  -0.123 0.902510    
    ## React                                1.92258    0.76858   2.501 0.014549 *  
    ## Next.js                              3.44331    1.34092   2.568 0.012222 *  
    ## Vue                                 -0.98929    1.66423  -0.594 0.554006    
    ## .NET                                 1.11343    1.93727   0.575 0.567186    
    ## ASP.NET                             -2.98818    2.19825  -1.359 0.178111    
    ## Node.js                                   NA         NA      NA       NA    
    ## NestJS                                    NA         NA      NA       NA    
    ## Svelte                               1.73224    1.64111   1.056 0.294572    
    ## Django                               0.60445    1.95108   0.310 0.757569    
    ## FastAPI                                   NA         NA      NA       NA    
    ## Symfony                              3.16158    2.92909   1.079 0.283881    
    ## Laravel                                   NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.271 on 75 degrees of freedom
    ## Multiple R-squared:  0.7111, Adjusted R-squared:  0.557 
    ## F-statistic: 4.615 on 40 and 75 DF,  p-value: 5.972e-09

``` r
# Predict on test set
predictions_test <- predict(model, newdata = df_test[,!colnames(df_test) %in% c("url", "job_title")])
```

    ## Warning in predict.lm(model, newdata = df_test[, !colnames(df_test) %in% :
    ## prediction from a rank-deficient fit may be misleading

``` r
df_test <- df_test %>% mutate(predicted_rating = predictions_test) 
df_test %>%
    ggplot(aes(x = rating, y = predicted_rating)) +
    geom_point(alpha=0.5, size = 3) +
    geom_abline(intercept = 0, slope = 1, color = "#2086b9", alpha = 0.5, linewidth = 2) +
    labs(x = "Actual Rating", y = "Predicted Rating")
```

![](analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# assert that there are no levels in the prediction data that are not in the model
#model$xlevels[["url"]] <- union(model$xlevels[["url"]], levels(df_one_hot$url))
model$xlevels[["job_category"]] <- union(model$xlevels[["job_category"]], levels(df_one_hot$job_category))
#model$xlevels[["career_stage_cleaned"]] <- union(model$xlevels[["career_stage_cleaned"]], levels(df_one_hot$career_stage_cleaned))
#model$xlevels[["programming_languages"]] <- union(model$xlevels[["programming_languages"]], levels(df_one_hot$programming_languages))
#model$xlevels[["frameworks"]] <- union(model$xlevels[["frameworks"]], levels(df_one_hot$frameworks))

# prediction from a rank-deficient fit may be misleading -> we need more data
predictions <- predict(model, newdata = df_to_predict[,!colnames(df_to_predict) %in% c("url", "job_title")])
```

    ## Warning in predict.lm(model, newdata = df_to_predict[, !colnames(df_to_predict)
    ## %in% : prediction from a rank-deficient fit may be misleading

``` r
# Add predictions to df_to_predict
df_to_predict <- df_to_predict %>%
    mutate(predicted_rating = predictions)

# Glimpse the predictions
glimpse(df_to_predict)
```

    ## Rows: 198
    ## Columns: 45
    ## $ job_title                  <chr> "Automation Engineer Aftersales (f/m/d)", "~
    ## $ url                        <chr> "https://www.jobs.ch/en/vacancies/detail/b4~
    ## $ job_category               <fct> Software Engineer, Cloud/System Engineer, S~
    ## $ career_stage_cleaned       <fct> No Value, No Value, No Value, No Value, No ~
    ## $ max_years                  <dbl> 2, 4, 3, 3, 2, 3, 2, 5, 5, 3, 2, 2, 2, 2, 2~
    ## $ rating                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ Java                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0~
    ## $ SQL                        <dbl> 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0~
    ## $ C                          <dbl> 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ `C++`                      <dbl> 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ `No Programming Languages` <dbl> 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0~
    ## $ `C#`                       <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1~
    ## $ Python                     <dbl> 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0~
    ## $ MATLAB                     <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Shell                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Lua                        <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ CSS                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1~
    ## $ HTML                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1~
    ## $ TypeScript                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1~
    ## $ JavaScript                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1~
    ## $ Bash                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0~
    ## $ Go                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ PowerShell                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Kotlin                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ PHP                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Dart                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Perl                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Rust                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Swift                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Angular                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0~
    ## $ Spring                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0~
    ## $ `No Frameworks`            <dbl> 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0~
    ## $ React                      <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1~
    ## $ Next.js                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Vue                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0~
    ## $ .NET                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0~
    ## $ ASP.NET                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0~
    ## $ Node.js                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0~
    ## $ NestJS                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Svelte                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Django                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ FastAPI                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Symfony                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Laravel                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ predicted_rating           <dbl> 2.8791856, 1.5016417, 5.9364392, 3.4368875,~

## Show most promising non-rated predictions

``` r
# Add a column with clickable URLs
df_to_predict <- df_to_predict %>%
    mutate(clickable_url = paste0("[", url, "](", url, ")"))

# Show most promising non-rated predictions with clickable URLs
df_to_predict %>%
    arrange(desc(predicted_rating)) %>%
    select(job_title, clickable_url, predicted_rating) %>%
    slice_head(n = 20) %>%
    knitr::kable()
```

| job_title                                           | clickable_url                                                                   | predicted_rating |
|:----------------------------------------------------|:--------------------------------------------------------------------------------|-----------------:|
| Senior Fullstack Entwickler:in PHP (60-100%)        | <https://www.jobs.ch/en/vacancies/detail/007316a1-38e9-445a-a27c-ddfa5a556409/> |        12.221208 |
| Software Engineer (w/m/d) 80 – 100%                 | <https://www.jobs.ch/en/vacancies/detail/830e5c06-c6bb-4c1b-8b92-0850edcd5a00/> |         9.819979 |
| Software Engineer mit Fokus Frontend (w/m/d)        | <https://www.jobs.ch/en/vacancies/detail/0027f781-dbb0-466b-ad87-4e25b18e898d/> |         9.337023 |
| Senior Fullstack Entwickler:in .NET (60-100%)       | <https://www.jobs.ch/en/vacancies/detail/2c91d743-26a9-4c27-ab80-bada14698093/> |         8.080188 |
| ICT Professional Applikations-Entwickler:in (100%)  | <https://www.jobs.ch/en/vacancies/detail/65fc362a-9622-493c-a3df-6022896bb52b/> |         6.459069 |
| Senior Data Engineer                                | <https://www.jobs.ch/en/vacancies/detail/0497859b-aeb2-4f8e-8b0d-2d56b3d00452/> |         6.430914 |
| Senior Data Engineer (m/w/d)                        | <https://www.jobs.ch/en/vacancies/detail/83382fb2-8c64-4596-a2ea-4e2196832dd8/> |         6.402383 |
| Senior Database Engineer 80-100%                    | <https://www.jobs.ch/en/vacancies/detail/b25004b1-d527-4761-b0d2-aa1bbb153880/> |         6.402383 |
| Software Engineer (80 - 100%)                       | <https://www.jobs.ch/en/vacancies/detail/a3ca254b-e98f-4886-9025-d231c58a9f34/> |         6.376428 |
| Senior AI Engineer & Consultant (a)                 | <https://www.jobs.ch/en/vacancies/detail/57ae08b5-2a69-4af2-905e-5b7a103da5af/> |         6.363566 |
| Senior AI Engineer & Consultant (a)                 | <https://www.itjobs.ch/jobs/senior-ai-engineer-consultant-a/120535/>            |         6.363566 |
| Research Software Engineer                          | <https://www.jobs.ch/en/vacancies/detail/9b130309-c525-4ae5-ab78-9e6a464d4e39/> |         6.344662 |
| Research Software Engineer                          | <https://www.itjobs.ch/jobs/research-software-engineer/118996/>                 |         6.344662 |
| Frontend Engineer (part-time possible, all genders) | <https://www.jobs.ch/en/vacancies/detail/9823e9bb-a8fd-4809-9da9-73fd0e7ede24/> |         6.324010 |
| Data Engineer                                       | <https://www.jobs.ch/en/vacancies/detail/d94dcbd4-d512-44b3-b7c9-388b4d0cee46/> |         6.259905 |
| Data Engineer / Data Analyst - Azure IoT (80-100%)  | <https://www.jobs.ch/en/vacancies/detail/a19e5f73-f3f5-4549-9040-794c40e14c5b/> |         6.123152 |
| Teamlead Data (w/m/d)                               | <https://www.jobs.ch/en/vacancies/detail/7ffad37d-4f1c-4abd-8ab7-1929c82fdd87/> |         6.023991 |
| Senior Frontend Developer                           | <https://www.jobs.ch/en/vacancies/detail/dd00a2de-57bd-4420-b7bb-3864f0ee14cd/> |         5.939807 |
| Fullstack Engineer 80 - 100 % (f/m/d)               | <https://www.jobs.ch/en/vacancies/detail/73a64291-f310-4029-ab10-997ddae30f90/> |         5.936439 |
| Senior Data Engineer                                | <https://www.jobs.ch/en/vacancies/detail/da56fc27-2b25-4eaa-aec0-b04705b9c7db/> |         5.911759 |

## Show highest rated jobs

``` r
# Add a column with clickable URLs
df_model <- df_model %>%
    mutate(clickable_url = paste0("[", url, "](", url, ")"))

# Show most promising non-rated predictions with clickable URLs
df_model %>%
    arrange(desc(rating)) %>%
    select(job_title, clickable_url, rating) %>%
    slice_head(n = 20) %>%
    knitr::kable()
```

| job_title                                                                        | clickable_url                                                                     | rating |
|:---------------------------------------------------------------------------------|:----------------------------------------------------------------------------------|-------:|
| Software Engineer Full-Stack – React Web / C# – 80% – Zürich – Digital Democracy | <https://www.jobs.ch/en/vacancies/detail/c1951c65-086f-4d60-a301-b9afbdb28f40/>   |      8 |
| Fullstack Java/Angular Developer                                                 | <https://www.jobs.ch/en/vacancies/detail/570c3b99-db96-4d53-9d3f-c649666df7fd/>   |      8 |
| (Junior) Software Engineer C#/.NET (80 – 100%) (a)                               | <https://www.jobs.ch/en/vacancies/detail/897ff339-03e9-49c1-8e23-c6861db9f556/>   |      8 |
| Data Engineer                                                                    | <https://www.jobs.ch/en/vacancies/detail/02b55667-90a0-4e9a-801c-01d8bdce3caa/>   |      8 |
| Softwareentwickler/in - Python (m/w/d)                                           | <https://www.jobs.ch/en/vacancies/detail/4f886ee9-7fad-4624-8b09-852ae633130d/>   |      8 |
| Java / Full Stack Developer (part-time possible, all genders)                    | <https://www.jobs.ch/en/vacancies/detail/285a0cf4-2c45-4c66-be24-d71209e34e06/>   |      8 |
| Data Engineer for Clinical Trials 80 - 100 % (f/m/d)                             | <https://www.jobs.ch/en/vacancies/detail/5543d29f-fa16-4f46-8239-8d9199c4fbbe/>   |      8 |
| Data Analyst (alle)                                                              | <https://www.jobs.ch/en/vacancies/detail/f7345123-d71c-4a47-b6dc-dfe5f17acbc7/>   |      7 |
| Erfahrener Java Software Engineer 80 - 100% (m/w/d)                              | <https://www.jobs.ch/en/vacancies/detail/704f6930-af1f-4253-92d4-617c4f615df8/>   |      7 |
| Software Engineer als Full Stack Web Developer                                   | <https://www.jobs.ch/en/vacancies/detail/127a7968-522e-4419-8773-ebea399215b6/>   |      7 |
| Frontend Engineer (part-time possible, all genders)                              | <https://www.jobs.ch/en/vacancies/detail/25436ec9-2273-4cb3-b645-d869d06f146f/>   |      7 |
| Fullstack Applikationsentwickler(in) (Java, Angular)                             | <https://www.jobs.ch/en/vacancies/detail/23d53c66-128e-494c-bea8-4f787932be04/>   |      6 |
| Software Developer Full-Stack JavaEE & Angular (w/m/d)                           | <https://www.jobs.ch/en/vacancies/detail/092b1556-29fd-4366-93e1-4b63e75651b6/>   |      6 |
| Junior Requirements Engineer (60 – 100%) (a)                                     | <https://www.jobs.ch/en/vacancies/detail/5acb2c12-a282-4441-b949-8e6ea4eae666/>   |      6 |
| Frontend Developer bei Schweizer Startup                                         | <https://www.jobs.ch/en/vacancies/detail/e0b64094-e9cc-4191-ae8d-06163dd98440/>   |      6 |
| Software Systems Engineer - Business Support Services Technology                 | <https://www.jobs.ch/en/vacancies/detail/6f43cbd9-8576-49f5-af4d-9f4498f28d0e/>   |      6 |
| Fullstack Entwickler (Java & Angular)                                            | <https://www.jobs.ch/en/vacancies/detail/bebee9bf-861f-489a-83c4-f5a32b9380be/>   |      6 |
| AI Risk Manager / Senior Data Scientist (all genders)                            | <https://www.jobs.ch/en/vacancies/detail/ed39bd0a-307e-4601-beb4-43cf5309f89d/>   |      6 |
| Senior Fullstack Engineer (C#, Angular, Python) (a)                              | <https://www.itjobs.ch/jobs/senior-fullstack-engineer-c-angular-python-a/121249/> |      6 |
| Data Engineer (alle)                                                             | <https://www.jobs.ch/en/vacancies/detail/920e44b5-a2a5-4a26-8589-bd5fb929c2d2/>   |      5 |
