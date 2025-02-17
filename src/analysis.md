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
glimpse(raw_df)
```

    ## Rows: 3,936
    ## Columns: 25
    ## $ company               <chr> NA, "BBT Software AG", "VAT Vakuumventile AG", "~
    ## $ contract_type         <chr> NA, NA, NA, "Unlimited employment", NA, NA, NA, ~
    ## $ descriptions          <list> [<data.frame[0 x 0]>], [<data.frame[1 x 1]>], [~
    ## $ downloaded            <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, ~
    ## $ job_title             <chr> "Bauingenieur*in (60-100%) Bereich Konstruktiver~
    ## $ language              <chr> NA, NA, NA, NA, NA, NA, NA, NA, "German (Interme~
    ## $ place_of_work         <chr> NA, "Root D4", NA, "Ringstrasse 39, 4106 Therwil~
    ## $ publication_date      <chr> NA, "06 February 2025", NA, "07 February 2025", ~
    ## $ rating                <dbl> NA, NA, NA, 6, NA, NA, NA, NA, NA, NA, NA, 4, NA~
    ## $ reviewed              <dbl> NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, 1, NA~
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
df <- raw_df %>%
    mutate(publication_date = dmy(publication_date), 
        job_title_cleaned = factor(job_title_cleaned),
        career_stage_cleaned = factor(career_stage_cleaned),
        canton = factor(canton),
        programming_languages = lapply(programming_languages, factor),
        frameworks = lapply(frameworks, factor),
        tools = lapply(tools, factor),
        operating_systems = lapply(operating_systems, factor),
        max_years = sapply(years, function(x) if (length(x) == 0) NaN else max(as.numeric(x), na.rm = TRUE))) # get min number of years, and replace nan with 0

colSums(is.na(df))
```

    ##               company         contract_type          descriptions 
    ##                   923                  2669                     0 
    ##            downloaded             job_title              language 
    ##                     0                     0                  2913 
    ##         place_of_work      publication_date                rating 
    ##                  2077                  2084                  3518 
    ##              reviewed                salary          search_query 
    ##                  3518                  3815                     0 
    ##                   url               website              workload 
    ##                     0                  2525                  2642 
    ##  career_stage_cleaned                canton programming_languages 
    ##                  3266                  3131                     0 
    ##            frameworks                 tools     operating_systems 
    ##                     0                     0                     0 
    ##                 years             education     job_title_cleaned 
    ##                     0                     0                  2331 
    ##          job_category             max_years 
    ##                  2331                  3294

Check if some IT key words in listing title were missed during
preprocessing, and therefore have a NaN value in the `job_title_cleaned`
attribute.

``` r
df %>%
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
df <- df %>%
    filter(!is.na(job_title_cleaned) & !is.na(publication_date))%>%# drop rows with no cleaned job title, since they are probably no IT related jobs 
    filter(publication_date > as.Date("2025-01-01"))%>%
    filter(max_years <= 20) # outliers

    
glimpse(df)
```

    ## Rows: 306
    ## Columns: 26
    ## $ company               <chr> "ITech Consult AG", "Hug Engineering AG", "Persi~
    ## $ contract_type         <chr> "Freelance", "Unlimited employment", "Unlimited ~
    ## $ descriptions          <list> [<data.frame[3 x 3]>], [<data.frame[3 x 3]>], [~
    ## $ downloaded            <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, ~
    ## $ job_title             <chr> "Fullstack Applikationsentwickler(in) (Java, Ang~
    ## $ language              <chr> "German (Intermediate), English (Basic knowledge~
    ## $ place_of_work         <chr> "Zürich", "Im Geren 14, 8352 Elsau", "Sursee", "~
    ## $ publication_date      <date> 2025-02-05, 2025-01-08, 2025-02-03, 2025-01-16,~
    ## $ rating                <dbl> NA, NA, NA, NA, 7, NA, 2, NA, NA, NA, 4, NA, NA,~
    ## $ reviewed              <dbl> NA, NA, NA, NA, 1, NA, 1, NA, NA, NA, 1, NA, NA,~
    ## $ salary                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    ## $ search_query          <chr> "software engineer", "software engineer", "data ~
    ## $ url                   <chr> "https://www.jobs.ch/en/vacancies/detail/23d53c6~
    ## $ website               <chr> NA, NA, NA, NA, NA, NA, "Jobs", NA, NA, NA, "Job~
    ## $ workload              <chr> "80 – 100%", "100%", "100%", "80 – 100%", "80 – ~
    ## $ career_stage_cleaned  <fct> NA, NA, NA, NA, NA, NA, senior, NA, NA, NA, NA, ~
    ## $ canton                <fct> Zürich, NA, NA, NA, NA, NA, NA, NA, Schaffhausen~
    ## $ programming_languages <list> <Java, SQL>, <C, C++>, <>, <C#, Python>, SQL, <~
    ## $ frameworks            <list> <Angular, Spring>, <>, <>, React, <>, <>, <>, <~
    ## $ tools                 <list> <Git, Jenkins>, <>, <>, <CI/CD, Docker, Kuberne~
    ## $ operating_systems     <list> <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>,~
    ## $ years                 <list> 5, 2, 4, 3, 5, 3, <3, 5>, 2, 3, 2, <3, 5>, 5, 3~
    ## $ education             <list> "Vocational", <>, "Vocational", <>, <>, "PhD", ~
    ## $ job_title_cleaned     <fct> Applikationsentwickler, Automation Engineer, Sys~
    ## $ job_category          <chr> "Software Engineer", "Software Engineer", "Cloud~
    ## $ max_years             <dbl> 5, 2, 4, 3, 5, 3, 5, 2, 3, 2, 5, 5, 3, 5, 3, 2, ~

## Visualize

``` r
df %>%
  count(publication_date) %>%
  ggplot(aes(publication_date, n)) +
  geom_line(aes(color = "All Listings"), linewidth = 1, alpha=0.5) +  # Line for all listings
  geom_line(data = df %>% filter(reviewed == TRUE) %>% count(publication_date), 
            aes(publication_date, n, color = "Reviewed Listings"), linewidth = 1, alpha=0.5) +  # Line for reviewed listings
  labs(title = "Number of scraped IT Job Listings per Day",
       x = "Date",
       y = "") +
    scale_color_manual(name = "Legend", values = c("All Listings" = "blue", "Reviewed Listings" = "red"))
```

![](analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
df %>%
  ggplot() +
  geom_bar(mapping=aes(x=rating), alpha = 0.7)+
  labs(x = "Rating",
       y = "Count") 
```

    ## Warning: Removed 221 rows containing non-finite values (`stat_count()`).

![](analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
df %>%
  ggplot() +
  geom_bar(mapping=aes(y=job_category), alpha = 0.7)+
  labs(y = "Job Category",
       x = "Count") 
```

![](analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
p <- ggplot(df, aes(max_years, career_stage_cleaned))  +
    geom_boxplot() +
    labs(y = "Career Stage",
        x = "Min years of experience") 
print(p)
```

![](analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
df %>%
    unnest(programming_languages) %>%         # Unnest the list column
    count(programming_languages) %>%           # Count occurrences of each language
    ggplot(aes(x = n, y = programming_languages)) +  
    geom_bar(stat = "identity", alpha = 0.7) +
    labs(x = "Count", y = "Programming Language")
```

![](analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
df %>%
    count(search_query) 
```

    ##        search_query   n
    ## 1          all jobs  38
    ## 2     data engineer  82
    ## 3    data scientist   9
    ## 4 software engineer 177

``` r
df %>%
    count(job_title_cleaned) 
```

    ##            job_title_cleaned  n
    ## 1                AI Engineer  2
    ## 2          Angular Developer  1
    ## 3       Application Engineer  7
    ## 4     Applikationsentwickler  2
    ## 5        Automation Engineer  9
    ## 6          Backend Developer  2
    ## 7           Backend Engineer  1
    ## 8            Cloud-Architekt  1
    ## 9            Cloud Architect  1
    ## 10            Cloud Engineer  5
    ## 11           Computer Vision  1
    ## 12    Cybersecurity Engineer  1
    ## 13                 Data & AI  1
    ## 14              Data Analyst  6
    ## 15            Data Architect  1
    ## 16             Data Engineer 10
    ## 17            Data Scientist  2
    ## 18         Database Engineer  1
    ## 19           Design Engineer  5
    ## 20           DevOps Engineer 10
    ## 21             Entwickler:in  8
    ## 22        Frontend Developer  5
    ## 23         Frontend Engineer  2
    ## 24      Full Stack Developer  2
    ## 25       Fullstack Developer  1
    ## 26        Fullstack Engineer  3
    ## 27      Fullstack Entwickler  2
    ## 28             ICT-Architekt  3
    ## 29             ICT Architekt  3
    ## 30       IT-Security Manager  1
    ## 31             IT Management  1
    ## 32            Java Developer  1
    ## 33       Middleware Engineer  1
    ## 34          Network Engineer  6
    ## 35         Platform Engineer  4
    ## 36          Projektleiter:in  3
    ## 37                       R&D  2
    ## 38     Requirements Engineer  5
    ## 39         Research Engineer  1
    ## 40       Salesforce Engineer  1
    ## 41         Security Engineer  9
    ## 42          Senior Developer  2
    ## 43 Site Reliability Engineer  3
    ## 44       Software-Entwickler  1
    ## 45        Software Architect  4
    ## 46        Software Developer 14
    ## 47         Software Engineer 62
    ## 48       Software Entwickler  6
    ## 49         Softwarearchitekt  1
    ## 50          Softwareengineer  2
    ## 51        Softwareentwickler  9
    ## 52         Solution Engineer  4
    ## 53          Storage Engineer  1
    ## 54           System Engineer 46
    ## 55           Systemingenieur  2
    ## 56          Systems Engineer  8
    ## 57             Teamlead Data  1
    ## 58            Technical Lead  1
    ## 59             Test Engineer  4
    ## 60               UX Designer  1
    ## 61             Web Developer  1

``` r
df %>%
    count(job_category) 
```

    ##              job_category   n
    ## 1   Cloud/System Engineer  58
    ## 2 Consulting & Management  17
    ## 3           Data Engineer  27
    ## 4                  Design   6
    ## 5          Infrastructure  33
    ## 6       Security Engineer  16
    ## 7       Software Engineer 149
