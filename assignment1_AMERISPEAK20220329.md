assignment1_SSAD1_template.Rmd
================
Junyou
2/4/2022

-   [Question_Democrats 1.1](#question_democrats-11)
-   [Question_Democrats 1.2](#question_democrats-12)
-   [Question_Democrats 1.3](#question_democrats-13)
-   [Question_Democrats 1.3.1](#question_democrats-131)
-   [Question_naloxone 2.1](#question_naloxone-21)
-   [Question_naloxone 2.2](#question_naloxone-22)
-   [Question_naloxone 2.3](#question_naloxone-23)
-   [Question_naloxone 2.3.1](#question_naloxone-231)
-   [Question_mandatory 3.1](#question_mandatory-31)
-   [Question_Mandatory 3.2](#question_mandatory-32)
-   [Question_Mandatory 3.3](#question_mandatory-33)
-   [Question_Mandatory 3.3.1](#question_mandatory-331)

Template regression program–Yeah we don’t need all these libraries.

``` r
#library(foreign)
#library(systemfit)
#library(stargazer)
#library(pander)
#library(sjPlot)
#library(pollster)
#library(lavaan)
#library(qwraps2)
#library(arsenal)
library(gtsummary) 
library(ggplot2)
library(glue)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.1.2     ✓ forcats 0.5.1
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
options(width = 150)
options(qwraps2_markup = "markdown")
knitr::opts_chunk$set(echo = TRUE)
```

Read the csv file

``` r
## read file
file_name<-"data_Amerispeakw5_SSAD579.csv"
v_analysis_raw <- glue('~/Desktop/Regression R/regression_Assignment/{file_name}')%>%
              read.csv()
## make a column to count people self-identified as republicans
v_analysis <- v_analysis_raw %>%
              mutate(drep = if_else(ddem == 0 & 
                                          dlean_DEM == 0 &
                                          dindependent_non_identified == 0 &
                                          dlean_rep == 0, 1, 0)) %>%
              mutate(drep = as.integer(drep))
str(head(v_analysis))
```

    ## 'data.frame':    6 obs. of  35 variables:
    ##  $ X                          : int  1 2 3 4 5 6
    ##  $ num_Mandatory              : int  4 2 3 5 4 4
    ##  $ num_Gov                    : int  3 2 3 4 2 4
    ##  $ num_Medicaid               : int  4 2 3 4 2 4
    ##  $ num_Naloxone               : int  5 2 3 4 2 4
    ##  $ CoBRAS_Scale               : num  2.75 4.38 3.25 3.75 3.88 ...
    ##  $ Stigma_Scale_Score         : num  3.67 3.33 3.17 4.33 4 ...
    ##  $ dmale                      : int  1 1 1 1 1 1
    ##  $ ddem                       : int  1 0 0 0 0 0
    ##  $ dlean_DEM                  : int  0 0 0 0 0 0
    ##  $ dindependent_non_identified: int  0 0 0 0 0 0
    ##  $ dlean_rep                  : int  0 1 0 1 0 0
    ##  $ drep                       : int  0 0 1 0 1 1
    ##  $ dless_HS                   : int  0 0 0 0 0 0
    ##  $ dHS                        : int  1 0 0 0 0 0
    ##  $ dsome_col                  : int  0 0 1 1 0 0
    ##  $ dage30_44                  : int  0 0 1 0 0 0
    ##  $ dage45_59                  : int  1 1 0 0 1 0
    ##  $ dage60_plus                : int  0 0 0 1 0 1
    ##  $ dNortheast                 : int  0 0 0 0 0 0
    ##  $ dMidwest                   : int  0 0 0 1 1 0
    ##  $ dWest                      : int  0 1 0 0 0 0
    ##  $ dSouth                     : int  1 0 1 0 0 1
    ##  $ race_BLACK                 : int  0 0 0 0 0 0
    ##  $ race_HISPANIC              : int  0 0 0 0 0 0
    ##  $ race_ASIAN                 : int  0 0 0 0 0 0
    ##  $ race_OTHERMIXED            : int  0 0 0 0 0 0
    ##  $ incomedummy_25to49K        : int  0 0 1 1 0 0
    ##  $ incomedummy_50to84K        : int  0 1 0 0 0 0
    ##  $ incomedummy_85to150K       : int  0 0 0 0 1 1
    ##  $ incomedummy_OVER150K       : int  0 0 0 0 0 0
    ##  $ dPersonal_use_ever         : int  1 0 0 0 0 0
    ##  $ dFamily_use_ever           : int  1 0 0 0 0 1
    ##  $ dPersonal_conviction_ever  : int  1 1 0 0 0 0
    ##  $ dFamily_conviction_ever    : int  1 1 0 1 0 0

``` r
## linear regression analysis
Medicaid_full <- lm(num_Medicaid ~ dmale +
                    ddem + 
                    dlean_DEM +
                    dindependent_non_identified +
                    dlean_rep +
                    dless_HS+
                    dHS+
                    dsome_col+
                    dage30_44+
                    dage45_59+
                    dage60_plus+
                    dNortheast+
                    dMidwest+dWest+
                    race_BLACK+
                    race_HISPANIC+
                    race_ASIAN+
                    race_OTHERMIXED+
                    incomedummy_25to49K+
                    incomedummy_50to84K+
                    incomedummy_85to150K+
                    incomedummy_OVER150K+
                    dPersonal_use_ever+
                    dFamily_use_ever+
                    dPersonal_conviction_ever+
                    dFamily_conviction_ever+
                    Stigma_Scale_Score+
                    CoBRAS_Scale,
                    data = v_analysis)

tbl_regression(Medicaid_full,exponentiate=FALSE)
```

<div id="ywehpbixpd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ywehpbixpd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ywehpbixpd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ywehpbixpd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ywehpbixpd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ywehpbixpd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ywehpbixpd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ywehpbixpd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ywehpbixpd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ywehpbixpd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ywehpbixpd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ywehpbixpd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ywehpbixpd .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ywehpbixpd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ywehpbixpd .gt_from_md > :first-child {
  margin-top: 0;
}

#ywehpbixpd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ywehpbixpd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ywehpbixpd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ywehpbixpd .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ywehpbixpd .gt_row_group_first td {
  border-top-width: 2px;
}

#ywehpbixpd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywehpbixpd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ywehpbixpd .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ywehpbixpd .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ywehpbixpd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywehpbixpd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ywehpbixpd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ywehpbixpd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ywehpbixpd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ywehpbixpd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywehpbixpd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ywehpbixpd .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywehpbixpd .gt_left {
  text-align: left;
}

#ywehpbixpd .gt_center {
  text-align: center;
}

#ywehpbixpd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ywehpbixpd .gt_font_normal {
  font-weight: normal;
}

#ywehpbixpd .gt_font_bold {
  font-weight: bold;
}

#ywehpbixpd .gt_font_italic {
  font-style: italic;
}

#ywehpbixpd .gt_super {
  font-size: 65%;
}

#ywehpbixpd .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ywehpbixpd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ywehpbixpd .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#ywehpbixpd .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#ywehpbixpd .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">-0.07</td>
<td class="gt_row gt_center">-0.19, 0.05</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.66</td>
<td class="gt_row gt_center">0.47, 0.85</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">dlean_DEM</td>
<td class="gt_row gt_center">0.48</td>
<td class="gt_row gt_center">0.24, 0.72</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">dindependent_non_identified</td>
<td class="gt_row gt_center">0.25</td>
<td class="gt_row gt_center">0.05, 0.46</td>
<td class="gt_row gt_center">0.016</td></tr>
    <tr><td class="gt_row gt_left">dlean_rep</td>
<td class="gt_row gt_center">0.11</td>
<td class="gt_row gt_center">-0.12, 0.33</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">-0.29</td>
<td class="gt_row gt_center">-0.60, 0.03</td>
<td class="gt_row gt_center">0.074</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">-0.16</td>
<td class="gt_row gt_center">-0.36, 0.05</td>
<td class="gt_row gt_center">0.13</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">-0.11</td>
<td class="gt_row gt_center">-0.26, 0.04</td>
<td class="gt_row gt_center">0.15</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.14, 0.24</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.02</td>
<td class="gt_row gt_center">-0.22, 0.19</td>
<td class="gt_row gt_center">0.9</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.26, 0.13</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">dNortheast</td>
<td class="gt_row gt_center">0.08</td>
<td class="gt_row gt_center">-0.11, 0.28</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">dMidwest</td>
<td class="gt_row gt_center">-0.04</td>
<td class="gt_row gt_center">-0.20, 0.13</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left">dWest</td>
<td class="gt_row gt_center">0.09</td>
<td class="gt_row gt_center">-0.06, 0.25</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">-0.25</td>
<td class="gt_row gt_center">-0.45, -0.04</td>
<td class="gt_row gt_center">0.019</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">-0.29</td>
<td class="gt_row gt_center">-0.46, -0.11</td>
<td class="gt_row gt_center">0.002</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">-0.29</td>
<td class="gt_row gt_center">-0.76, 0.19</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.03</td>
<td class="gt_row gt_center">-0.26, 0.33</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">-0.36</td>
<td class="gt_row gt_center">-0.55, -0.17</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.54, -0.15</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.56, -0.15</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.37</td>
<td class="gt_row gt_center">-0.66, -0.08</td>
<td class="gt_row gt_center">0.012</td></tr>
    <tr><td class="gt_row gt_left">dPersonal_use_ever</td>
<td class="gt_row gt_center">0.08</td>
<td class="gt_row gt_center">-0.15, 0.30</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">dFamily_use_ever</td>
<td class="gt_row gt_center">0.22</td>
<td class="gt_row gt_center">0.07, 0.37</td>
<td class="gt_row gt_center">0.004</td></tr>
    <tr><td class="gt_row gt_left">dPersonal_conviction_ever</td>
<td class="gt_row gt_center">-0.15</td>
<td class="gt_row gt_center">-0.36, 0.06</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">dFamily_conviction_ever</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">-0.12, 0.17</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">Stigma_Scale_Score</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.44, -0.26</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">CoBRAS_Scale</td>
<td class="gt_row gt_center">-0.33</td>
<td class="gt_row gt_center">-0.40, -0.25</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_Democrats 1.1

As we can see from the table, the self-identified democrats has a
statistically significant positive strong relationship(beta = 0.62,
P\<0.001), while the republicans having a statistically significant
positive moderate relationship(beta = -0.59, P\<0.001) with attitudes of
supporting expanded Medicaid benefits for addiction coverage. In other
words, self-identified democrats are more likely to support expanded
Medicaid benefits for addiction coverage while republicans not.

``` r
Medicaid_demo <- lm(num_Medicaid ~ ddem + drep, 
                    data = v_analysis)
tbl_regression(Medicaid_demo)
```

<div id="axrbcdjqbz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#axrbcdjqbz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#axrbcdjqbz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#axrbcdjqbz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#axrbcdjqbz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#axrbcdjqbz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#axrbcdjqbz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#axrbcdjqbz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#axrbcdjqbz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#axrbcdjqbz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#axrbcdjqbz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#axrbcdjqbz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#axrbcdjqbz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#axrbcdjqbz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#axrbcdjqbz .gt_from_md > :first-child {
  margin-top: 0;
}

#axrbcdjqbz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#axrbcdjqbz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#axrbcdjqbz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#axrbcdjqbz .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#axrbcdjqbz .gt_row_group_first td {
  border-top-width: 2px;
}

#axrbcdjqbz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#axrbcdjqbz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#axrbcdjqbz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#axrbcdjqbz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#axrbcdjqbz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#axrbcdjqbz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#axrbcdjqbz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#axrbcdjqbz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#axrbcdjqbz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#axrbcdjqbz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#axrbcdjqbz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#axrbcdjqbz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#axrbcdjqbz .gt_left {
  text-align: left;
}

#axrbcdjqbz .gt_center {
  text-align: center;
}

#axrbcdjqbz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#axrbcdjqbz .gt_font_normal {
  font-weight: normal;
}

#axrbcdjqbz .gt_font_bold {
  font-weight: bold;
}

#axrbcdjqbz .gt_font_italic {
  font-style: italic;
}

#axrbcdjqbz .gt_super {
  font-size: 65%;
}

#axrbcdjqbz .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#axrbcdjqbz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#axrbcdjqbz .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#axrbcdjqbz .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#axrbcdjqbz .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.62</td>
<td class="gt_row gt_center">0.47, 0.77</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.59</td>
<td class="gt_row gt_center">-0.76, -0.43</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_Democrats 1.2

As we can see from the table, when controlled for gender, age, race,
income and years of education, the self-identified democrats still have
a statistically significant positive strong relationship(beta = 0.66,
P\<0.001), while republicans still have a statistically significant
negative moderate relationship(beta = -0.56, p\<0.001) with attitudes of
supporting expanded Medicaid benefits for addiction coverage. In other
words, self-identified democrats are still more likely to support
expanded Medicaid benefits for addiction coverage, and the tendency of
supporting has even increased a little bit from0.62 to 0.66 while
republicans not.

``` r
Medicaid_demo2 <- lm(num_Medicaid ~ ddem + 
                      drep +
                      dmale +
                      dage30_44 +
                      dage45_59 +
                      dage60_plus +
                      race_BLACK +
                      race_HISPANIC +
                      race_ASIAN +
                      race_OTHERMIXED +
                      incomedummy_25to49K +
                      incomedummy_50to84K+
                      incomedummy_85to150K + 
                      incomedummy_OVER150K +
                      dless_HS +
                      dHS +
                      dsome_col,
                      data = v_analysis)
tbl_regression(Medicaid_demo2)
```

<div id="mtbidrfqle" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mtbidrfqle .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mtbidrfqle .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mtbidrfqle .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mtbidrfqle .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mtbidrfqle .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtbidrfqle .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mtbidrfqle .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mtbidrfqle .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mtbidrfqle .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mtbidrfqle .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mtbidrfqle .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mtbidrfqle .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#mtbidrfqle .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mtbidrfqle .gt_from_md > :first-child {
  margin-top: 0;
}

#mtbidrfqle .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mtbidrfqle .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mtbidrfqle .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mtbidrfqle .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mtbidrfqle .gt_row_group_first td {
  border-top-width: 2px;
}

#mtbidrfqle .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtbidrfqle .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mtbidrfqle .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mtbidrfqle .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtbidrfqle .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtbidrfqle .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mtbidrfqle .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mtbidrfqle .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtbidrfqle .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mtbidrfqle .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtbidrfqle .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mtbidrfqle .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtbidrfqle .gt_left {
  text-align: left;
}

#mtbidrfqle .gt_center {
  text-align: center;
}

#mtbidrfqle .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mtbidrfqle .gt_font_normal {
  font-weight: normal;
}

#mtbidrfqle .gt_font_bold {
  font-weight: bold;
}

#mtbidrfqle .gt_font_italic {
  font-style: italic;
}

#mtbidrfqle .gt_super {
  font-size: 65%;
}

#mtbidrfqle .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#mtbidrfqle .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mtbidrfqle .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#mtbidrfqle .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#mtbidrfqle .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.66</td>
<td class="gt_row gt_center">0.50, 0.81</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.56</td>
<td class="gt_row gt_center">-0.73, -0.40</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">-0.15</td>
<td class="gt_row gt_center">-0.28, -0.02</td>
<td class="gt_row gt_center">0.023</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">-0.15</td>
<td class="gt_row gt_center">-0.35, 0.05</td>
<td class="gt_row gt_center">0.14</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.57, -0.14</td>
<td class="gt_row gt_center">0.001</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.55, -0.14</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">-0.13</td>
<td class="gt_row gt_center">-0.35, 0.08</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">-0.27</td>
<td class="gt_row gt_center">-0.45, -0.08</td>
<td class="gt_row gt_center">0.005</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">-0.41</td>
<td class="gt_row gt_center">-0.89, 0.08</td>
<td class="gt_row gt_center">0.10</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.08</td>
<td class="gt_row gt_center">-0.23, 0.39</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">-0.31</td>
<td class="gt_row gt_center">-0.51, -0.11</td>
<td class="gt_row gt_center">0.003</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.30</td>
<td class="gt_row gt_center">-0.51, -0.09</td>
<td class="gt_row gt_center">0.005</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.27</td>
<td class="gt_row gt_center">-0.49, -0.05</td>
<td class="gt_row gt_center">0.015</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.37</td>
<td class="gt_row gt_center">-0.68, -0.06</td>
<td class="gt_row gt_center">0.020</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">-0.39</td>
<td class="gt_row gt_center">-0.72, -0.05</td>
<td class="gt_row gt_center">0.023</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">-0.29</td>
<td class="gt_row gt_center">-0.50, -0.08</td>
<td class="gt_row gt_center">0.007</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">-0.17</td>
<td class="gt_row gt_center">-0.32, -0.01</td>
<td class="gt_row gt_center">0.038</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
## omitted age category why omitted?
## How to set dummy variables?
```

## Question_Democrats 1.3

As we can see from the table, if we add opioid stigma and CoBRAS scores
to the regression specification, the self-identified democrats still has
a statistically significant positive moderate relationship(beta = 0.36,
P\<0.001) with attitudes of supporting expanded Medicaid benefits for
addiction coverage,however, the strength is only as half as before. For
the republicans, they have a significant negative weak relationship(beta
= -0.27, p = 0.01) with attitudes of supporting expanded Medicaid
benefits for addiction coverage.

``` r
Medicaid_demo3 <- lm(num_Medicaid ~ ddem + 
                      drep +
                      dmale +
                      dage30_44 +
                      dage45_59 +
                      dage60_plus +
                      race_BLACK +
                      race_HISPANIC +
                      race_ASIAN +
                      race_OTHERMIXED +
                      incomedummy_25to49K +
                      incomedummy_50to84K+
                      incomedummy_85to150K + 
                      incomedummy_OVER150K +
                      dless_HS +
                      dHS +
                      dsome_col +
                      Stigma_Scale_Score +
                      CoBRAS_Scale, 
                      data = v_analysis)
tbl_regression(Medicaid_demo3)
```

<div id="udvqwlbucg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#udvqwlbucg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#udvqwlbucg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#udvqwlbucg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#udvqwlbucg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#udvqwlbucg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#udvqwlbucg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#udvqwlbucg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#udvqwlbucg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#udvqwlbucg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#udvqwlbucg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#udvqwlbucg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#udvqwlbucg .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#udvqwlbucg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#udvqwlbucg .gt_from_md > :first-child {
  margin-top: 0;
}

#udvqwlbucg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#udvqwlbucg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#udvqwlbucg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#udvqwlbucg .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#udvqwlbucg .gt_row_group_first td {
  border-top-width: 2px;
}

#udvqwlbucg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#udvqwlbucg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#udvqwlbucg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#udvqwlbucg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#udvqwlbucg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#udvqwlbucg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#udvqwlbucg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#udvqwlbucg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#udvqwlbucg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#udvqwlbucg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#udvqwlbucg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#udvqwlbucg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#udvqwlbucg .gt_left {
  text-align: left;
}

#udvqwlbucg .gt_center {
  text-align: center;
}

#udvqwlbucg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#udvqwlbucg .gt_font_normal {
  font-weight: normal;
}

#udvqwlbucg .gt_font_bold {
  font-weight: bold;
}

#udvqwlbucg .gt_font_italic {
  font-style: italic;
}

#udvqwlbucg .gt_super {
  font-size: 65%;
}

#udvqwlbucg .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#udvqwlbucg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#udvqwlbucg .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#udvqwlbucg .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#udvqwlbucg .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.36</td>
<td class="gt_row gt_center">0.21, 0.51</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.27</td>
<td class="gt_row gt_center">-0.43, -0.10</td>
<td class="gt_row gt_center">0.001</td></tr>
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">-0.12</td>
<td class="gt_row gt_center">-0.24, 0.00</td>
<td class="gt_row gt_center">0.057</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">-0.17, 0.21</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.26, 0.14</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">-0.09</td>
<td class="gt_row gt_center">-0.28, 0.10</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">-0.29</td>
<td class="gt_row gt_center">-0.49, -0.09</td>
<td class="gt_row gt_center">0.004</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">-0.28</td>
<td class="gt_row gt_center">-0.45, -0.11</td>
<td class="gt_row gt_center">0.001</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">-0.27</td>
<td class="gt_row gt_center">-0.73, 0.18</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.06</td>
<td class="gt_row gt_center">-0.23, 0.35</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.54, -0.16</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.54, -0.15</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.32</td>
<td class="gt_row gt_center">-0.53, -0.12</td>
<td class="gt_row gt_center">0.002</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.36</td>
<td class="gt_row gt_center">-0.65, -0.07</td>
<td class="gt_row gt_center">0.014</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">-0.27</td>
<td class="gt_row gt_center">-0.58, 0.04</td>
<td class="gt_row gt_center">0.086</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">-0.17</td>
<td class="gt_row gt_center">-0.37, 0.03</td>
<td class="gt_row gt_center">0.094</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">-0.10</td>
<td class="gt_row gt_center">-0.24, 0.05</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">Stigma_Scale_Score</td>
<td class="gt_row gt_center">-0.34</td>
<td class="gt_row gt_center">-0.43, -0.25</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">CoBRAS_Scale</td>
<td class="gt_row gt_center">-0.36</td>
<td class="gt_row gt_center">-0.43, -0.29</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_Democrats 1.3.1

As we can see from the table, both the CoBRAS subscale(implicit
racism)(beta = -0.34, p \< 0.001) and stigmatizing attitudes and beliefs
regarding opioid use disorder(beta = -0.36, p\<0.001) have a moderate
negative association with attitudes of supporting expanded Medicaid
benefits for addiction coverage. In other words, people with more
implicit racism and stigmatiazing attitudes are less likely to support
expanded Medicaid benefits for addiction coverage.

## Question_naloxone 2.1

As we can see from the table, the self-identified democrats has a
statistically significant positive strong relationship(beta = 0.47,
P\<0.001), while republicans have a statistically significant negative
moderate relationship(beta = -0.32, p\<0.001) with attitudes of
supporting naloxone distribution to prevent opioid overdose. In other
words, self-identified democrats are more likely to support naloxone
distribution to prevent opioid overdose while republicans not.

``` r
nalo_demo1 <- lm(num_Naloxone ~ ddem +
                    drep, 
                    data = v_analysis)
tbl_regression(nalo_demo1 )
```

<div id="dkbwgoxyml" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dkbwgoxyml .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dkbwgoxyml .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dkbwgoxyml .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dkbwgoxyml .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dkbwgoxyml .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dkbwgoxyml .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dkbwgoxyml .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dkbwgoxyml .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dkbwgoxyml .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dkbwgoxyml .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dkbwgoxyml .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dkbwgoxyml .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#dkbwgoxyml .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dkbwgoxyml .gt_from_md > :first-child {
  margin-top: 0;
}

#dkbwgoxyml .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dkbwgoxyml .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dkbwgoxyml .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#dkbwgoxyml .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#dkbwgoxyml .gt_row_group_first td {
  border-top-width: 2px;
}

#dkbwgoxyml .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dkbwgoxyml .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dkbwgoxyml .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dkbwgoxyml .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dkbwgoxyml .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dkbwgoxyml .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dkbwgoxyml .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dkbwgoxyml .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dkbwgoxyml .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dkbwgoxyml .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dkbwgoxyml .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dkbwgoxyml .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dkbwgoxyml .gt_left {
  text-align: left;
}

#dkbwgoxyml .gt_center {
  text-align: center;
}

#dkbwgoxyml .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dkbwgoxyml .gt_font_normal {
  font-weight: normal;
}

#dkbwgoxyml .gt_font_bold {
  font-weight: bold;
}

#dkbwgoxyml .gt_font_italic {
  font-style: italic;
}

#dkbwgoxyml .gt_super {
  font-size: 65%;
}

#dkbwgoxyml .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#dkbwgoxyml .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dkbwgoxyml .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#dkbwgoxyml .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#dkbwgoxyml .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.47</td>
<td class="gt_row gt_center">0.31, 0.62</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.32</td>
<td class="gt_row gt_center">-0.49, -0.15</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_naloxone 2.2

As we can see from the table, when controlled for gender, age, race,
income and years of education, the self-identified democrats still have
a statistically significant positive strong relationship(beta = 0.52,
P\<0.001) while republicants still a statistically significant negative
moderate relationshio(beta = -0.35, p\<0.001) with attitudes of
supporting naloxone distribution to prevent opioid overdose. In other
words, self-identified democrats are still more likely to support
naloxone distribution to prevent opioid overdose than republicans, and
the tendency of supporting has even increased a little bit from 0.47 to
0.52.

``` r
nalo_demo2 <- lm(num_Naloxone ~ ddem + 
                      drep +
                      dmale +
                      dage30_44 +
                      dage45_59 +
                      dage60_plus +
                      race_BLACK +
                      race_HISPANIC +
                      race_ASIAN +
                      race_OTHERMIXED +
                      incomedummy_25to49K +
                      incomedummy_50to84K+
                      incomedummy_85to150K + 
                      incomedummy_OVER150K +
                      dless_HS +
                      dHS +
                      dsome_col,
                      data = v_analysis)
tbl_regression(nalo_demo2)
```

<div id="mzqyhggxxg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mzqyhggxxg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mzqyhggxxg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mzqyhggxxg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mzqyhggxxg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mzqyhggxxg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mzqyhggxxg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mzqyhggxxg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mzqyhggxxg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mzqyhggxxg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mzqyhggxxg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mzqyhggxxg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mzqyhggxxg .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#mzqyhggxxg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mzqyhggxxg .gt_from_md > :first-child {
  margin-top: 0;
}

#mzqyhggxxg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mzqyhggxxg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mzqyhggxxg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mzqyhggxxg .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mzqyhggxxg .gt_row_group_first td {
  border-top-width: 2px;
}

#mzqyhggxxg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mzqyhggxxg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mzqyhggxxg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mzqyhggxxg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mzqyhggxxg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mzqyhggxxg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mzqyhggxxg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mzqyhggxxg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mzqyhggxxg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mzqyhggxxg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mzqyhggxxg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mzqyhggxxg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mzqyhggxxg .gt_left {
  text-align: left;
}

#mzqyhggxxg .gt_center {
  text-align: center;
}

#mzqyhggxxg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mzqyhggxxg .gt_font_normal {
  font-weight: normal;
}

#mzqyhggxxg .gt_font_bold {
  font-weight: bold;
}

#mzqyhggxxg .gt_font_italic {
  font-style: italic;
}

#mzqyhggxxg .gt_super {
  font-size: 65%;
}

#mzqyhggxxg .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#mzqyhggxxg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mzqyhggxxg .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#mzqyhggxxg .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#mzqyhggxxg .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.52</td>
<td class="gt_row gt_center">0.36, 0.68</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.52, -0.18</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">0.09</td>
<td class="gt_row gt_center">-0.05, 0.22</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">-0.19</td>
<td class="gt_row gt_center">-0.39, 0.01</td>
<td class="gt_row gt_center">0.066</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.29</td>
<td class="gt_row gt_center">-0.50, -0.07</td>
<td class="gt_row gt_center">0.009</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">-0.14</td>
<td class="gt_row gt_center">-0.34, 0.07</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">-0.44</td>
<td class="gt_row gt_center">-0.65, -0.22</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">-0.13</td>
<td class="gt_row gt_center">-0.31, 0.06</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">-0.40</td>
<td class="gt_row gt_center">-0.90, 0.09</td>
<td class="gt_row gt_center">0.11</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.18</td>
<td class="gt_row gt_center">-0.13, 0.49</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">-0.18</td>
<td class="gt_row gt_center">-0.38, 0.03</td>
<td class="gt_row gt_center">0.090</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.14</td>
<td class="gt_row gt_center">-0.36, 0.07</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.07</td>
<td class="gt_row gt_center">-0.30, 0.15</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.37, 0.25</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">-0.26</td>
<td class="gt_row gt_center">-0.60, 0.08</td>
<td class="gt_row gt_center">0.13</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">-0.43</td>
<td class="gt_row gt_center">-0.64, -0.21</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">-0.15</td>
<td class="gt_row gt_center">-0.31, 0.01</td>
<td class="gt_row gt_center">0.067</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_naloxone 2.3

As we can see from the table, if we add opioid stigma and CoBRAS scores
to the regression specification, the self-identified democrats still has
a statistically significant positive moderate relationship(beta = 0.31,
P\<0.001) with attitudes of supporting naloxone distribution to prevent
opioid overdose, however, the strength is only as half as before. While
republicans have no statistically significant relationship with
attitudes of supporting naloxone distribution to prevent opioid
overdose.

``` r
nalo_demo3 <- lm(num_Naloxone ~ ddem + 
                      drep +
                      dmale +
                      dage30_44 +
                      dage45_59 +
                      dage60_plus +
                      race_BLACK +
                      race_HISPANIC +
                      race_ASIAN +
                      race_OTHERMIXED +
                      incomedummy_25to49K +
                      incomedummy_50to84K+
                      incomedummy_85to150K + 
                      incomedummy_OVER150K +
                      dless_HS +
                      dHS +
                      dsome_col +
                      Stigma_Scale_Score +
                      CoBRAS_Scale, 
                      data = v_analysis)
tbl_regression(nalo_demo3)
```

<div id="ouvqmttdku" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ouvqmttdku .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ouvqmttdku .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ouvqmttdku .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ouvqmttdku .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ouvqmttdku .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ouvqmttdku .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ouvqmttdku .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ouvqmttdku .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ouvqmttdku .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ouvqmttdku .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ouvqmttdku .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ouvqmttdku .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ouvqmttdku .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ouvqmttdku .gt_from_md > :first-child {
  margin-top: 0;
}

#ouvqmttdku .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ouvqmttdku .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ouvqmttdku .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ouvqmttdku .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ouvqmttdku .gt_row_group_first td {
  border-top-width: 2px;
}

#ouvqmttdku .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ouvqmttdku .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ouvqmttdku .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ouvqmttdku .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ouvqmttdku .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ouvqmttdku .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ouvqmttdku .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ouvqmttdku .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ouvqmttdku .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ouvqmttdku .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ouvqmttdku .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ouvqmttdku .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ouvqmttdku .gt_left {
  text-align: left;
}

#ouvqmttdku .gt_center {
  text-align: center;
}

#ouvqmttdku .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ouvqmttdku .gt_font_normal {
  font-weight: normal;
}

#ouvqmttdku .gt_font_bold {
  font-weight: bold;
}

#ouvqmttdku .gt_font_italic {
  font-style: italic;
}

#ouvqmttdku .gt_super {
  font-size: 65%;
}

#ouvqmttdku .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ouvqmttdku .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ouvqmttdku .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#ouvqmttdku .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#ouvqmttdku .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.31</td>
<td class="gt_row gt_center">0.15, 0.47</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.14</td>
<td class="gt_row gt_center">-0.31, 0.03</td>
<td class="gt_row gt_center">0.11</td></tr>
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">0.11</td>
<td class="gt_row gt_center">-0.02, 0.24</td>
<td class="gt_row gt_center">0.10</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">-0.07</td>
<td class="gt_row gt_center">-0.27, 0.13</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.08</td>
<td class="gt_row gt_center">-0.29, 0.13</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.16, 0.25</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">-0.54</td>
<td class="gt_row gt_center">-0.74, -0.33</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">-0.13</td>
<td class="gt_row gt_center">-0.32, 0.05</td>
<td class="gt_row gt_center">0.15</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">-0.31</td>
<td class="gt_row gt_center">-0.79, 0.17</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.16</td>
<td class="gt_row gt_center">-0.14, 0.47</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">-0.20</td>
<td class="gt_row gt_center">-0.40, 0.00</td>
<td class="gt_row gt_center">0.046</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.17</td>
<td class="gt_row gt_center">-0.38, 0.03</td>
<td class="gt_row gt_center">0.10</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.11</td>
<td class="gt_row gt_center">-0.32, 0.10</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.04</td>
<td class="gt_row gt_center">-0.35, 0.26</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">-0.19</td>
<td class="gt_row gt_center">-0.51, 0.14</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">-0.34</td>
<td class="gt_row gt_center">-0.55, -0.13</td>
<td class="gt_row gt_center">0.001</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">-0.10</td>
<td class="gt_row gt_center">-0.26, 0.05</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">Stigma_Scale_Score</td>
<td class="gt_row gt_center">-0.24</td>
<td class="gt_row gt_center">-0.33, -0.14</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">CoBRAS_Scale</td>
<td class="gt_row gt_center">-0.25</td>
<td class="gt_row gt_center">-0.32, -0.17</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_naloxone 2.3.1

As we can see from the table, both the CoBRAS subscale(implicit
racism)(beta = -0.25, p \> 0.001) and stigmatizing attitudes and beliefs
regarding opioid use disorder(beta = -0.24, p \> 0.001) have a moderate
negative association with supporting naloxone distribution to prevent
opioid overdose,. In other words, people with more implicit racism and
stigmatiazing attitudes are less likely to support naloxone distribution
to prevent opiod overdose.

## Question_mandatory 3.1

As we can see from the table, the self-identified democrats has a
statistically significant weak positive relationship(beta = 0.29,
P\<0.001) with attitudes believing mandatory treatment effective. In
other words, self-identified democrats are more likely to believe
mandatory treatment to be effective. While republicans have no
statistical signficant relationship with attitudes believing mandatory
treatment to be effective.

``` r
man_demo1 <- lm(num_Mandatory ~ ddem +
                    drep, 
                    data = v_analysis)

tbl_regression(man_demo1)
```

<div id="golzlqnnsf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#golzlqnnsf .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#golzlqnnsf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#golzlqnnsf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#golzlqnnsf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#golzlqnnsf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#golzlqnnsf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#golzlqnnsf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#golzlqnnsf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#golzlqnnsf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#golzlqnnsf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#golzlqnnsf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#golzlqnnsf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#golzlqnnsf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#golzlqnnsf .gt_from_md > :first-child {
  margin-top: 0;
}

#golzlqnnsf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#golzlqnnsf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#golzlqnnsf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#golzlqnnsf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#golzlqnnsf .gt_row_group_first td {
  border-top-width: 2px;
}

#golzlqnnsf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#golzlqnnsf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#golzlqnnsf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#golzlqnnsf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#golzlqnnsf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#golzlqnnsf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#golzlqnnsf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#golzlqnnsf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#golzlqnnsf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#golzlqnnsf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#golzlqnnsf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#golzlqnnsf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#golzlqnnsf .gt_left {
  text-align: left;
}

#golzlqnnsf .gt_center {
  text-align: center;
}

#golzlqnnsf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#golzlqnnsf .gt_font_normal {
  font-weight: normal;
}

#golzlqnnsf .gt_font_bold {
  font-weight: bold;
}

#golzlqnnsf .gt_font_italic {
  font-style: italic;
}

#golzlqnnsf .gt_super {
  font-size: 65%;
}

#golzlqnnsf .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#golzlqnnsf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#golzlqnnsf .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#golzlqnnsf .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#golzlqnnsf .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.29</td>
<td class="gt_row gt_center">0.14, 0.44</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.17</td>
<td class="gt_row gt_center">-0.34, 0.00</td>
<td class="gt_row gt_center">0.045</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_Mandatory 3.2

As we can see from the table, when controlled for gender, age, race,
income and years of education, the self-identified democrats still has a
statistically significant weak positive relationship(beta = 0.29,
P\<0.001) with attitudes of believing mandatory treatment be effective.
While republicans have no statistically significant relationship with
attitudes of believing mandatory treatment to be effective. In other
words, self-identified democrats are still likely to believe mandatory
treatment to be more effective.

``` r
man_demo2 <- lm(num_Mandatory ~ ddem + 
                      drep +
                      dmale +
                      dage30_44 +
                      dage45_59 +
                      dage60_plus +
                      race_BLACK +
                      race_HISPANIC +
                      race_ASIAN +
                      race_OTHERMIXED +
                      incomedummy_25to49K +
                      incomedummy_50to84K+
                      incomedummy_85to150K + 
                      incomedummy_OVER150K +
                      dless_HS +
                      dHS +
                      dsome_col,
                      data = v_analysis)
tbl_regression(man_demo2)
```

<div id="uhlfadonoi" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uhlfadonoi .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uhlfadonoi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uhlfadonoi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uhlfadonoi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uhlfadonoi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uhlfadonoi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uhlfadonoi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uhlfadonoi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uhlfadonoi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uhlfadonoi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uhlfadonoi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uhlfadonoi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uhlfadonoi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uhlfadonoi .gt_from_md > :first-child {
  margin-top: 0;
}

#uhlfadonoi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uhlfadonoi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uhlfadonoi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#uhlfadonoi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#uhlfadonoi .gt_row_group_first td {
  border-top-width: 2px;
}

#uhlfadonoi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uhlfadonoi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uhlfadonoi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uhlfadonoi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uhlfadonoi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uhlfadonoi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uhlfadonoi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uhlfadonoi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uhlfadonoi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uhlfadonoi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uhlfadonoi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uhlfadonoi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uhlfadonoi .gt_left {
  text-align: left;
}

#uhlfadonoi .gt_center {
  text-align: center;
}

#uhlfadonoi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uhlfadonoi .gt_font_normal {
  font-weight: normal;
}

#uhlfadonoi .gt_font_bold {
  font-weight: bold;
}

#uhlfadonoi .gt_font_italic {
  font-style: italic;
}

#uhlfadonoi .gt_super {
  font-size: 65%;
}

#uhlfadonoi .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#uhlfadonoi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uhlfadonoi .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#uhlfadonoi .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#uhlfadonoi .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.29</td>
<td class="gt_row gt_center">0.13, 0.44</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.14</td>
<td class="gt_row gt_center">-0.31, 0.03</td>
<td class="gt_row gt_center">0.10</td></tr>
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">-0.05</td>
<td class="gt_row gt_center">-0.18, 0.08</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">-0.10</td>
<td class="gt_row gt_center">-0.30, 0.10</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.11</td>
<td class="gt_row gt_center">-0.32, 0.10</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">-0.08</td>
<td class="gt_row gt_center">-0.28, 0.13</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.16, 0.26</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">0.16</td>
<td class="gt_row gt_center">-0.03, 0.34</td>
<td class="gt_row gt_center">0.10</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">0.37</td>
<td class="gt_row gt_center">-0.13, 0.87</td>
<td class="gt_row gt_center">0.15</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.26, 0.36</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.16, 0.25</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.12</td>
<td class="gt_row gt_center">-0.33, 0.10</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.28, 0.16</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.16</td>
<td class="gt_row gt_center">-0.48, 0.15</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">0.17</td>
<td class="gt_row gt_center">-0.17, 0.51</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">-0.19, 0.24</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">0.03</td>
<td class="gt_row gt_center">-0.13, 0.19</td>
<td class="gt_row gt_center">0.7</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_Mandatory 3.3

As we can see from the table, if we add opioid stigma and CoBRAS scores
to the regression specification, the self-identified democrats still has
a statistically significant weak positive relationship(beta = 0.22,
P\<0.01) with attitudes of believing mandatory treatment be effective,
however, the strength of the relationship has been reduced from 0.29 to
0.22. While for republicans, they have no statistically significant
relationship with attitudes of believing mandatory treatment be
effective.

``` r
man_demo3 <- lm(num_Mandatory ~ ddem + 
                      drep +
                      dmale +
                      dage30_44 +
                      dage45_59 +
                      dage60_plus +
                      race_BLACK +
                      race_HISPANIC +
                      race_ASIAN +
                      race_OTHERMIXED +
                      incomedummy_25to49K +
                      incomedummy_50to84K+
                      incomedummy_85to150K + 
                      incomedummy_OVER150K +
                      dless_HS +
                      dHS +
                      dsome_col +
                      Stigma_Scale_Score +
                      CoBRAS_Scale, 
                      data = v_analysis)
tbl_regression(man_demo3)
```

<div id="opijrlgtyv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#opijrlgtyv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#opijrlgtyv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opijrlgtyv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#opijrlgtyv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#opijrlgtyv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opijrlgtyv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opijrlgtyv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#opijrlgtyv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#opijrlgtyv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#opijrlgtyv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#opijrlgtyv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#opijrlgtyv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#opijrlgtyv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#opijrlgtyv .gt_from_md > :first-child {
  margin-top: 0;
}

#opijrlgtyv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#opijrlgtyv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#opijrlgtyv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#opijrlgtyv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#opijrlgtyv .gt_row_group_first td {
  border-top-width: 2px;
}

#opijrlgtyv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opijrlgtyv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#opijrlgtyv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#opijrlgtyv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opijrlgtyv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opijrlgtyv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#opijrlgtyv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#opijrlgtyv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opijrlgtyv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#opijrlgtyv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opijrlgtyv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#opijrlgtyv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opijrlgtyv .gt_left {
  text-align: left;
}

#opijrlgtyv .gt_center {
  text-align: center;
}

#opijrlgtyv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#opijrlgtyv .gt_font_normal {
  font-weight: normal;
}

#opijrlgtyv .gt_font_bold {
  font-weight: bold;
}

#opijrlgtyv .gt_font_italic {
  font-style: italic;
}

#opijrlgtyv .gt_super {
  font-size: 65%;
}

#opijrlgtyv .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#opijrlgtyv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#opijrlgtyv .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#opijrlgtyv .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#opijrlgtyv .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ddem</td>
<td class="gt_row gt_center">0.22</td>
<td class="gt_row gt_center">0.06, 0.39</td>
<td class="gt_row gt_center">0.007</td></tr>
    <tr><td class="gt_row gt_left">drep</td>
<td class="gt_row gt_center">-0.07</td>
<td class="gt_row gt_center">-0.25, 0.11</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">dmale</td>
<td class="gt_row gt_center">-0.05</td>
<td class="gt_row gt_center">-0.18, 0.08</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">dage30_44</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.26, 0.14</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">dage45_59</td>
<td class="gt_row gt_center">-0.05</td>
<td class="gt_row gt_center">-0.27, 0.17</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">dage60_plus</td>
<td class="gt_row gt_center">-0.03</td>
<td class="gt_row gt_center">-0.24, 0.18</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">race_BLACK</td>
<td class="gt_row gt_center">0.01</td>
<td class="gt_row gt_center">-0.20, 0.23</td>
<td class="gt_row gt_center">0.9</td></tr>
    <tr><td class="gt_row gt_left">race_HISPANIC</td>
<td class="gt_row gt_center">0.15</td>
<td class="gt_row gt_center">-0.04, 0.34</td>
<td class="gt_row gt_center">0.11</td></tr>
    <tr><td class="gt_row gt_left">race_ASIAN</td>
<td class="gt_row gt_center">0.39</td>
<td class="gt_row gt_center">-0.11, 0.89</td>
<td class="gt_row gt_center">0.13</td></tr>
    <tr><td class="gt_row gt_left">race_OTHERMIXED</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.26, 0.36</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_25to49K</td>
<td class="gt_row gt_center">0.04</td>
<td class="gt_row gt_center">-0.16, 0.25</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_50to84K</td>
<td class="gt_row gt_center">-0.12</td>
<td class="gt_row gt_center">-0.34, 0.09</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_85to150K</td>
<td class="gt_row gt_center">-0.07</td>
<td class="gt_row gt_center">-0.29, 0.15</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">incomedummy_OVER150K</td>
<td class="gt_row gt_center">-0.14</td>
<td class="gt_row gt_center">-0.45, 0.17</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">dless_HS</td>
<td class="gt_row gt_center">0.20</td>
<td class="gt_row gt_center">-0.14, 0.53</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">dHS</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">-0.16, 0.26</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">dsome_col</td>
<td class="gt_row gt_center">0.04</td>
<td class="gt_row gt_center">-0.12, 0.20</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">Stigma_Scale_Score</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.15, 0.04</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left">CoBRAS_Scale</td>
<td class="gt_row gt_center">-0.08</td>
<td class="gt_row gt_center">-0.16, -0.01</td>
<td class="gt_row gt_center">0.035</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Question_Mandatory 3.3.1

As we can see from the table, both the CoBRAS subscale(implicit racism)
and stigmatizing attitudes and beliefs regarding opioid use disorder
have no association with attitudes of believing mandatory treatment be
effective. In other words, people with more implicit racism and
stigmatiazing attitudes and those who are not make no difference
regarding their attitudes of believing madatory treatment be effective
or not.
