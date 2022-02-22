Avocado Retail Prices Project
================
Tyler Reed

## Data

In this project, you will explore avocado prices. The data come from
kaggle and represent weekly retail scan data. A description of the data
can be found on the [kaggle
page](https://www.kaggle.com/neuromusic/avocado-prices).

### Task 3: Read in the data and prepare for analysis

Write the code that reads in the avocado dataset. Store these data into
an object called `avocado`. Do not forget to load the packages that you
will need.

``` r
library(tidyverse)
library(viridis)
library(knitr)
library(hrbrthemes)
library(ggthemes)
library(patchwork)
library(glue)
avocado <- read_csv("data/avocado.csv")
```

Rename the three columns that are currently price lookup (PLU) codes to
be more meaningful and define a vector of major regions.

  - `4046`: number of small sized avocados sold
  - `4225`: number of large sized avocados sold
  - `4770`: number of extra large sized avocados sold

<!-- end list -->

``` r
avocado <- avocado %>%
   rename("Small" = `4046`,
          "Large" = `4225`,
          "Extra_Large" = `4770`)

major_regions <- c("California", "GreatLakes", "Midsouth", "Northeast", "Plains",
                   "SouthCentral", "Southeast", "West")
```

#### Tyler Reed:

**Rationale**

Renaming the columns with PLU codes makes it easier to analyze the sales
data by size of avocado.

Create a new object called `avocado_major_regions` that contains a
subset of the `avocado` dataset for the eight major regions for Hass
avocados: California, Great Lakes, Midsouth, Northeast, Plains, South
Central, Southeast, and West.

``` r
avocado_major_regions <- avocado %>%
   filter(region == major_regions)
```

#### Tyler Reed:

**Rationale**

Creating a subset or filtered dataset only with the eight major regions
should help in analyzing and addressing regional information.

Create a new object called `avocado_greatlakes_regions` that contains a
subset of the `avocado` dataset for the six local markets for Hass
avocados: Chicago, Cincinnati Dayton, Columbus, Detroit, Grand Rapids,
and Indianapolis.

``` r
greatlakes_regions <- c("Chicago", "CincinnatiDayton", "Columbus", "Detroit",
                        "GrandRapids", "Indianapolis")

avocado_greatlakes_regions <- avocado %>%
   filter(region == greatlakes_regions)
```

#### Tyler Reed:

**Rationale**

Creating a new list of `greatlakes_regions` to more easily filter the
avocado dataset by these previously undefined regions. Now, analysis of
these sub-regions can be more focused.

## Analysis

### Task 4: Top region

Which major region sold the most organic small Hass avocados in 2017?
Display the `year`, `region`, and *total* small avocados sold for all
major regions, appropriately sorted so that the answer to this question
is the first item.

``` r
avocado_major_regions %>%
   filter(year == "2017", type == "organic") %>%
   group_by(region) %>%
   summarize(year, region, "total" = round(sum(`Small`))) %>%
   distinct() %>%
   arrange(desc(`total`)) %>%
   kable(caption = "Highest Small, Organic Avocado Sales in 2017 by Region")
```

| region       | year | total |
| :----------- | ---: | ----: |
| California   | 2017 | 99916 |
| West         | 2017 | 75855 |
| SouthCentral | 2017 | 32930 |
| Plains       | 2017 | 14200 |
| Northeast    | 2017 |  6369 |
| Midsouth     | 2017 |  6079 |
| GreatLakes   | 2017 |  3482 |
| Southeast    | 2017 |  2611 |

Highest Small, Organic Avocado Sales in 2017 by Region

#### Tyler Reed:

**Rationale**

Provided the table above to facilitate a quick analysis of small,
organic avocado sales by region in order to catch any trends or
outliers. Sorted highest sales to lowest sales to clearly see if any
trends exist.

**Conclusion**

The following conclusions could be drawn from the table data:

  - More northward regions tend to have much fewer sales; however, a
    breakdown into subregions would be needed to confirm this because
    the *West* region includes northwest subregions.
  - Southward regions tend to have higher sales, except for the
    *Midsouth* and *Southeast*. This may be due to several factors, but
    significant factors, such as population density, and median
    household income may play the biggest roles. This exception with the
    *Midsouth* and *Southeast* may be invalidated once a control for
    population density is put into place and measurements like, sales
    per capita can be compared. Nonetheless, further analysis is needed
    for a more accurate conclusion.

**Complete the remaining tasks using the Great Lakes regions only.**

### Task 5: Visualize Large Avocado Sales

Make a tibble with one row per year and columns of median large avocados
sold for all Great Lakes region markets (e.g., *Large Sized Grand
Rapids* would be one of your columns). You must use the `values_fn =
...` argument when restructuring your data. Store this into an R object
called `greatlakes_large_year`

``` r
greatlakes_large_year <- avocado_greatlakes_regions %>%
   select(Large, year, region) %>%
   mutate(region = str_glue("Large Sized {region}"),
          region = map_chr(`region`, 
                           str_replace, 
                           "GrandRapids", 
                           "Grand Rapids"),
          region = map_chr(`region`, 
                           str_replace, 
                           "CincinnatiDayton", 
                           "Cincinnati Dayton"),
          year = as_factor(`year`)) %>%
   pivot_wider(id_cols = c(region, year), 
               names_from = region, 
               values_from = Large,
               values_fn = median) %>%
   rename("Year" = year) %>%
   arrange(desc(Year))

greatlakes_large_year %>%
   kable(caption = "Median Sales of Large Avocados per Year by Region", 
         digits = 2,
         align = "c")
```

| Year | Large Sized Grand Rapids | Large Sized Chicago | Large Sized Detroit | Large Sized Cincinnati Dayton | Large Sized Indianapolis | Large Sized Columbus |
| :--: | :----------------------: | :-----------------: | :-----------------: | :---------------------------: | :----------------------: | :------------------: |
| 2018 |            NA            |      488374.40      |      73291.77       |            4308.74            |         1460.83          |       8582.89        |
| 2017 |         46898.66         |      215597.51      |      25201.61       |            6528.34            |         1438.86          |       1463.98        |
| 2016 |         66146.50         |      373205.75      |      51486.24       |            7571.38            |         52842.30         |       15517.83       |
| 2015 |          724.07          |      24654.17       |      31391.78       |           53681.86            |         33482.65         |       33535.09       |

Median Sales of Large Avocados per Year by Region

#### Tyler Reed:

**Rationale**

To explore the relationship between the sales of *Large Sized* avocados
*region*, I have provide the table above. The table is sorted by year
for ease in viewing the most recent or relevant sales.

Median was chosen as a measurement of the middle or “center” of the
distribution of all sales of *Large Sized* avocados for each region
during the given year. For each year and region, if you were to create a
list for each of the sales of *Large Sized* avocados from lowest sales
to highest sales and then split the list exactly in half, the sales
value at that halfway point would be the median sales mentioned above.

**Conclusion**

The following conclusions could be drawn from the table data:

  - Median sales vary widely between regions; however, in 2017, the
    median was the lowest for each region except for *Cincinnati
    Dayton*, though still low for that region.
  - For *Grand Rapids*, *Chicago*, and *Detroit* 2015 was an
    exceptionally low year, but for the other three regions it was a
    very high year in sales. This may suggest a trend within supply
    lines if the two different areas are supplied by different vendors.
    Weather and price related factors may also be playing a role, though
    further investigation would be needed.

Using this restructured data, create a scatterplot for median large
avocados sold for one Great Lakes region market (`x`) against that of
another Great Lakes region market (`y`).

``` r
greatlakes_large_year %>%
   ggplot() +
      geom_point(aes(x = `Large Sized Columbus`, 
                     y = `Large Sized Detroit`,
                     color = reorder(`Year`, 2015:2018)), 
                     shape = 20, 
                     stroke = TRUE,
                     size = 10) +
      theme_bw(base_size = 14) +
      scale_color_manual(name = "Year",
                       values = c("#002B0C", "#005C09", "#48E057","#B6FAAD")) +
      labs(title = "Avocado Median Sales Volume",
           subtitle = "by year and subregion") +
      theme(plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 10, color = "#4e4d47"))
```

![](project04_files/figure-gfm/scatterplot-1.png)<!-- -->

#### Tyler Reed:

**Rationale**

To explore the relationship between the median sales of each *year*,
*Large* avocado size and the *Detroit* and *Columbus* *regions*, I chose
to plot each year’s median sales figure for each *region*. Here we can
assess potential for correlations between two *regions* and each *year*
of median sales of *large* avocados.

**Conclusion**

  - There is not enough information to formerly conclude any
    correlations here; however, just looking at the graph you can see
    that if the trend continues with more data, and inverse relationship
    between *Columbus* and *Detroit* appears. Besides the outlier of
    *2017*, median sales of *Large* avocados decreases with time for
    *Columbus* and increases with time in *Detroit.*

### Task 6: Summary table

Write a function called `summarise_vars()` that takes a tibble and
vector of variable names as input and returns something like:

    # A tibble: p x 3
       variable         mean   med
       <chr>           <dbl> <dbl>
     1 variable1           #     # 
     2 variable2           #     #
     ...
     p variablep           #     #

For example, `summarise_vars(greatlakes_large_year, c(chicago_large,
detroit_large, grandrapids_large))` should return the following
information (assuming that your columns are similarly named):

    # A tibble: 3 x 3
      variable             mean     med
      <chr>               <dbl>   <dbl>
    1 chicago_large         ...     ...
    2 detroit_large         ...     ...
    3 grandrapids_large     ...     ...

**Hint**: look at the [Programming with dplyr
vignette](https://dplyr.tidyverse.org/articles/programming.html) and the
[`across()`
documentation](https://dplyr.tidyverse.org/reference/across.html).

``` r
vec <- c("Large Sized Columbus", 
         "Large Sized Chicago", 
         "Large Sized Detroit",
         "Large Sized Indianapolis")

summarise_vars <- function(tib, vec) {
   tib %>%
      select(vec) %>%
      summarise(across(vec, list(mean = mean, median = median))) %>%
      pivot_longer(cols = everything(),
                   names_to = c("variables", "summary"),
                   names_pattern = "(.*)_(.*)",
                   values_to = "values") %>%
      pivot_wider(id_cols = c(variables, summary), 
                  names_from = summary, 
                  values_from = values) %>%
      kable(caption = "Output of `summarise_vars()` function")
}
summarise_vars(greatlakes_large_year, vec)
```

| variables                |      mean |    median |
| :----------------------- | --------: | --------: |
| Large Sized Columbus     |  14774.95 |  12050.36 |
| Large Sized Chicago      | 275457.96 | 294401.63 |
| Large Sized Detroit      |  45342.85 |  41439.01 |
| Large Sized Indianapolis |  22306.16 |  17471.74 |

Output of `summarise_vars()` function

#### Tyler Reed:

**Rationale**

To explore more quickly explore brief summaries of a simple list of
regions or subregions within the avocado dataset or any other dataset, I
provided the function which outputs the table above. In this way, a user
will be able to supply a list of items or variables to look into and
quickly receive the mean and median of the sales of said list.

**Conclusion**

Quick summaries of sales data by given region/item including mean and
median.

### Task 7: Recreate this visualization

The following plot shows, for all six Great Lakes regions, the percent
of Hass avocado sales (as average number sold) that are small, large, or
extra large; conventional vs. organic. Recreate the plot. You do not
need to get the colors exactly the same, but you should address them in
your plot.

![](img/recreate-proj04.png)

``` r
# Prepare dataset for plotting
avocado_greatlakes_regions %>%
   mutate(region = map_chr(`region`, 
                           str_replace, 
                           "GrandRapids", 
                           "Grand Rapids"),
          region = map_chr(`region`, 
                           str_replace, 
                           "CincinnatiDayton", 
                           "Cincinnati Dayton"),
          type = map_chr(`type`, 
                         str_replace, 
                         "conventional", 
                         "Conventional"),
          type = map_chr(`type`, 
                         str_replace, 
                         "organic", 
                         "Organic")) %>%
   select(region, 
          Small, 
          Large, 
          Extra_Large, 
          type, 
          `Total Volume`) %>%
   group_by(region, type) %>%
   pivot_wider(names_from = type, values_from = c(Small, Large, Extra_Large)) %>%
   pivot_longer(cols = ends_with(c("conventional", "organic")),
                names_to = c("size", "type"),
                names_pattern = "(.*)_(.*)",
                values_to = "values") %>%
   group_by(size, type, region) %>% 
   mutate(values = mean(`values`, na.rm = TRUE)) %>%
   select(-`Total Volume`) %>%
   summarize(region, size, values, type) %>%
   distinct() %>%
   # Create plot
   ggplot(aes(fill = size, y = values, x = region)) + 
     geom_bar(position="fill", stat="identity") +
     facet_wrap(~type) +
     theme_bw(base_size = 18) +
     scale_fill_manual(name = "Size", 
                       labels = c("Extra Large", "Large", "Small"),
                       values = c("#002B0C", "#005C09", "#48E057")) +
     labs(y = "Percent of average sales",
          x = "Region") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
           legend.text = element_text(size = 12))
```

![](project04_files/figure-gfm/recreate%20plot-1.png)<!-- -->

#### Tyler Reed:

**Rationale**

To explore the relationship between the *type* of avocado, its *size*
and the *region*, I chose to create the following plot. It shows for all
six Great Lakes regions, the percent of Hass avocado sales (as average
number sold) that are *small*, *large*, or *extra large*; *conventional*
vs. *organic*.

To better see the impact of each *size* and *type* of avocado sales in
each *region*, they are organized by *type* first and colored by *size*
with their percentage of total average sales along the y-axis. Any
trends by *region* and *size* of avocado should be more clearly visible
with the plot.

**Conclusion**

The following conclusions could be drawn from the plotted data:

  - By far, *extra large* avocados are the least in proportion of sales
    for all regions. This may suggest an answer to why no organic,
    *extra large* avocados are sold in any of the *regions*; possibly
    low demand, or low supply.
  - All *regions* favor *large* avocados in both *types*, except
    *Columbus* and *Detroit.* This may be due to several factors; brand
    of grocery market, simple demand of *small* avocados being much
    greater, supplier interests, etc. Further investigation required.
  - *Grand Rapids* is quite picky when it comes to their *small*
    avocados. It seems if they’re going to buy *small* avocados, it’s
    going to *organic* most of the time.
  - When it comes to *conventional* avocados, *Detroit* favors the two
    extreme *sizes*.

## Attribution

This Application Task is based on a lab from [Dr. Kelly
Bodwin](https://www.kelly-bodwin.com/)’s STAT 331 course.
