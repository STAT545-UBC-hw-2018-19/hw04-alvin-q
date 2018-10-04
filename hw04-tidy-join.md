HW04
================

Homework 04: Tidy data and joins
================================

Load packages

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
```

Data Reshaping Prompts (and relationship to aggregation)
--------------------------------------------------------

### Activity 2

Make a tibble with one row per year and columns for life expectancy for two or more countries.

``` r
gap_subset <- gapminder %>%
  filter(country %in% c("Iceland", "Japan", "Kenya")) %>%
  select(country, year, lifeExp)

gap_subset %>%
  knitr::kable()
```

| country |  year|  lifeExp|
|:--------|-----:|--------:|
| Iceland |  1952|   72.490|
| Iceland |  1957|   73.470|
| Iceland |  1962|   73.680|
| Iceland |  1967|   73.730|
| Iceland |  1972|   74.460|
| Iceland |  1977|   76.110|
| Iceland |  1982|   76.990|
| Iceland |  1987|   77.230|
| Iceland |  1992|   78.770|
| Iceland |  1997|   78.950|
| Iceland |  2002|   80.500|
| Iceland |  2007|   81.757|
| Japan   |  1952|   63.030|
| Japan   |  1957|   65.500|
| Japan   |  1962|   68.730|
| Japan   |  1967|   71.430|
| Japan   |  1972|   73.420|
| Japan   |  1977|   75.380|
| Japan   |  1982|   77.110|
| Japan   |  1987|   78.670|
| Japan   |  1992|   79.360|
| Japan   |  1997|   80.690|
| Japan   |  2002|   82.000|
| Japan   |  2007|   82.603|
| Kenya   |  1952|   42.270|
| Kenya   |  1957|   44.686|
| Kenya   |  1962|   47.949|
| Kenya   |  1967|   50.654|
| Kenya   |  1972|   53.559|
| Kenya   |  1977|   56.155|
| Kenya   |  1982|   58.766|
| Kenya   |  1987|   59.339|
| Kenya   |  1992|   59.285|
| Kenya   |  1997|   54.407|
| Kenya   |  2002|   50.992|
| Kenya   |  2007|   54.110|

Here we can see the gapminder subset with 3 countries in a long format (with year and lifeExp).

``` r
gap_subset %>%
  spread(key = "country", value = "lifeExp") %>%
  knitr::kable()
```

|  year|  Iceland|   Japan|   Kenya|
|-----:|--------:|-------:|-------:|
|  1952|   72.490|  63.030|  42.270|
|  1957|   73.470|  65.500|  44.686|
|  1962|   73.680|  68.730|  47.949|
|  1967|   73.730|  71.430|  50.654|
|  1972|   74.460|  73.420|  53.559|
|  1977|   76.110|  75.380|  56.155|
|  1982|   76.990|  77.110|  58.766|
|  1987|   77.230|  78.670|  59.339|
|  1992|   78.770|  79.360|  59.285|
|  1997|   78.950|  80.690|  54.407|
|  2002|   80.500|  82.000|  50.992|
|  2007|   81.757|  82.603|  54.110|

After `spread`, we can see that the columns are the lifeExp for the 3 countries and each row is a year.

``` r
gap_subset %>%
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Life Expectancy")
```

![](hw04-tidy-join_files/figure-markdown_github/unnamed-chunk-4-1.png)

Now we have a scatter plot with line to visualize. Note, I have changed the theme and the axis label on the Y.

### Activity 3

Compute some measure of life expectancy (mean? median? min? max?) for all possible combinations of continent and year. Reshape that to have one row per year and one variable for each continent. Or the other way around: one row per continent and one variable per year.

``` r
med_lifeEx <- gapminder %>%
  group_by(continent, year) %>%
  summarise(med = median(lifeExp))

med_lifeEx %>%
  knitr::kable()
```

| continent |  year|      med|
|:----------|-----:|--------:|
| Africa    |  1952|  38.8330|
| Africa    |  1957|  40.5925|
| Africa    |  1962|  42.6305|
| Africa    |  1967|  44.6985|
| Africa    |  1972|  47.0315|
| Africa    |  1977|  49.2725|
| Africa    |  1982|  50.7560|
| Africa    |  1987|  51.6395|
| Africa    |  1992|  52.4290|
| Africa    |  1997|  52.7590|
| Africa    |  2002|  51.2355|
| Africa    |  2007|  52.9265|
| Americas  |  1952|  54.7450|
| Americas  |  1957|  56.0740|
| Americas  |  1962|  58.2990|
| Americas  |  1967|  60.5230|
| Americas  |  1972|  63.4410|
| Americas  |  1977|  66.3530|
| Americas  |  1982|  67.4050|
| Americas  |  1987|  69.4980|
| Americas  |  1992|  69.8620|
| Americas  |  1997|  72.1460|
| Americas  |  2002|  72.0470|
| Americas  |  2007|  72.8990|
| Asia      |  1952|  44.8690|
| Asia      |  1957|  48.2840|
| Asia      |  1962|  49.3250|
| Asia      |  1967|  53.6550|
| Asia      |  1972|  56.9500|
| Asia      |  1977|  60.7650|
| Asia      |  1982|  63.7390|
| Asia      |  1987|  66.2950|
| Asia      |  1992|  68.6900|
| Asia      |  1997|  70.2650|
| Asia      |  2002|  71.0280|
| Asia      |  2007|  72.3960|
| Europe    |  1952|  65.9000|
| Europe    |  1957|  67.6500|
| Europe    |  1962|  69.5250|
| Europe    |  1967|  70.6100|
| Europe    |  1972|  70.8850|
| Europe    |  1977|  72.3350|
| Europe    |  1982|  73.4900|
| Europe    |  1987|  74.8150|
| Europe    |  1992|  75.4510|
| Europe    |  1997|  76.1160|
| Europe    |  2002|  77.5365|
| Europe    |  2007|  78.6085|
| Oceania   |  1952|  69.2550|
| Oceania   |  1957|  70.2950|
| Oceania   |  1962|  71.0850|
| Oceania   |  1967|  71.3100|
| Oceania   |  1972|  71.9100|
| Oceania   |  1977|  72.8550|
| Oceania   |  1982|  74.2900|
| Oceania   |  1987|  75.3200|
| Oceania   |  1992|  76.9450|
| Oceania   |  1997|  78.1900|
| Oceania   |  2002|  79.7400|
| Oceania   |  2007|  80.7195|

Here we can see the median lifeExp for each continent each year. It is in a long format.

``` r
med_lifeEx %>%
  spread(key = "continent", value = "med") %>%
  knitr::kable()
```

|  year|   Africa|  Americas|    Asia|   Europe|  Oceania|
|-----:|--------:|---------:|-------:|--------:|--------:|
|  1952|  38.8330|    54.745|  44.869|  65.9000|  69.2550|
|  1957|  40.5925|    56.074|  48.284|  67.6500|  70.2950|
|  1962|  42.6305|    58.299|  49.325|  69.5250|  71.0850|
|  1967|  44.6985|    60.523|  53.655|  70.6100|  71.3100|
|  1972|  47.0315|    63.441|  56.950|  70.8850|  71.9100|
|  1977|  49.2725|    66.353|  60.765|  72.3350|  72.8550|
|  1982|  50.7560|    67.405|  63.739|  73.4900|  74.2900|
|  1987|  51.6395|    69.498|  66.295|  74.8150|  75.3200|
|  1992|  52.4290|    69.862|  68.690|  75.4510|  76.9450|
|  1997|  52.7590|    72.146|  70.265|  76.1160|  78.1900|
|  2002|  51.2355|    72.047|  71.028|  77.5365|  79.7400|
|  2007|  52.9265|    72.899|  72.396|  78.6085|  80.7195|

Here we can see it in a `spread` form with one column per continent.

``` r
med_lifeEx %>%
  spread(key = "continent", value = "med") %>%
  gather(key = "continent", value = "med", -"year") %>%
  knitr::kable()
```

|  year| continent |      med|
|-----:|:----------|--------:|
|  1952| Africa    |  38.8330|
|  1957| Africa    |  40.5925|
|  1962| Africa    |  42.6305|
|  1967| Africa    |  44.6985|
|  1972| Africa    |  47.0315|
|  1977| Africa    |  49.2725|
|  1982| Africa    |  50.7560|
|  1987| Africa    |  51.6395|
|  1992| Africa    |  52.4290|
|  1997| Africa    |  52.7590|
|  2002| Africa    |  51.2355|
|  2007| Africa    |  52.9265|
|  1952| Americas  |  54.7450|
|  1957| Americas  |  56.0740|
|  1962| Americas  |  58.2990|
|  1967| Americas  |  60.5230|
|  1972| Americas  |  63.4410|
|  1977| Americas  |  66.3530|
|  1982| Americas  |  67.4050|
|  1987| Americas  |  69.4980|
|  1992| Americas  |  69.8620|
|  1997| Americas  |  72.1460|
|  2002| Americas  |  72.0470|
|  2007| Americas  |  72.8990|
|  1952| Asia      |  44.8690|
|  1957| Asia      |  48.2840|
|  1962| Asia      |  49.3250|
|  1967| Asia      |  53.6550|
|  1972| Asia      |  56.9500|
|  1977| Asia      |  60.7650|
|  1982| Asia      |  63.7390|
|  1987| Asia      |  66.2950|
|  1992| Asia      |  68.6900|
|  1997| Asia      |  70.2650|
|  2002| Asia      |  71.0280|
|  2007| Asia      |  72.3960|
|  1952| Europe    |  65.9000|
|  1957| Europe    |  67.6500|
|  1962| Europe    |  69.5250|
|  1967| Europe    |  70.6100|
|  1972| Europe    |  70.8850|
|  1977| Europe    |  72.3350|
|  1982| Europe    |  73.4900|
|  1987| Europe    |  74.8150|
|  1992| Europe    |  75.4510|
|  1997| Europe    |  76.1160|
|  2002| Europe    |  77.5365|
|  2007| Europe    |  78.6085|
|  1952| Oceania   |  69.2550|
|  1957| Oceania   |  70.2950|
|  1962| Oceania   |  71.0850|
|  1967| Oceania   |  71.3100|
|  1972| Oceania   |  71.9100|
|  1977| Oceania   |  72.8550|
|  1982| Oceania   |  74.2900|
|  1987| Oceania   |  75.3200|
|  1992| Oceania   |  76.9450|
|  1997| Oceania   |  78.1900|
|  2002| Oceania   |  79.7400|
|  2007| Oceania   |  80.7195|

We can see that `gather` and reverts `spread` back to the original form.

``` r
med_lifeEx %>%
    ggplot(aes(x = year, y = med, color = continent)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Median Life Expectancy")
```

![](hw04-tidy-join_files/figure-markdown_github/unnamed-chunk-8-1.png)

Now we have a scatter plot with line to visualize. Note, I have changed the theme and the axis label on the Y. Additionally, the gathered format is easier for plotting, but the spread format is nicer for viewing in a table.

Join Prompts (join, merge, look up)
-----------------------------------
