# Map seasonal occurrence frequency for individual plankton species

Map seasonal occurrence frequency for individual plankton species

## Usage

``` r
pr_plot_FreqMap(dat, species, interactive = TRUE)
```

## Arguments

- dat:

  dataframe of format similar to output of pr_get_fmap_data()

- species:

  species to plot

- interactive:

  ggplot if false, plotlist of leaflets if true

## Value

a plot of frequency of occurrence of chosen species

## Examples

``` r
dat <- data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                 freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                 Season = c("December - February","March - May",
                 "June - August","September - November"),
                 Taxon = 'Acartia danae',
                 Survey = 'CPR')
plot <- pr_plot_FreqMap(dat, species = 'Acartia danae', interactive = TRUE)
```
