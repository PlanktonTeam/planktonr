# Create pie charts showing functional group composition

Visualise the relative proportion of plankton functional groups averaged
across all samples. Useful for summarising community composition in a
simple, accessible format.

## Usage

``` r
pr_plot_PieFG(dat)
```

## Arguments

- dat:

  A dataframe from
  [`pr_get_FuncGroups()`](https://planktonteam.github.io/planktonr/reference/pr_get_FuncGroups.md)
  containing functional group abundance or biomass data

## Value

A ggplot2 object that can be further customised or saved with
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Details

### Plot Structure

The pie chart shows:

- Each functional group as a wedge

- Wedge size proportional to mean abundance/biomass across all samples

- Colours from the "Set1" palette for clear distinction

- Legend below plot listing all functional groups

### Interpretation

This plot provides a quick overview of which functional groups dominate
the plankton community on average. Use this for:

- Initial data exploration

- Comparing overall community structure between surveys or regions

- Educational presentations requiring simple visualisations

### Limitations

- Shows average composition only, hiding temporal variability

- Cannot show changes over time (use
  [`pr_plot_tsfg()`](https://planktonteam.github.io/planktonr/reference/pr_plot_tsfg.md)
  for that)

- Works best with 5-10 functional groups; too many makes wedges hard to
  distinguish

### Functional Groups

The plot automatically detects whether data are:

- **Phytoplankton**: Diatoms, dinoflagellates, ciliates, etc.

- **Zooplankton**: Copepods, appendicularians, fish larvae, etc.

And labels the legend accordingly.

## See also

- [`pr_get_FuncGroups()`](https://planktonteam.github.io/planktonr/reference/pr_get_FuncGroups.md)
  to generate input data

- [`pr_plot_tsfg()`](https://planktonteam.github.io/planktonr/reference/pr_plot_tsfg.md)
  for time series of functional group composition

## Examples

``` r
# Phytoplankton functional groups from CPR
dat <- pr_get_FuncGroups("CPR", "Phytoplankton")
plot <- pr_plot_PieFG(dat)
print(plot)


# Zooplankton functional groups from NRS
dat <- pr_get_FuncGroups("NRS", "Zooplankton")
pr_plot_PieFG(dat)

```
