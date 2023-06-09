The indicator analysis tab allows you to explore your data in some depth,
and identifies potential statistical issues with indicators. It gives the
option to remove indicators within the app, but you may also wish to alter your input data.

Note that this tab is *completely optional* and you may skip it if you wish.

**Indicator analysis**

The indicator analysis table gives information about each indicator, and in particular flags
any potential statistical issues by highlighting cells in yellow.

* *% Availability* is the data availability: if less than 66% this is flagged
* *% Same* is the percentage of points that have the same value - e.g. if 50% of points are zero, this will be 50%. It is flagged if greater than 50%.
* *Skew/kurtosis* gives the skewness and kurtosis values for each indicator - these are used as simple indicators of outliers: if an indicator has absolute skewness > 2 and kurtosis > 3.5 it is flagged. Note that outliers are automatically treated when calculating the results - see the [A2SIT documentation](https://unhcr-guatemala.github.io/A2SIT/book/analysis.html) for more info.
* *Collinear with* reports any indicators that the indicator has a rank correlation of greater than 0.9 with *and* is within the same group at level 2. Flagged if any are present.
* *Negatively correlated with* is the same as the previous but for correlations less than -0.4.
* *Status* reports whether the indicator is currently included or excluded (by default all indicators are included).

For a deeper explanation of these criteria and why they are of interest, please see the [A2SIT documentation](https://unhcr-guatemala.github.io/A2SIT/book/analysis.html).

**Indicator information**

The indicator information box reports information about the indicator currently selected in the analysis table. It also includes the option to remove the selected indicator by clicking the "Remove" button, and to restore any removed indicators with the "Restore" button. When an indicator is removed, it is not included in the calculation of the index scores on the following tabs. Indicators can of course also be added and removed in your input data spreadsheet.

**Distribution**

The distribution plot shows the statistical distribution of the indicator selected in the analysis table. The intention is to show visually how each indicator might be skewed, or contain outliers. This can also be useful to spot errors, e.g. incorrect data points. Click on the "gear" icon in the top right corner of the box to toggle between a histogram and violin plot.

**Scatter plot**

The scatter plot allows plotting any indicator against any other. The x-axis indicator is selected by selecting an row in the analysis table. The y-axis indicator is controlled by clicking the "gear" icon in the top right and using the drop-down menu. Here you can also optionally add a linear trend line and toggle log axes (the latter option useful for skewed indicators).
