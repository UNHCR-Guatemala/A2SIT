The results tab allows you to explore the results, as maps, tables and bar charts.
It also gives the option to select different scenarios.

#### Calculation of results

When you first click on the Results tab, the app aggregates each group of indicators into aggregate scores, and aggregates those scores together, following the specified structure, up to the overall index. The steps to do this are:

1. Treat any outliers
2. Normalise indicators onto a 1-100 scale
3. Aggregate using the weighted arithmetic mean, using the weights specified in your input spreadsheet.

The details of these steps are described in the A2SIT documentation's [data processing](https://unhcr-guatemala.github.io/A2SIT/book/data_processing.html) and [Aggregation and weighting](https://unhcr-guatemala.github.io/A2SIT/book/aggregation_weighting.html) chapters.

#### Exploring results

In the main "Results" box, the top level of the index is plotted on the map. You can select any indicator or aggregate using the dropdown menu in the top right of this box. You can also select whether to view the scores as a 1-5 (categorical) scale or as scores (raw values a the indicator level).

Clicking on any region will bring a up an information panel in the bottom right of the map, showing the main scores and ranks. You can click on "Go to profile" for a full profile of the selected region in the Profiles tab.

In the Results box you can also select the "Table" tab to see the results as a table.

Underneath the Results box, there is a bar chart of the same indicator/aggregate shown on the map. Use the "gear" icon in the top right for more options: by default only the top 50 regions are shown but you can select the bottom 50, or all. The checkbox selects whether to break down scores into their components on the bar chart.

#### Scenarios

By default the index is calculated using the arithmetic mean, which implies that low (high) scores in one indicator are perfectly compensated by high (low) scores in another. Optionally, select "Scenario 2" which uses the geometric mean: in this case lower scores in one indicator are less compensated by higher scores in others, or "Scenario 3" which implies even less compensation. Click "Recalculate" to see the new results.

Clicking on the 'gear' icon in the top right of this box opens the option to adjust weights. If this is enabled, you can interactively adjust weights for the highest level of the index (below the index).
