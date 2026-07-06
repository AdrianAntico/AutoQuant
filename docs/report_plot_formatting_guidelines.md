# Report Plot Formatting Guidelines

AutoPlots owns visual theme defaults for report plots. AutoQuant report generators should not override axis label colors, fonts, sizes, margins, or other theme styling with report-side eCharts post-processing.

AutoQuant reports may still pass semantic and readability parameters through AutoPlots function arguments:

- use `xAxis.axisLabel.rotate` when categorical or binned x-axis labels need rotation
- use `tooltip.show = FALSE` for `AutoPlots::Box()` report plots
- use `tooltip.show = FALSE` for heatmaps when crowded axis labels make hover behavior noisy
- use blank axis titles when the plotted column is only a helper field such as `feature`, `category`, `bin`, `value`, `segment`, or `period`
- preserve meaningful axis titles when they are target, prediction, metric, date, group, or source feature names

For flipped-coordinate / horizontal ranking bars:

- sort the plotted data ascending by the plotted numeric value before plotting
- after `e_flip_coords()`, the largest values should appear at the top of the chart
- apply this to variable importance, interaction importance, top correlation bars, categorical ranking bars, metric ranking bars, and other horizontal bar/ranking plots
- do not reverse-sort unless the plotting library behavior is explicitly verified to require it

Numeric display should stay readable:

- round report table values consistently
- avoid long decimal strings in plot labels, heatmap values, and table cells
- preserve raw numeric values for sorting and filtering where possible

Numeric bins should be ordered by numeric lower/upper/order metadata, not lexicographic label order.
