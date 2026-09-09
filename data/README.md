# Synthetic demonstration data

`demo.mif` is a populated, semicolon-delimited IAMC fixture made specifically for
this repository. It is **not an OPEN-PROM model run, historical dataset, forecast,
or policy recommendation**. The model field is `Synthetic Demo` and both apps
display a demo notice.

It contains 63 variables, three illustrative scenarios (`Baseline`,
`Delayed transition`, `Accelerated transition`), three disjoint example regions
(`EU`, `USA`, `IND`), and five years (2020, 2030, 2040, 2050, 2060): 567 wide
rows, or 2,835 numeric observations. No `World` aggregate is included, so selecting
all demo regions does not count a global total alongside its components.

The scenarios share 2020 starting values, then follow simple linear trajectories.
For example, EU CO2 emissions start at 3,200 Mt CO2/yr and reach 2,560 (Baseline),
1,440 (Delayed transition), or 480 (Accelerated transition) in 2060. USA values
are scaled by 1.5 and IND values by 0.8. Carbon prices are shared across regions.
The names describe illustrative contrasts; the trajectories do not encode a
calibrated policy schedule.

Energy and generation use EJ/yr; capacity uses GW; additions use GW/yr; emissions
and capture use Mt CO2/yr. GDP uses billion USD2010/yr, population uses millions,
and carbon prices use USD2010/t CO2.

The file intentionally includes emissions totals and nested Energy/Supply/Demand
categories to exercise hierarchy-aware aggregation. Energy accounts for 90% of
CO2, Land Use 10%, with Energy split 60:40 between Demand and Supply. Reported
parents must not be summed with their children. Primary-energy totals equal fuel
sums; final-energy sector totals equal carrier totals.

These stylized series are not a balanced energy-system simulation. The Sankey
view joins primary energy to final sectors without explicit transformation losses;
gross emissions, capture, and net emissions are not constrained to an accounting
identity. Maps use representative regional points, not geographic boundaries.

The automated tests assert known totals, renewable shares, filtering, hierarchy
handling, and coverage. The original `test.mif` is an unpopulated legacy template;
use `demo.mif` to try the dashboard.
