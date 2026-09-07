# IEP Compliance Audit Calculator

This repository contains a Shiny calculator designed to support Office of Special Education (OSE) compliance audits that use samples of Individualized Education Programs (IEPs). The calculator helps determine when the number of noncompliant IEPs observed in an audit sample provides sufficiently strong evidence that noncompliance in the full population exceeds a selected systemic noncompliance threshold.

The tool is intended to make the sampling logic transparent for technical and non-technical reviewers. Users enter four audit parameters: (1) the total number of IEPs in the population, (2) the number of IEPs sampled, (3) the systemic noncompliance threshold, and (4) the required certainty level. The app then identifies the smallest observed sample count that would meet the selected certainty criterion, displays a lookup table for all possible sample results, and provides a technical summary of the exact probability calculation used.

## Statistical Approach

The calculator uses the hypergeometric distribution because an audit sample is drawn without replacement from a finite population of IEPs. For each possible observed number of noncompliant IEPs in the sample, the calculator computes and tabulates the exact upper-tail probability of observing that many or more noncompliant IEPs if the full population were at the selected threshold boundary.

The calculator presents this result as an exact probability rather than primarily using hypothesis-testing terminology. The underlying calculation is equivalent to a one-sided exact test against the threshold boundary, but the app is designed for audit interpretation rather than formal statistical reporting. Stating the probability directly answers the practical review question: If population noncompliance were no higher than the selected threshold boundary, how likely would this sample result be? The selected certainty level then functions as the decision rule for when that probability is low enough to treat the sample as sufficient evidence of above-threshold systemic noncompliance.

The threshold boundary is defined as:

```text
threshold_count = floor(systemic threshold x total population)
```

This is the largest whole-number count of noncompliant IEPs that does not exceed the selected systemic threshold. The calculator then evaluates:

```text
P(X >= x | N, threshold_count, sample_n)
```

where `N` is the total population size, `threshold_count` is the threshold-boundary count, `sample_n` is the number of IEPs sampled, `X` is the random number of noncompliant IEPs that could appear in a sample of that size if the population contained exactly `threshold_count` noncompliant IEPs, and `x` is the observed number of noncompliant IEPs in the sample. In R, this is calculated as:

```r
phyper(q = x - 1, m = threshold_count, n = N - threshold_count, k = sample_n, lower.tail = FALSE)
```

The calculator uses `phyper()` from R's built-in `stats` package to compute exact hypergeometric tail probabilities. In R, `phyper(q, m, n, k, lower.tail = FALSE)` gives the probability of drawing more than `q` successes in a sample of size `k`, without replacement, from a finite population containing `m` successes and `n` non-successes.

For this calculator, the arguments are mapped as follows: `threshold_count` is the number of noncompliant IEPs at the threshold boundary, `N - threshold_count` is the number of compliant IEPs at that boundary, `sample_n` is the audit sample size, and `x` is the observed number of noncompliant IEPs in the sample. The use of `q = x - 1` is intentional. With `lower.tail = FALSE`, R returns `P(X > q)`, so setting `q` to `x - 1` gives the desired probability `P(X >= x)`.

The selected certainty level is converted to an exact probability cutoff. For example, a 95% certainty requirement corresponds to a 5% probability cutoff. A sample result is treated as sufficient evidence of above-threshold population noncompliance when the exact upper-tail probability is less than or equal to that cutoff.

## How to Use the App

The app is currently hosted on shinyapps.io at https://steveneheil.shinyapps.io/ose-sample-calculator/

1. Enter the total number of IEPs in the population being audited.
2. Enter the number of IEPs included in the audit sample.
3. Enter the percent of all IEPs that population noncompliance must exceed to indicate systemic noncompliance.
4. Enter the required certainty level for treating the sample result as sufficient evidence.

The app reports the first observed sample count that meets the certainty criterion, shows the corresponding probability relationship in a plot, and provides a lookup table for each possible number of noncompliant IEPs in the sample.

## Running Locally

Open `OSE-app.R` in RStudio or run it from R with the required packages installed:

```r
install.packages(c("shiny", "dplyr", "DT", "scales", "ggplot2"))
shiny::runApp("OSE-app.R")
```

## Interpretation

This calculator does not determine whether an individual IEP is compliant or noncompliant. That determination must come from the underlying audit review. The calculator addresses a narrower question: Given a finite population, a sample size, a systemic threshold, and a required certainty level, how many observed noncompliant IEPs are needed before the sample provides exact statistical evidence that population noncompliance exceeds the selected threshold?

## License and Attribution

This project is released under the MIT License. Others may use, copy, modify, and distribute the calculator, including for public or internal audit work, provided that the copyright and license notice are preserved.

Suggested attribution:

```text
IEP Compliance Audit Calculator by Steven Heil.
Licensed under the MIT License.
https://github.com/plateausteve/hypergeometric-probability-audit-calculator
```
