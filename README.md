# IEP Compliance Audit Calculator

This repository contains a Shiny calculator designed to support Office of Special Education (OSE) compliance audits that use samples of Individualized Education Programs (IEPs). The calculator helps determine when the number of noncompliant IEPs observed in an audit sample provides sufficiently strong evidence for flagging population noncompliance as above a selected systemic noncompliance threshold.

The tool is intended to make the sampling logic transparent for technical and non-technical reviewers. Users enter four audit parameters: (1) the total number of IEPs in the population, (2) the number of IEPs sampled, (3) the systemic noncompliance threshold, and (4) the required evidence level. The app then identifies the smallest observed sample count that would meet the selected evidence criterion, displays a lookup table for all possible sample results, and provides a technical summary of the exact probability calculation used.

## Statistical Approach

The calculator uses the hypergeometric distribution because an audit sample is drawn without replacement from a finite population of IEPs. For each possible observed number of noncompliant IEPs in the sample, the calculator computes and tabulates the exact upper-tail probability of observing that many or more noncompliant IEPs if the full population were just below the selected systemic noncompliance threshold.

The calculator presents this result as an exact probability rather than primarily using hypothesis-testing terminology. The underlying calculation is equivalent to a one-sided exact test against the largest whole-number population count below the systemic threshold, but the app is designed for audit interpretation rather than formal statistical reporting. Stating the probability directly answers the practical review question: If population noncompliance were at the boundary count below the selected systemic threshold, how likely would this sample result be? The selected evidence criterion then functions as the decision rule for when that probability is low enough to flag the result as above the systemic noncompliance threshold.

The minimum systemic noncompliance count is defined as:

```text
systemic_count = ceiling(systemic threshold x total population)
```

This is the smallest whole-number count of noncompliant IEPs that meets the selected systemic threshold. For example, if the threshold is 5% and the population contains 100 IEPs, 5 noncompliant IEPs is the first count treated as systemic noncompliance. The exact calculation then uses the largest whole-number count below that threshold:

```text
boundary_count = max(systemic_count - 1, 0)
```

The calculator evaluates:

```text
P(X >= x | N, boundary_count, sample_n)
```

where `N` is the total population size, `systemic_count` is the minimum count treated as systemic noncompliance, `boundary_count` is the largest count below that systemic threshold, `sample_n` is the number of IEPs sampled, `X` is the random number of noncompliant IEPs that could appear in a sample of that size if the population contained exactly `boundary_count` noncompliant IEPs, and `x` is the observed number of noncompliant IEPs in the sample.

The hypergeometric probability of observing exactly `j` noncompliant IEPs in the sample is:

$$
P(X = j) =
\frac{
  \binom{\text{boundary_count}}{j}
  \binom{N - \text{boundary_count}}{\text{sample_n} - j}
}{
  \binom{N}{\text{sample_n}}
}
$$

The calculator uses the corresponding upper-tail probability:

$$
P(X \ge x) =
\sum_{j = x}^{\min(\text{sample_n}, \text{boundary_count})}
\frac{
  \binom{\text{boundary_count}}{j}
  \binom{N - \text{boundary_count}}{\text{sample_n} - j}
}{
  \binom{N}{\text{sample_n}}
}
$$

In R, this is calculated as:

```r
phyper(q = x - 1, m = boundary_count, n = N - boundary_count, k = sample_n, lower.tail = FALSE)
```

The calculator uses `phyper()` from R's built-in `stats` package to compute exact hypergeometric tail probabilities. In R, `phyper(q, m, n, k, lower.tail = FALSE)` gives the probability of drawing more than `q` successes in a sample of size `k`, without replacement, from a finite population containing `m` successes and `n` non-successes.

For this calculator, the arguments are mapped as follows: `boundary_count` is the largest whole-number count of noncompliant IEPs below the systemic threshold, `N - boundary_count` is the number of compliant IEPs at that boundary, `sample_n` is the audit sample size, and `x` is the observed number of noncompliant IEPs in the sample. The use of `q = x - 1` is intentional. With `lower.tail = FALSE`, R returns `P(X > q)`, so setting `q` to `x - 1` gives the desired probability `P(X >= x)`.

The selected evidence level is converted to a corresponding exact upper-tail probability cutoff: exact probability cutoff = 1 - evidence level. For example, a 95% evidence criterion corresponds to a 5% probability cutoff. A sample result is treated as sufficient evidence for flagging population noncompliance as above the systemic threshold when the exact upper-tail probability is less than or equal to that cutoff.

## How to Use the App

The app is currently hosted on shinyapps.io at https://steveneheil.shinyapps.io/ose-sample-calculator/

1. Enter the total number of IEPs in the population being audited.
2. Enter the number of IEPs included in the audit sample.
3. Enter the percent of all IEPs that indicates systemic noncompliance.
4. Enter the required evidence level for treating the sample result as sufficient evidence.

The app reports the first observed sample count that meets the evidence criterion, shows the corresponding probability relationship in a plot, and provides a lookup table for each possible number of noncompliant IEPs in the sample.

## Running Locally

Open `OSE-app.R` in RStudio or run it from R with the required packages installed:

```r
install.packages(c("shiny", "dplyr", "DT", "scales", "ggplot2"))
shiny::runApp("OSE-app.R")
```

## Interpretation

This calculator does not determine whether an individual IEP is compliant or noncompliant. That determination must come from the underlying audit review. The calculator addresses a narrower question: Given a finite population, a sample size, a systemic threshold, and a required evidence level, how many observed noncompliant IEPs are needed before the sample provides sufficient evidence for flagging population noncompliance as above the selected threshold?

## License and Attribution

This project is released under the MIT License. Others may use, copy, modify, and distribute the calculator, including for public or internal audit work, provided that the copyright and license notice are preserved.

Suggested attribution:

```text
IEP Compliance Audit Calculator by Steven Heil.
Licensed under the MIT License.
https://github.com/plateausteve/hypergeometric-probability-audit-calculator
```
