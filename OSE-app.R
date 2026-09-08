# app.R

library(shiny)
library(dplyr)
library(DT)
library(scales)
library(ggplot2)

#--------------------------------------------------------
# Function: hypergeometric hypothesis-test lookup table
#--------------------------------------------------------

make_hypergeom_lookup <- function(N, sample_n, threshold_pct, evidence_cutoff) {
    
    threshold <- threshold_pct / 100
    
    systemic_count <- ceiling(threshold * N)
    boundary_count <- max(systemic_count - 1, 0)
    
    # Possible observed noncompliant counts in the sample
    x_values <- 0:sample_n
    
    lookup <- tibble(
        N = N,
        sample_n = sample_n,
        threshold_pct = threshold_pct,
        systemic_count = systemic_count,
        boundary_count = boundary_count,
        boundary_noncompliance_rate = boundary_count / N,
        observed_noncompliant = x_values,
        observed_sample_rate = observed_noncompliant / sample_n
    ) |>
        mutate(
            # One-sided exact p-value:
            # P(X >= observed x | N, boundary_count, sample_n)
            exact_tail_probability = phyper(
                q = observed_noncompliant - 1,
                m = boundary_count,
                n = N - boundary_count,
                k = sample_n,
                lower.tail = FALSE
            ),
            evidence_reaches_threshold = exact_tail_probability <= evidence_cutoff,
            conclusion = if_else(
                evidence_reaches_threshold,
                paste0("Flags systemic noncompliance at or above ", 
                       threshold_pct, 
                       "% at minimum ",
                       100 * (1 - evidence_cutoff),
                       "% certainty."
                ),
                paste0("Does not flag systemic noncompliance at or above ",
                      threshold_pct,
                      "% at minimum ",
                      100 * (1 - evidence_cutoff),
                      "% certainty."
                )
            )
        ) |>
        mutate(
            evidence_cutoff = evidence_cutoff,
            threshold_label = percent(threshold, accuracy = 0.1),
            systemic_rate_label = percent(systemic_count / N, accuracy = 0.1),
            boundary_noncompliance_rate_label = percent(boundary_noncompliance_rate, accuracy = 0.1),
            observed_sample_rate_label = percent(observed_sample_rate, accuracy = 0.1),
            certainty_label = percent(1 - exact_tail_probability, accuracy = 0.1),
            exact_tail_probability_label = if_else(
              exact_tail_probability < 0.001,
                "< .001",
                number(exact_tail_probability, accuracy = 0.001)
            )
        )
    
    lookup
}

#--------------------------------------------------------
# UI
#--------------------------------------------------------

ui <- fluidPage(
  
    tags$head(
      tags$style(HTML("
      #summary_text {
        white-space: pre-wrap;
        word-break: normal;
        overflow-wrap: anywhere;
        max-width: 100%;
      }
    "))
    ),
    
    titlePanel("IEP Compliance Audit Calculator"),
    
    sidebarLayout(
        
        sidebarPanel(
            helpText(
                "This calculator tests for systemic noncompliance in IEPs using a sample of all the IEPs in a population"
            ),
            helpText(
              "Enter the information for this audit below."
            ),
            sliderInput(
              "N",
              "1. Enter the total count of IEPs in the population.",
              min = 1,
              max = 500,
              value = 100,
              step = 1,
              sep = ",",
              ticks = FALSE
            ),
            
            numericInput(
                inputId = "sample_n",
                label = "2. Enter the sampled number of IEPs in the audit.",
                value = 20,
                min = 1,
                step = 1
            ),
            
            numericInput(
                inputId = "threshold_pct",
                label = "3. Choose the percent of all IEPs at a school that indicates systemic noncompliance.",
                value = 5,
                min = 0,
                max = 100,
                step = 1
            ),
            
            numericInput(
                inputId = "certainty",
                label = "4. Choose the required percent certainty for treating the sample as evidence of systemic noncompliance.",
                value = 95,
                min = 50,
                max = 99,
                step = 1
            ),
        ),
        
        mainPanel(
            
            h3("Check Sample Evidence for Systemic Noncompliance"),
            
            uiOutput("interpretation_text"),
            
            h3("Sample Evidence Relative to the Systemic Threshold"),
            
            plotOutput("certainty_plot", height = "350px"),
            
            h3("Lookup Table by Observed Sample Count"),
            
            DTOutput("lookup_table"),
            
            br(),
            
            h3("Technical Summary: Exact Hypergeometric Probability Calculation"),
            
            verbatimTextOutput("summary_text")
        )
    )
)

#--------------------------------------------------------
# Server
#--------------------------------------------------------
server <- function(input, output, session) {
  
  # Keep sample size from exceeding N
  observeEvent(input$N, {
    updateNumericInput(
      session,
      inputId = "sample_n",
      max = input$N
    )
  })
  
  validated_inputs <- reactive({
    
    N <- as.integer(input$N)
    sample_n <- as.integer(input$sample_n)
    threshold_pct <- input$threshold_pct
    certainty <- input$certainty
    evidence_cutoff <- 1 - (certainty/100)
    
    validate(
      need(!is.na(N) && N >= 1, "N must be at least 1."),
      need(!is.na(sample_n) && sample_n >= 1, "Sample size n must be at least 1."),
      need(sample_n <= N, "Sample size n cannot exceed total IEP count N."),
      need(!is.na(threshold_pct) && threshold_pct >= 0 && threshold_pct <= 100,
           "Threshold percent must be between 0 and 100."),
      need(!is.na(certainty) && certainty >= 50 && certainty < 100,
           "Certainty must be at least 50 and less than 100.")
    )
    
    list(
      N = N,
      sample_n = sample_n,
      threshold_pct = threshold_pct,
      certainty = certainty,
      evidence_cutoff = evidence_cutoff
    )
  })
  
  lookup_data <- reactive({
    
    vals <- validated_inputs()
    
    make_hypergeom_lookup(
      N = vals$N,
      sample_n = vals$sample_n,
      threshold_pct = vals$threshold_pct,
      evidence_cutoff = vals$evidence_cutoff
    )
  })
  
  output$summary_text <- renderText({
    
    vals <- validated_inputs()
    lookup <- lookup_data()
    
    critical_row <- lookup |>
      filter(evidence_reaches_threshold) |>
      slice_head(n = 1)
    
    systemic_count <- ceiling((vals$threshold_pct / 100) * vals$N)
    boundary_count <- max(systemic_count - 1, 0)
    
    systemic_rate <- systemic_count / vals$N
    boundary_rate <- boundary_count / vals$N
    
    cutoff_label <- percent(
      vals$evidence_cutoff,
      accuracy = 0.1
    )
    
    if (nrow(critical_row) == 0) {
      
      critical_statement <- paste0(
        "No possible sample count from 0 to ",
        vals$sample_n,
        " produces an exact probability at or below the corresponding exact probability cutoff of ",
        cutoff_label,
        "."
      )
      
    } else {
      
      critical_statement <- paste0(
        "Evidence threshold: Observing ",
        critical_row$observed_noncompliant,
        " or more noncompliant IEPs in the sample produces an exact tail probability at or below ",
        cutoff_label,
        " when the population contains ",
        boundary_count,
        " noncompliant IEPs, the largest whole-number count below the systemic threshold."
      )
    }
    
    paste0(
      "Inputs\n",
      "Population N = ", vals$N, "\n",
      "Sample n = ", vals$sample_n, "\n",
      "Systemic threshold = ", vals$threshold_pct, "% of N\n",
      "Required certainty = ", vals$certainty, "%\n",
      "Corresponding exact probability cutoff = ", cutoff_label, "\n\n",
      
      "Derived values\n",
      "Minimum systemic noncompliance count = ceiling(",
      vals$threshold_pct, "% x ", vals$N, ") = ",
      systemic_count, "\n",
      "Minimum systemic noncompliance rate = ",
      percent(systemic_rate, accuracy = 0.1), "\n",
      "Boundary noncompliance count = ",
      boundary_count, "\n\n",
      
      "Exact hypergeometric probability calculation\n",
      "This calculator uses the hypergeometric distribution because the audit sample is drawn ",
      "without replacement from a finite population of IEPs.\n\n",
      
      "For each possible observed sample count x, the calculator computes the exact probability ",
      "of observing that many or more noncompliant IEPs in the sample when the population contains ",
      boundary_count,
      " noncompliant IEPs, the largest whole-number count below the selected ",
      "systemic threshold.\n\n",
      
      "Formula\n",
      "P(X >= x | N, boundary_count, sample_n)\n\n",
      
      "Where:\n",
      "N = total number of IEPs in the population\n",
      "systemic_count = ceiling(systemic threshold x N), the smallest whole-number count of noncompliant IEPs ",
      "that meets the selected systemic threshold.\n",
      "boundary_count = max(systemic_count - 1, 0), the largest whole-number count below the selected systemic threshold.\n",
      "sample_n = number of IEPs sampled.\n",
      "X = the random number of noncompliant IEPs that could appear in a sample of that size if the population contained exactly boundary_count noncompliant IEPs\n", 
      "x = observed number of noncompliant IEPs in the sample\n\n",
      
      "In R, this is calculated as:\n",
      "phyper(q = x - 1, m = boundary_count, n = N - boundary_count, k = sample_n, lower.tail = FALSE)\n\n",
      
      "Certainty criterion: ",
      "The selected certainty is converted to a corresponding exact probability cutoff: ",
      "exact probability cutoff = 1 - certainty. ",
      "A sample provides sufficient evidence of systemic noncompliance in the population when ",
      "its exact tail probability is at or below that cutoff.\n\n",
      
      critical_statement,
      "\n\n",
      
      "R documentation for phyper(): ",
      "https://stat.ethz.ch/R-manual/R-devel/library/stats/help/Hypergeometric.html"
    )
  })
  
  output$lookup_table <- renderDT({
    
    lookup <- lookup_data() |>
      transmute(
        `Observed noncompliant count in sample` = observed_noncompliant,
        `Observed noncompliant rate in sample` = observed_sample_rate_label,
        `Probability of systemic noncompliance in population` = certainty_label,
        `Systemic noncompliance threshold reached?` = if_else(evidence_reaches_threshold, "Yes", "No"),
        `Conclusion` = conclusion,
        row_status = if_else(evidence_reaches_threshold, "Reached", "Not reached")
      )
    
    datatable(
      lookup,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        autoHideNavigation = TRUE,
        searching = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = ncol(lookup) - 1)
        )
      )
    ) |>
      formatStyle(
        columns = names(lookup),
        valueColumns = "row_status",
        backgroundColor = styleEqual(
          c("Not reached", "Reached"),
          c("#e8f5e9", "#fde2e7")
        )
      )
  })
  
  output$certainty_plot <- renderPlot({
    
    vals <- validated_inputs()
    lookup <- lookup_data() |>
      mutate(certainty_level = 1 - exact_tail_probability)
    
    selected_certainty <- vals$certainty / 100
    
    critical_row <- lookup |>
      filter(evidence_reaches_threshold) |>
      slice_head(n = 1)
    
    p <- ggplot(
      lookup,
      aes(
        x = observed_noncompliant,
        y = certainty_level
      )
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_hline(
        yintercept = selected_certainty,
        linetype = "dashed",
        linewidth = 0.8
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      scale_x_continuous(
        breaks = lookup$observed_noncompliant
      ) +
      labs(
        x = "Count of noncompliant IEPs in sample",
        y = "Probability of systemic noncompliance",
        caption = "Points at or above the line provide sufficient evidence that population noncompliance meets the systemic threshold."
      ) +
      theme_minimal(base_size = 13)
    
    if (nrow(critical_row) > 0) {
      p <- p +
        geom_vline(
          xintercept = critical_row$observed_noncompliant,
          linetype = "dotted",
          linewidth = 0.8
        ) +
        annotate(
          "text",
          x = max(lookup$observed_noncompliant),
          y = selected_certainty,
          label = paste0("Required certainty: ", percent(selected_certainty)),
          hjust = 1,
          vjust = 2,
          size = 5
        ) +
        annotate(
          "text",
          x = critical_row$observed_noncompliant,
          y = 0.10,
          label = paste0(
            "Required certainty reached at ",
            critical_row$observed_noncompliant,
            " or more"
          ),
          hjust = -0.05,
          size = 5
        )
    }
    
    p
  })
  
  output$interpretation_text <- renderUI({
      
      vals <- validated_inputs()
      lookup <- lookup_data()
      
      critical_row <- lookup |>
          filter(evidence_reaches_threshold) |>
          slice_head(n = 1)
      
      systemic_count <- ceiling((vals$threshold_pct / 100) * vals$N)
      boundary_count <- max(systemic_count - 1, 0)
      systemic_rate <- systemic_count / vals$N
      
      if (nrow(critical_row) == 0) {
          HTML(
              paste0(
                "<p>For this combination of total IEPs, sample size, systemic threshold, and required certainty, ",
                "no possible sample result has an exact probability low enough to provide sufficient evidence that population noncompliance meets the systemic threshold.</p>",
                "<p>The selected noncompliance threshold corresponds to <b>",
                systemic_count,
                "</b> noncompliant IEPs, or <b>",
                percent(systemic_rate),
                "</b> of all IEPs at the school.</p>"
              )
          )
      } else {
          HTML(
              paste0(
                  "<p>If the audit finds <b>",
                  critical_row$observed_noncompliant,
                  " or more</b> noncompliant IEPs in the sample of <b>",
                  vals$sample_n,
                  "</b>, the exact probability of observing that many or more noncompliant IEPs when the population is just below the systemic threshold is at or below <b>",
                  percent(vals$evidence_cutoff),
                  "</b>. This meets the selected <b>",
                  percent(1 - vals$evidence_cutoff),
                  "</b> certainty criterion for sufficient evidence that population noncompliance meets the systemic threshold.</p>",
                  "<p>The selected threshold for systemic noncompliance would correspond to <b>",
                  systemic_count,
                  "</b> noncompliant IEPs, or <b>",
                  percent(systemic_rate),
                  "</b> of all IEPs at the school. The exact calculation compares the sample against <b>",
                  boundary_count,
                  "</b> noncompliant IEPs, the largest whole-number count below that threshold."
              )
          )
      }
  })
}

#--------------------------------------------------------
# Run app
#--------------------------------------------------------

shinyApp(ui = ui, server = server)
