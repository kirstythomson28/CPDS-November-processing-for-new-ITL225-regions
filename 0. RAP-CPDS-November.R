# CPDS RAP
# Run part 1 of the scripts 
Part1scripts <- c(
  "1. CPDS-November-Utility.R",
  "2. Filtering mail list for reminder emails.R",
  "3. CPDS-November-Production-and-Disposal-Shaping.R",
  "4. CPDS-November-Production-QA.R",
  "5. CPDS-November-Disposals-QA.R"
)

# Run them all with logging
for (s in Part1scripts) {
  cat(">>> Starting:", s, "\n")
  tryCatch(
    {
      source(s)
      cat(">>> Finished:", s, "\n\n")
    },
    error = function(e) {
      cat("!!! Error in", s, ":", conditionMessage(e), "\n\n")
    }
  )
}

#### Once decided which outliers to keep run:
Finalised_removals <- read_excel(
  file.path("November results",
            "2025-26 - November - Production - Data - QA - Removals (FF, WC and yield Outliers) - 24 November 08-56.xlsx"))

Part2scripts <- c(
  "6. CPDS-November-Production-Processing.R",
# "7. CPDS-November-Production-Confidence Intervals.R",
  "8. CPDS Final Estimates Production Plots.R"
# ,
# "9. CPDS-November-Selecting June sample.R"
)

# Run them all with logging
for (s in Part2scripts) {
  cat(">>> Starting:", s, "\n")
  tryCatch(
    {
      source(s)
      cat(">>> Finished:", s, "\n\n")
    },
    error = function(e) {
      cat("!!! Error in", s, ":", conditionMessage(e), "\n\n")
    }
  )
}
