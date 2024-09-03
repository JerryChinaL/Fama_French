# Sample data for means and t-stats (replace with actual data)
means1 <- matrix(runif(25, 0, 2), nrow=5, ncol=5)
means2 <- matrix(runif(25, 0, 2), nrow=5, ncol=5)
means3 <- matrix(runif(25, 0, 2), nrow=5, ncol=5)

tstats1 <- matrix(runif(25, -3, 3), nrow=5, ncol=5)
tstats2 <- matrix(runif(25, -3, 3), nrow=5, ncol=5)
tstats3 <- matrix(runif(25, -3, 3), nrow=5, ncol=5)

# Significance level
alpha <- 0.05

# Function to mark bold if significant
mark_bold <- function(means, tstats, alpha) {
  result_means <- matrix("", nrow=nrow(means), ncol=ncol(means))
  result_tstats <- matrix("", nrow=nrow(means), ncol=ncol(means))
  for (i in 1:nrow(means)) {
    for (j in 1:ncol(means)) {
      if (abs(tstats[i, j]) > 1.96) {
        result_means[i, j] <- paste0("\\textbf{", sprintf("%.2f", means[i, j]), "}")
        result_tstats[i, j] <- paste0("\\textbf{", sprintf("%.2f", tstats[i, j]), "}")
      } else {
        result_means[i, j] <- sprintf("%.2f", means[i, j])
        result_tstats[i, j] <- sprintf("%.2f", tstats[i, j])
      }
    }
  }
  return(list(means=result_means, tstats=result_tstats))
}

# Mark bold significant values
panel1 <- mark_bold(means1, tstats1, alpha)
panel2 <- mark_bold(means2, tstats2, alpha)
panel3 <- mark_bold(means3, tstats3, alpha)

# Create the LaTeX code for the table
output <- paste0("
\\documentclass{article}
\\usepackage{geometry}
\\geometry{letterpaper, margin=1in}
\\usepackage{array}
\\usepackage{booktabs}
\\begin{document}

\\begin{tabular}{p{1.8cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{0.8cm}}
 \\hline
  & Low & 2 & 3 & 4 & High & Low & 2 & 3 & 4 & High \\\\
 \\multicolumn{5}{c}{Arithmetic Mean} & \\multicolumn{5}{c}{T Stat} \\\\
 \\hline
 \\multicolumn{4}{l}{Panel A: SIZE - BM Sorts} & & & & & & & \\\\
 Illiquid &", paste(panel1$means[1,], collapse=" & "), "&", paste(panel1$tstats[1,], collapse=" & "), "\\\\
 2 &", paste(panel1$means[2,], collapse=" & "), "&", paste(panel1$tstats[2,], collapse=" & "), "\\\\
 3 &", paste(panel1$means[3,], collapse=" & "), "&", paste(panel1$tstats[3,], collapse=" & "), "\\\\
 4 &", paste(panel1$means[4,], collapse=" & "), "&", paste(panel1$tstats[4,], collapse=" & "), "\\\\
 Liquid &", paste(panel1$means[5,], collapse=" & "), "&", paste(panel1$tstats[5,], collapse=" & "), "\\\\
 \\multicolumn{4}{l}{Panel B: SIZE - IA Sorts} & & & & & & & \\\\
 Illiquid &", paste(panel2$means[1,], collapse=" & "), "&", paste(panel2$tstats[1,], collapse=" & "), "\\\\
 2 &", paste(panel2$means[2,], collapse=" & "), "&", paste(panel2$tstats[2,], collapse=" & "), "\\\\
 3 &", paste(panel2$means[3,], collapse=" & "), "&", paste(panel2$tstats[3,], collapse=" & "), "\\\\
 4 &", paste(panel2$means[4,], collapse=" & "), "&", paste(panel2$tstats[4,], collapse=" & "), "\\\\
 Liquid &", paste(panel2$means[5,], collapse=" & "), "&", paste(panel2$tstats[5,], collapse=" & "), "\\\\
 \\multicolumn{4}{l}{Panel C: SIZE - INV Sorts} & & & & & & & \\\\
 Illiquid &", paste(panel3$means[1,], collapse=" & "), "&", paste(panel3$tstats[1,], collapse=" & "), "\\\\
 2 &", paste(panel3$means[2,], collapse=" & "), "&", paste(panel3$tstats[2,], collapse=" & "), "\\\\
 3 &", paste(panel3$means[3,], collapse=" & "), "&", paste(panel3$tstats[3,], collapse=" & "), "\\\\
 4 &", paste(panel3$means[4,], collapse=" & "), "&", paste(panel3$tstats[4,], collapse=" & "), "\\\\
 Liquid &", paste(panel3$means[5,], collapse=" & "), "&", paste(panel3$tstats[5,], collapse=" & "), "\\\\
 \\hline
\\end{tabular}

\\end{document}
")

# Print the output
cat(output)
