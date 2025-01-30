library(tidyverse)
library(RColorBrewer)  
data <- read.csv("Commun_Dom_Down.csv", stringsAsFactors = FALSE, na.strings = "#N/A") #or UP
data <- data %>%
  mutate(Domain.Annotations = ifelse(is.na(Domain.Annotations) | Domain.Annotations == "", "None", Domain.Annotations))

# Split domain annotations
domain_list <- unlist(str_split(data$Domain.Annotations, ";"))

# Trim whitespace and count occurrences of each unique domain
domain_counts <- domain_list %>%
  str_trim() %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))
colnames(domain_counts) <- c("Domain", "Count")


# Assign categories from domain names
assign_dynamic_category <- function(domain_annotation) {
  for (domain in domain_counts$Domain) {
    if (domain_annotation == "None") return("Other")
    if (str_detect(domain_annotation, fixed(domain))) {
      return(domain)
    }
  }
  return("Other")  #Unmatched domains
}

# Categorize domains
data_cat <- data %>%
  rowwise() %>%
  mutate(Category = assign_dynamic_category(Domain.Annotations))

#HOGs and assigned category
# Add column with the assigned category for each HOG
data <- data %>%
  rowwise() %>%
  mutate(Category = assign_dynamic_category(Domain.Annotations)) %>%
  ungroup()  # Ensure the data is no longer grouped


# Summarize counts by category
category_counts <- data_cat %>%
  group_by(Category) %>%
  summarize(Total = n())


####### UP#######
#Pie w/o "other" & only domains with at least 10 HOGs
filtered_category_counts <- category_counts %>%
  filter(Total >= 10, Category != "Other")


# Sort the data by Total in descending order
filtered_category_counts <- filtered_category_counts %>%
  arrange(desc(Total))
# Subset reds
colors <- colorRampPalette(brewer.pal(n = 9, "Reds"))(nrow(filtered_category_counts) + 2)[2:(nrow(filtered_category_counts) + 1)]
colors <- rev(colors)

pie(filtered_category_counts$Total,
    labels = paste(filtered_category_counts$Category, "(", filtered_category_counts$Total, ")", sep = ""),
    main = "Domain Categories Distribution (Filtered)",
    col = colors
)

####### DOWN #######
#Pie w/o "other" & only domains with at least 2 HOGs
filtered_category_counts <- category_counts %>%
  filter(Total >= 2, Category != "Other")

# Sort the data by Total in descending order
filtered_category_counts <- filtered_category_counts %>%
  arrange(desc(Total))

# Subset blues
colors <- colorRampPalette(brewer.pal(n = 9, "Blues"))(nrow(filtered_category_counts) + 2)[2:(nrow(filtered_category_counts) + 1)]
colors <- rev(colors)
pie(filtered_category_counts$Total,
    labels = paste(filtered_category_counts$Category, "(", filtered_category_counts$Total, ")", sep = ""),
    main = "Domain Categories Distribution (Filtered)",
    col = colors
)

