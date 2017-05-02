library(dplyr)
library(extrafont)
library(ggplot2)
library(grid)
library(lubridate)
library(tidyr)

# Define party name mapping
party_names = list(
    "con" = "Conservative",
    "lab" = "Labour",
    "lib" = "Liberal Democrat",
    "green" = "Green",
    "ukip" = "UKIP"
)

# Define party colours to use
party_colours = c(
    "#0F80D6",  # con
    "#5EC500",  # green
    "#D20004",  # lab
    "#FEAA09",  # lib
    "#5E0D78"  # ukip
)

# Import & reshape data ready for plotting
polls = 'https://s3-eu-west-1.amazonaws.com/sixfifty/polls.csv' %>%
    read.csv() %>%
    # Coerce to datetime
    mutate(sampled_to = parse_date_time(sampled_to, "d m y")) %>%
    # Reshape from wide df into long df (party cols -> "party" and "pc" cols)
    gather("party", "pc", 6:10) %>%
    # Add new col + multiply pc by 100
    mutate(party_long = factor(unlist(party_names)[party],
                            levels = c("Conservative",
                                       "Green",
                                       "Labour",
                                       "Liberal Democrat",
                                       "UKIP")),
           pc = pc * 100)

# Visualise as scatterplot with LOESS smoothed averages
polls_plot = polls %>%
    filter(sampled_to >= ymd('2017-01-01')) %>%
    ggplot(aes(x=sampled_to, y=pc, colour=party_long)) +
        xlab("") + ylab("Percent")        

# Save as 1200x600 png
polls_1200 = polls_plot +
        geom_point(size=7, alpha=0.15, pch=16) +
        scale_colour_manual("Party", values=party_colours) +
        geom_smooth(method="loess", span=0.4, se=FALSE, size=2.1) +
        theme(text = element_text(size=28, family="Open Sans"),
              legend.key.height=unit(2.5,"line"))
ggsave("polls-1200x600.png", plot=polls_1200, width=12, height=6, dpi=100)

# Save as 600x300 png
polls_600 = polls_plot +
    geom_point(size=2, alpha=0.2, pch=16) +
    scale_colour_manual("Party", values=party_colours) +
    geom_smooth(method="loess", span=0.4, se=FALSE, size=1.0) +
    theme(text = element_text(size=12, family="Open Sans"))
ggsave("polls-600x300.png", plot=polls_600, width=6, height=3, dpi=100)

# Upload to S3
for (f in c("polls-1200x600.png", "polls-600x300.png")) {
    system_command = paste(c(
        "aws s3api put-object --bucket sixfifty --key '", f, "' ",
        "--body '", f, "' --acl 'public-read'"
        ), sep="", collapse="")
    system(system_command)
}
