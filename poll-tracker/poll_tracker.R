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
    "ld" = "Liberal Democrat",
    "grn" = "Green",
    "ukip" = "UKIP",
    "snp" = "SNP"
)

# Define party colours to use
party_colours = c(
    "#0F80D6",  # con
    "#5EC500",  # green
    "#D20004",  # lab
    "#FEAA09",  # lib
    "#5E0D78",  # ukip
    "#FFFF00"  # snp
)

# Import & reshape data ready for plotting
polls = 'https://s3-eu-west-1.amazonaws.com/sixfifty/polls.csv' %>%
    read.csv() %>%
    # Coerce to datetime
    mutate(to = ymd(to)) %>%
    # Reshape from wide df into long df (party cols -> "party" and "pc" cols)
    gather("party", "pc", 7:12) %>%
    # Add new col + multiply pc by 100
    mutate(party_long = factor(unlist(party_names)[party],
                            levels = c("Conservative",
                                       "Green",
                                       "Labour",
                                       "Liberal Democrat",
                                       "UKIP",
                                       "SNP")),
           pc = pc * 100)

# Visualise as scatterplot with LOESS smoothed averages
polls_plot = polls %>%
    filter(to >= ymd('2017-01-01')) %>%
    ggplot(aes(x=to, y=pc, colour=party_long)) +
        xlab("") + ylab("Percent") +
        geom_point(size=3, alpha=0.3, pch=16) +
        scale_colour_manual("Party", values=party_colours) +
        geom_smooth(method="loess", span=0.4, se=FALSE, size=1) +
        theme(text = element_text(size=18, family="Open Sans"),
              legend.key.height=unit(2.8, "line"),
              plot.margin = unit(c(0,0,0,0), "cm"))
ggsave("polls-1200x600.png", plot=polls_plot, width=12, height=6, dpi=100)

# Upload to S3
for (f in c("polls-1200x600.png")) {
    system_command = paste(c(
        "aws s3api put-object --bucket sixfifty --key '", f, "' ",
        "--body '", f, "' --acl 'public-read' --content-type 'image/png'"
        ), sep="", collapse="")
    system(system_command)
}
