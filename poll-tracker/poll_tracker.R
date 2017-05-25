library(dplyr)
library(extrafont)
library(ggplot2)
library(grid)
library(lubridate)
library(tidyr)

# Define party name mapping
party_names = list(
    "con" = "Conservatives",
    "lab" = "Labour",
    "ld" = "Liberal Democrats",
    "grn" = "Green",
    "ukip" = "UKIP",
    "snp" = "SNP",
    "bnp" = "BNP",
    "respect" = "Respect",
    "independent" = "Independent",
    "pc" = "Plaid Cymru",
    "dup" = "Democratic Unionist Party",
    "ulster_unionist_party" = "Ulster Unionist Party",
    "traditional_unionist_voice" = "Traditional Unionist Voice",
    "progressive_unionist_party" = "Progressive Unionist Party",
    "sinn_fein" = "Sinn Féin",
    "sdlp" = "Social Democratic and Labour Party",
    "alliance" = "Alliance Party of Northern Ireland",
    "people_before_profit_alliance" = "People Before Profit Alliance",
    "other" = "Other"
)

# Define party colours to use
party_colours_base = c(
    "#0F80D6",  # con
    "#D20004",  # lab
    "#FEAA09",  # lib
    "#5E0D78",  # ukip
    "#5EC500"  # green
)
party_colours_uk = c(
    party_colours_base,
    "#FFFF00"  # snp
)

party_colours_scotland = c(
    "#FFFF00",  # snp
    party_colours_base,
    "#999999"  # other
)

party_colours_london = c(
    party_colours_base,
    "#085081",  # bnp
    "#46801c",  # respect
    "#CCCCCC",  # independent
    "#999999"  # other
)

party_colours_wales = c(
    "#337D1E",  # PC
    party_colours_base,
    "#999999"  # other
)

party_colours_ni = c(
    "#D46A4C",  # DUP
    "#008800",  # Sinn Féin
    "#9999FF",  # UUP
    "#99FF66",  # SDLP
    "#F6CB2F",  # Alliance
    "#5EC500",  # Green
    "#660000",  # PBP
    "#0095B6",  # TUV
    "#022A68",  # PUP
    "#5E0D78",  # ukip
    "#0F80D6",  # Conservatives
    "#999999"  # other
)

# Import & reshape data ready for plotting
import_data = function(url, party_cols, party_order) {
    out = url %>%
        read.csv() %>%
        # Coerce to datetime
        mutate(to = ymd(to)) %>%
        # Reshape from wide df into long df (party cols -> "party" and "pc" cols)
        gather("party", "pc", party_cols) %>%
        # Add new col + multiply pc by 100
        mutate(party_long = factor(unlist(party_names)[party],
                                   levels = party_order),
               pc = pc * 100)
    return(out)
}

polls = import_data(
    url='https://s3-eu-west-1.amazonaws.com/sixfifty/polls.csv',
    party_cols=7:12,
    party_order=c("Conservatives",
                  "Labour",
                  "Liberal Democrats",
                  "UKIP",
                  "Green",
                  "SNP"
                  )
    )

polls_scotland = import_data(
    url='https://s3-eu-west-1.amazonaws.com/sixfifty/polls_scotland.csv',
    party_cols=7:13,
    party_order=c("SNP",
                  "Conservatives",
                  "Labour",
                  "Liberal Democrats",
                  "UKIP",
                  "Green",
                  "Other")
    )

polls_london = import_data(
    url='https://s3-eu-west-1.amazonaws.com/sixfifty/polls_london.csv',
    party_cols=7:13,
    party_order=c("Conservatives",
                  "Labour",
                  "Liberal Democrats",
                  "UKIP",
                  "Green",
                  "BNP",
                  "Respect",
                  "Independent",
                  "Other")
    )

polls_wales = import_data(
    url='https://s3-eu-west-1.amazonaws.com/sixfifty/polls_wales.csv',
    party_cols=7:13,
    party_order=c("Plaid Cymru",
                  "Conservatives",
                  "Labour",
                  "Liberal Democrats",
                  "UKIP",
                  "Green",
                  "Other")
    )

polls_ni = import_data(
    url='https://s3-eu-west-1.amazonaws.com/sixfifty/polls_ni.csv',
    party_cols=8:19,
    party_order=c(
        "Democratic Unionist Party",
        "Sinn Féin",
        "Ulster Unionist Party",
        "Social Democratic and Labour Party",
        "Alliance Party of Northern Ireland",
        "Green",
        "People Before Profit Alliance",
        "Traditional Unionist Voice",
        "Progressive Unionist Party",
        "UKIP",
        "Conservatives",
        "Other")
)

# Visualise as scatterplot with LOESS smoothed averages
polls_plot = polls %>%
    filter(to >= ymd('2017-01-01')) %>%
    ggplot(aes(x=to, y=pc, colour=party_long, weight=sample_size)) +
        xlab("") + ylab("Percent") +
        geom_point(size=3, alpha=0.3, pch=16) +
        scale_colour_manual("Voting Intention", values=party_colours_uk) +
        geom_smooth(method="loess", span=0.4, se=FALSE, size=1.2) +
        theme(text = element_text(size=18, family="Open Sans"),
              legend.key.height=unit(2.8, "line"),
              plot.margin = unit(c(0,0,0,0), "cm")) +
    guides(size=FALSE)
ggsave("polls-1200x600.png", plot=polls_plot, width=12, height=6, dpi=100)

poll_tracker = function(input_data, filter_date, span, party_colours_geo, filename) {
    out = input_data %>%
        filter(to >= ymd(filter_date)) %>%
        ggplot(aes(x=to, y=pc, colour=party_long, size=sample_size, weight=sample_size)) +
        xlab("") + ylab("Voting Intention") +
        geom_point(alpha=0.4, pch=16) +
        scale_colour_manual("Party", values=party_colours_geo) +
        geom_smooth(method="loess", span=span, se=FALSE, size=1.2) +
        theme(text = element_text(size=15, family="Open Sans"),
              legend.key.height=unit(2.8, "line"),
              plot.margin = unit(c(0,0,0,0), "cm")) +
        guides(size=FALSE)
    ggsave(filename, plot=out, width=12, height=6, dpi=100)
    return(out)
}

polls_plot_scotland = polls_scotland %>%
    poll_tracker(
        filter_date='2017-04-01',
        span=0.5,
        party_colours_geo=party_colours_scotland,
        filename='polls-scotland.png'
    )

polls_plot_london = polls_london %>%
    poll_tracker(
        filter_date='2017-04-01',
        span=0.5,
        party_colours_geo=party_colours_london,
        filename='polls-london.png'
    )

polls_plot_wales = polls_wales %>%
    poll_tracker(
        filter_date='2017-04-01',
        span=0.5,
        party_colours_geo=party_colours_wales,
        filename='polls-wales.png'
    )

polls_plot_ni = polls_ni %>%
    filter(to >= ymd('2017-01-01')) %>%
    filter(sample_size >= 100) %>%
    ggplot(aes(x=to, y=pc, colour=party_long, weight=sample_size)) +
    xlab("") + ylab("Voting Intention") +
    geom_point(size=5, alpha=0.4, pch=16) +
    scale_colour_manual("Party", values=party_colours_ni) +
    geom_smooth(method="loess", span=1, se=FALSE, size=1.2) +
    theme(text = element_text(size=15, family="Open Sans"),
          legend.key.height=unit(1.8, "line"),
          plot.margin = unit(c(0,0,0,0), "cm")) +
    guides(size=FALSE)
ggsave("polls-ni.png", plot=polls_plot_ni, width=12, height=6, dpi=100)


# Upload to S3
for (f in c(
    "polls-1200x600.png",
    "polls-scotland.png",
    "polls-london.png",
    "polls-wales.png",
    "polls-ni.png"
    )) {
    system_command = paste(c(
        "aws s3api put-object --bucket sixfifty --key '", f, "' ",
        "--body '", f, "' --acl 'public-read' --content-type 'image/png'"
        ), sep="", collapse="")
    system(system_command)
}
