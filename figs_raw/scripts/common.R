format_Mb <- function(x) {
    paste(comma(x/1e6), "Mb")
}

plot_region <- function(d, lines = NULL, scale_fac = 1.4) {
    y_lim = max(d[,quantile(w+c, 0.9), by = cell]$V1)
    bar_width = median(d$end - d$start)
    plt <- ggplot(d) +
        aes(x = (start+end)/2) +
        geom_bar(aes(y = -w), width = bar_width, stat='identity', position = 'identity', fill='sandybrown') +
        geom_bar(aes(y = c), width = bar_width, stat='identity', position = 'identity', fill='paleturquoise4') +
        # Trim image to 2*median cov
        coord_flip(expand = F, ylim=c(-scale_fac*y_lim, scale_fac*y_lim)) +
        facet_grid(.~cell, scale = "free_x") +
        scale_x_continuous(breaks = pretty_breaks(10), labels = format_Mb) +
        scale_y_continuous(breaks = pretty_breaks(5)) + 
        theme_bw() +
        theme(panel.spacing = unit(0.5, "lines"),
              #axis.text.x = element_blank(),
              #axis.ticks.x = element_blank(),
              strip.placement = 'outside',
              strip.background = element_rect(fill = NA, colour=NA)) + 
        guides(fill = FALSE)
    if (!is.null(lines)) {
        plt <- plt + geom_vline(data = lines, aes(xintercept = x), col = "dodgerblue", linetype = "dashed")
    }
    plt
}
