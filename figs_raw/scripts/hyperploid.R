library(data.table)
library(ggplot2)

# Read in two different resolutions
#d = fread(paste("/usr/local/bin/zcat","/Users/meiers/work/data/strandseq/RPE_WT/BM160815_WT.200000.txt.gz"))
d = fread(paste("/usr/local/bin/zcat", "/Users/meiers/work/data/strandseq/RPE_C29/BM160815_C29.200000.txt.gz"))
d = d[grepl('^([1-9]|[12][0-9]|X|Y)$', chrom),]
d = d[, chrom := factor(chrom, levels=as.character(c(1:22,'X','Y')), ordered = T)][]
d <- merge(d, d[,.(total = sum(w+c)), by = cell], by = "cell")
d <- merge(d, d[,.(mean = median(w+c)), by = cell], by = "cell")
d <- d[total > 2e5,]

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
        scale_x_continuous(breaks = pretty_breaks(5), labels = format_Mb) +
        scale_y_continuous(breaks = pretty_breaks(3)) +
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


selection = c("BM160815_C29_075p1", "BM160815_C29_049p1", "BM160815_C29_033p1", "BM160815_C29_026p1", "BM160815_C29_060p1")
e = d[chrom==20 & cell %in% selection, ]
e$cell = factor(e$cell, levels = selection, ordered = T)
p = plot_region(e) +
    ggtitle("Tetraploid chromosome 20") +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.tetraploid.chr20.pdf", width=6, height=5)


p = ggplot(d[chrom == 20 & w+c>20,]) + aes(c/(w+c)) +
    geom_histogram(binwidth=0.01, fill = "dodgerblue3", col = "dodgerblue3", alpha=0.4) +
    #geom_density(adjust=0.3, fill = "dodgerblue3", col = "dodgerblue3", alpha=0.4) +
    theme_classic() +
    ggtitle("Watson/Crick fraction on tetraploid chr20") +
    xlab("Crick fraction")
ggsave(p, filename = "tetraploid.WCratios.pdf", width=4, height=3)
