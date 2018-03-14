library(data.table)
library(ggExtra)
library(ggplot2)
library(scales)
source("common.R")

d = fread(paste("/usr/local/bin/zcat","/Users/meiers/work/data/strandseq/RPE_WT/BM160815_WT.200000.txt.gz"))
d = d[, chrom := factor(chrom, levels=as.character(c(1:22,'X','Y')), ordered = T)][]
d <- merge(d, d[,.(total = sum(w+c)), by = cell], by = "cell")
d <- d[total > 2e5,]
d <- merge(d, d[,.(median_count = median(w+c)), by = cell], by = "cell")




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
        scale_x_continuous(breaks = pretty_breaks(6), labels = format_Mb) +
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



### TRA example
p <- plot_region(d[chrom == "10" & cell %in% c("BM160815_WT_007p1", "BM160815_WT_094p1","BM160815_WT_037p1"),],
                 lines = data.table(x = 61e6),
                 scale_fac = 1.2) +
    ggtitle("Translocation of chr10 q-arm") +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.TRA.chr10.pdf", width=4, height=5)
p <- plot_region(d[chrom == "X" & cell %in% c("BM160815_WT_007p1", "BM160815_WT_094p1","BM160815_WT_037p1"),],
                 scale_fac = 1.2) +
    ggtitle("Chromosome X") +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.TRA.chrX.pdf", width=4, height=5)



lr = merge(d[start>=40e6 & end<=60e6 & chrom=="10", .(w_left = median(w/median_count), c_left = median(c/median_count), frac_left = median(c/(c+w))), by = cell],
           d[start>=62e6 & end<=82e6 & chrom=="10", .(w_right = median(w/median_count), c_right = median(c/median_count), frac_right = median(c/(c+w))), by = cell],
           by = "cell")
lr <- lr[, `X state q-arm` := d[total > 1e5 & start > 130e6 & chrom == "X", .(X_state = names(table(class))[which.max(table(class))]), by = cell]$X_state ]
lr <- lr[, `X state p-arm` := d[total > 1e5 & start < 25e6 & chrom == "X", .(X_state = names(table(class))[which.max(table(class))]), by = cell]$X_state ]
lr <- lr[, `chromosome state` := ifelse(c_left/(c_left+w_left) < 0.2, "CC", ifelse(w_left/(c_left+w_left)<0.2, "WW", "WC")) ][]

p1 = ggplot(lr) +
    aes(frac_left, frac_right, col = `X state p-arm`) +
    geom_point(alpha = 0.5) +
    xlab("Crick fraction chr10:40-60Mb") +
    ylab("Crick fraction chr10:62-82Mb") +
    theme_classic() +
    theme(legend.position = "bottom") +
    ggtitle("Strand state around translocation")
p1_ = ggMarginal(p1, type = "histogram", binwidth = 0.02, fill = "dodgerblue3", col = "dodgerblue3", alpha=0.3)
ggsave(p1_, filename = "rpe_translocation.local_state_change.left_arm.pdf", width=4,height=4)

p1 = ggplot(lr) +
    aes(c_left/(c_left+w_left), c_right/(c_right+w_right), col = `X state q-arm`) +
    geom_point(alpha = 0.5) +
    xlab("Crick fraction chr10:40-60Mb") +
    ylab("Crick fraction chr10:62-82Mb") +
    theme_classic() +
    theme(legend.position = "bottom") +
    ggtitle("Strand state around translocation")
p1_ = ggMarginal(p1, type = "histogram", binwidth = 0.02, fill = "dodgerblue3", col = "dodgerblue3", alpha=0.3)
ggsave(p1_, filename = "rpe_translocation.local_state_change.right_arm.pdf", width=4,height=4)

