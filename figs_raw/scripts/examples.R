library(data.table)
library(ggplot2)
library(scales)
source("common.R")

# Read in two different resolutions
D = fread(paste("/usr/local/bin/zcat","/Users/meiers/work/data/strandseq/RPE_WT/BM160815_WT.200000.txt.gz"))
D = D[grepl('^([1-9]|[12][0-9]|X|Y)$', chrom),]
D = D[, chrom := factor(chrom, levels=as.character(c(1:22,'X','Y')), ordered = T)][]
D <- merge(D, D[,.(total = sum(w+c)), by = cell], by = "cell")
D <- merge(D, D[,.(mean = median(w+c)), by = cell], by = "cell")
D <- D[total > 1e5,]

b = fread(paste("/usr/local/bin/zcat","/Users/meiers/work/data/strandseq/RPE_WT/BM160815_WT.100000.txt.gz"))
b = b[grepl('^([1-9]|[12][0-9]|X|Y)$', chrom),]
b = b[, chrom := factor(chrom, levels=as.character(c(1:22,'X','Y')), ordered = T)][]
b <- merge(b, b[,.(total = sum(w+c)), by = cell], by = "cell")
b <- merge(b, b[,.(mean = median(w+c)), by = cell], by = "cell")
b <- b[total > 1e5,]







### Het INV
x = b[chrom == "17" & start >= 10e6 & end <= 20e6 & cell %in% c("BM160815_WT_005p1","BM160815_WT_010p1","BM160815_WT_007p1"),]
x[, cell := factor(cell, levels = c("BM160815_WT_005p1","BM160815_WT_010p1","BM160815_WT_007p1"), ordered = T)]
p = plot_region(x, lines = data.table(x = c(16.8e6,18.4e6))) +
    ggtitle("Heterozygous INV on chr17")  +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.het_INV.chr17.pdf", width=4, height=5)



### Hom INV
x = b[chrom == "16" & start >= 17e6 & end <= 27e6 & cell %in% c("BM160815_WT_058p1", "BM160815_WT_005p1", "BM160815_WT_007p1"),]
x[, cell := factor(cell, c("BM160815_WT_058p1", "BM160815_WT_005p1", "BM160815_WT_007p1"), ordered = T)]
p = plot_region(x,
            lines = data.table(x = c(21.4e6,22.6e6))) + # "BM160815_WT_010", "BM160815_WT_011"
    ggtitle("Homozygous INV on chr16") +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.hom_INV.chr16.pdf", width=4, height=5)



### DUP example
x = b[chrom == "3" & start >= 55e6 & end <= 65e6 & total > 7e5 & cell %in% c("BM160815_WT_008p1", "BM160815_WT_011p1", "BM160815_WT_010p1"),]
x[, cell := factor(cell, levels = c("BM160815_WT_008p1", "BM160815_WT_011p1", "BM160815_WT_010p1"), ordered = T)]
p = plot_region(x, data.table(x=c(60.8e6,62.4e6))) +
    ggtitle("Heterozygous duplication on chr3") +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.het_DUP.chr3.pdf", width=4, height=5)




### inv DUP
#plot_region(b[chrom == "19" & start > 20e6 & end < 30e6 & cell %in% c("BM160815_WT_010", "BM160815_WT_011", "BM160815_WT_007", "BM160815_WT_005", "BM160815_WT_058"),],
#            lines = data.table(x = c(24.3e6,24.4e6))) +
#    ggtitle("Potential inverted duplication on chr16")
#plot_region(b[chrom == "6" & end < 10e6 & total > 7e5,],
#            lines = data.table(x = c(0.2e6,0.4e6))) +
#    ggtitle("Potential inverted duplication on chr6 - there is some weird heterogeneity (artefact or biology?)")
x = b[chrom == "6" & end <= 10e6 & cell %in% c("BM160815_WT_032p1", "BM160815_WT_010p1", "BM160815_WT_084p1"),]
x[, cell := factor(cell, levels = c("BM160815_WT_032p1", "BM160815_WT_010p1", "BM160815_WT_084p1"), ordered = T)]
p = plot_region(x, # "BM160815_WT_010"
            lines = data.table(x = c(0.2e6,0.4e6))) +
    ggtitle("Potential inverted duplication on chr6")  +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.inv_DUP.chr6.pdf", width=4, height=5)



## SCEs
p = plot_region(d[chrom == "4" & cell %in% c("BM160815_WT_024","BM160815_WT_043"),],
                lines = data.table(x = 71e6)) +
    ggtitle("Sister chromatid exchange event on chr4")  +
    ylab("Binned read counts: Watson | Crick") +
    xlab("Chromosomal position")
ggsave(p, filename = "example.SCE.chr4.pdf", width=5, height=6)




