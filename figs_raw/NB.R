library(ggplot2)
library(data.table)
library(cowplot)

# plot_NB: L = length of x axis; N = expecte
plot_NB_dist <- function(L, E, p, a=0.05) {
    d = data.table(x = rep(seq(1,L),3),
                   y = c(dnbinom(seq(1,L), E*(1-a)*p/(1-p), p),
                         dnbinom(seq(1,L), E/2*p/(1-p),     p),
                         dnbinom(seq(1,L), E*a*p/(1-p),     p)),
                   t = c(rep("2N",L), 
                         rep("1N",L),
                         rep("0N",L)))
    ggplot(d) + 
        aes(x,y,col=t) + geom_line(size=0.8) + theme_minimal() + 
        xlab("Number of reads") + ylab("NB density") + 
        ggtitle(paste("Expected =", N, "; p =", p)) + 
        theme(legend.position = "bottom") + 
        scale_color_discrete(name = NULL)
}

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}


# joint probability tables
set_up_joint_probs <- function(xlim, E, p, w_reads, c_reads, a=0.05) {
    data.table(name = c("WC", "WW", "CC", "W", "C"),
               read_W   = rep(w_reads,5),
               read_C   = rep(c_reads,5),
               read_end = rep(xlim,5),
               prob_W = c(dnbinom(w_reads, E/2*p/(1-p),p),
                          dnbinom(w_reads, E*(1-a)*p/(1-p),p),
                          dnbinom(w_reads, E*a*p/(1-p),p),
                          dnbinom(w_reads, E/2*p/(1-p),p),
                          dnbinom(w_reads, E*a*p/(1-p),p)),
               prob_C = c(dnbinom(c_reads, E/2*p/(1-p),p),
                          dnbinom(c_reads, E*a*p/(1-p),p),
                          dnbinom(c_reads, E*(1-a)*p/(1-p),p),
                          dnbinom(c_reads, E*a*p/(1-p),p),
                          dnbinom(c_reads, E/2*p/(1-p),p)))
}



plot_total_plot <- function(E, p, w_reads, c_reads, xlim, ylim, a=0.05) {
    
    # Get legend of NB dist plots
    nb_w = plot_NB_dist(xlim, E, p, a)
    nb_legend = g_legend(nb_w)
    
    # Prepare table of joint probabilities
    e = set_up_joint_probs(xlim, E, p, w_reads, c_reads, a)
    
    # Make plots with NB distributions for x and y axis
    nb_w = plot_NB_dist(xlim, E, p, a) +
        ggtitle(NULL) + 
        theme(legend.position="none") + 
        geom_vline(xintercept = w_reads, color = "darkgrey") +
        xlab(NULL) + 
        ylab("NB density (W strand)") +
        coord_cartesian(ylim = c(0,ylim)) + 
        geom_segment(data = e, aes(x = read_W, xend = read_end, y = prob_W, yend = prob_W), inherit.aes = F, linetype = "dashed", col = "darkgrey")
    nb_c = plot_NB_dist(xlim, E, p, a) + 
        ggtitle(NULL) + 
        theme(legend.position="none", axis.text.y = element_blank()) + 
        geom_vline(xintercept = c_reads, color = "darkgrey") + 
        xlab(NULL) + 
        ylab("NB density (C strand)") + 
        coord_flip(ylim = c(0,ylim)) +
        geom_segment(data = e, aes(x = read_C, xend = read_end, y = prob_C, yend = prob_C), inherit.aes = F, linetype = "dashed", col = "darkgrey")
    
    # Plot of joint probabilities
    nb_prod = ggplot(e) + 
        aes(xmin = 0, xmax = prob_C, ymin = 0, ymax = prob_W, fill = name) + 
        geom_point(size = 1, aes(x=prob_C, y=prob_W, col=name))
    nb_prod_leg = g_legend(nb_prod)
    nb_prod = nb_prod + 
        geom_rect(alpha=0.33) +
        coord_cartesian(ylim = c(0,ylim), xlim = c(0,ylim)) +
        theme_minimal() + 
        xlab(NULL) + ylab(NULL) +
        theme(legend.position = "none", axis.text.y = element_blank()) + 
        geom_point(aes(x=prob_C, y=prob_W, col=name)) #+ 
        #geom_text(aes(x=prob_C, y=prob_W, label = paste0(round(prob_C*prob_W*100,4),"%")), 
        #          hjust=0, vjust=0, position = position_nudge(x=ylim/100,y=ylim/100))
        
    
    plot_grid(nb_w,nb_prod,NULL,nb_c) + 
        draw_grob(nb_legend,x=0,y=0.4,width=0.5,height=0.1) +
        draw_grob(nb_prod_leg,width=0.5,height=0.3)
    
}


p1 = plot_total_plot(20, 0.3, 12, 8, 30, 0.095, a=0.075)
p2 = plot_total_plot(20, 0.3, 16, 4, 30, 0.095, a=0.075)

dev.off()
pdf("NB_example_12_8.pdf", width = 6, height = 6)
p1
dev.off()
pdf("NB_example_16_4.pdf", width = 6, height = 6)
p2
dev.off()


