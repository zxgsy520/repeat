#!/usr/bin/env Rscript

library(ggplot2)


plot_ltr_time <- function(data, name){

    data <- read.table(data, header = T)
    outpdf <- paste(name, ".ltr_time.pdf", sep="")
    outpng <- paste(name, ".ltr_time.png", sep="")
    data$V2 <- data$species/1000000 #百万年
    data1 <- data[which(data$V2>0),]
    xmax <- max(data$V2)

    p <- ggplot(data1, aes(x=V2))
    p <- p + xlab("Time of Insertion (mya)") + ylab("Density") + theme_bw()
    #p + geom_histogram(position="dodge",binwidth=0.5) + xlim(0, 5)
    p <- p + geom_histogram(aes(y=..density..), binwidth=0.1, colour="black", fill="white")
    p <- p + geom_line(stat="density") + scale_x_continuous(limits=c(0, xmax))

    ggsave(p, file=outpdf)
    ggsave(p, file=outpng)

}


add_help <- function(args){

    if(length(args) != 2) {
        print("version: v1.2.0")
        print("function:Draw a ltr_time map .")
        print("Yating Zhu, 1404213815@qq.com")
        print("plot_ltr_time.R time.list name")
        quit()
    }

}


args <- commandArgs(trailingOnly=TRUE)

add_help(args)
plot_ltr_time(args[1], args[2])
