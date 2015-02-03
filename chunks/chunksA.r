## @knitr load.libraries --------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

## @knitr load.data -------------------------------------------------
load('data/books_politics.rda')



## @knitr make.simple.summaries -------------------------------------

n.books <- n_distinct(books.politics$productID)

n.subcat2 <- books.politics %>%
  group_by(Sub_Cat2) %>%
  summarize(n=n(),
            n.books=n_distinct(productID),
            rev.book = n/n.books) %>%
  arrange(desc(n)) 








book.stats <- books.politics %>%
  group_by(productID) %>%
  summarize(title=title[1],
            avg.score=mean(score),
            Sub_Cat1=Sub_Cat1[1],
            Sub_Cat2=Sub_Cat2[1],
            Sub_Cat3=Sub_Cat3[1],
            Sub_Cat4=Sub_Cat4[1],
            Sub_Cat5=Sub_Cat5[1],
            Sub_Cat6=Sub_Cat6[1],
            NumReviews=n())
            

plot.total.number.reviews.time <- books.politics %>%
  group_by(month.year) %>%
  summarize('Number of Reviews'=n(),
            'Average Score'=mean(score)) %>%
  gather(metric,value,-month.year) %>%
  ggplot(aes(x=month.year,y=value,color=metric,group=metric)) + 
    geom_line() + 
    scale_x_discrete(breaks=paste0('Jan ',c(1996:2013)))+theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none")+
    facet_wrap(~metric,scales='free')









