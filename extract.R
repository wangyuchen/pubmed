# orgnize all data about the author
source('functions.R')
author <- "Wu,Yilong"
root <- search_author(author)
personal.info <- extract_author_info(root)


# orgnize coauthors
coauthors <- unlist(personal.info$al)
freq <- integer()
for (i in 1:length(unique(coauthors))) {
  freq[i] <- length(which(unique(coauthors)[i] == coauthors))
}
coauthors <- unique(coauthors)

# publication role
role <- factor(levels=c("Solo Authorships", "First Authorships", 
                   "Last Authorships", "Other Authorships"))
role[personal.info$sa] <- "Solo Authorships"
role[personal.info$fa == author] <- "First Authorships"
role[personal.info$la == author] <- "Last Authorships"
role[is.na(role)] <- "Other Authorships"

info <- as.data.frame(cbind(personal.info$at, personal.info$jt, 
                            personal.info$year))
names(info) <- c("at","jt", "year")

library(ggplot2)
pie <- ggplot(data.frame(), 
              aes(x = factor(1), fill = role)) + geom_bar(width = 1)
pie + coord_polar(theta = "y")

hist_cut <- ggplot(info, aes(x=year, fill=role))
hist_cut + geom_bar()



# find coauthor positions
positions[3] <- t(sapply(coauthors[freq>1][3], locate_coauthor))


link <- as.data.frame(cbind(coauthors[freq>1], positions, freq[freq>1]), 
                      stringsAsFactors=F)

link$V3 <- as.numeric(link$V3)
link$V2 <- as.numeric(link$V2)
link$V4 <- as.numeric(link$V4)
names(link) <- c("name", "lon", "lat", "freq")

library(maps)
world<-map_data('world')

p <- ggplot(legend=FALSE) +
  geom_polygon(data=world, aes(x=long, y=lat,group=group), fill="grey") +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  xlab("") + ylab("")


line <- data.frame()
for (i in 2:nrow(link)) {
  line <- link[c(1,i), 2:3]
  p <- p + geom_line(data=line, aes(lon,lat), size=0.5)
}
p + geom_point(data=link, 
             aes(lon,lat,shape=factor(freq), color=factor(freq)), 
             size=5) 

