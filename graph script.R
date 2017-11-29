shots<-read.csv("data.csv")
shots2<-shots %>%
  mutate(shot_made_flag=as.numeric(shot_made_flag), 
         action_type=as.numeric(action_type), combined_shot_type=as.numeric(combined_shot_type),
         playoffs=as.numeric(playoffs), shot_type=as.numeric(shot_type),
         shot_zone_area=as.numeric(shot_zone_area), shot_zone_basic=as.numeric(shot_zone_basic),
         shot_zone_range=as.numeric(shot_zone_range), period=as.numeric(period))
test<-subset(shots2, is.na(shot_made_flag))
train<-subset(shots2, !is.na(shot_made_flag))



zones<-train2 %>%
  group_by(shot_zone_basic, shot_zone_area, shot_zone_range) %>%
  summarise(shots_made=sum(shot_made_flag), mlocx=mean(loc_x), mlocy=mean(loc_y), tot=n())

# calculate shot zone accuracy and add zone accuracy labels
zones$accuracy <- (zones$shots_made / zones$tot)
zones$lab <- paste(as.character(round(100 * zones$accuracy, 1)), "%", sep="")

# plot shot accuracy per zone
ggplot(zones, aes(x=mlocx, y=mlocy)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = accuracy, size = tot, alpha = 0.8)) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) 



  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size = 12),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "thedatagame.com.au", just = "centre", vjust = 50)


t<-hexbin(train2$loc_x, train2$loc_y, IDs=TRUE)


hbin<-hexbin(train2$loc_x, train2$loc_y,xbins=40,IDs=TRUE)
good<-hexTapply(hbin,train2$shot_made_flag,mean,na.rm=TRUE)

train3<-mutate(train2, bin_ID=good@cID)




library(densityvis)
train2<-subset(train2, center_y<350 & abs(center_x)<250)
closest<-hex_pos(train2$loc_x, train2$loc_y, 70,70)
train2$center_x=closest[,1]
train2$center_y=closest[,2]

train3<-train2 %>%
  group_by(center_x, center_y) %>%
  summarise(makes=sum(shot_made_flag), tot=n()) %>%
  mutate(Accuracy=100*makes/tot) 


ggplot(train3, aes(center_x,center_y, color=Accuracy)) +
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(size=tot)) + 
  scale_color_gradient(low="blue", high="red") + 
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  xlab("") + ylab("")
  


