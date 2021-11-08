extrafont::loadfonts(device = "win")
#font_import(paths = "C:/Users/HP/AppData/Local/Microsoft/Windows/Fonts")
library(tidyverse)
library(ggsoccer)
library(grid)
library(viridis)
library(cowplot)
library(Cairo)
library(splancs)
library(jpeg)
library(ggrepel)
library(ggtext)

font = "Raleway"

field = 1
minute.of.sub <- 67
maxNodeSize <- 18
node_pos = "origin"
pass_dir = T
minPass = 5
maxEdgeSize = 2
Flipx = F
convex = T
edgeAlpha = 0.7
nodeFill = "#034694"
label = TRUE
labelSize = 2.5

df <- read.csv("Chelsea vs Leicester Final.csv")

head(df)

match.data <- df %>% select(minute, second, x, y, teamId,type.displayName, outcomeType.value, outcomeType.displayName, playerId,
                            isTouch, endX, endY, blockedX, blockedY, isShot, qualifiers.4.type.displayName)

allpass <- match.data %>% filter(type.displayName == "Pass", teamId == 15, minute < minute.of.sub) %>%
  filter(!qualifiers.4.type.displayName %in% c("CornerTaken", "ThrowIn", "FreekickTaken"))

pass.data <- match.data %>% group_by(teamId) %>% 
  mutate(pass.receiver = ifelse(outcomeType.displayName == "Successful" & type.displayName == "Pass", lead(playerId), NA)) %>%
  filter(type.displayName == "Pass", teamId == 15, minute < minute.of.sub, outcomeType.displayName == "Successful") %>%
  ungroup(teamId) %>% filter(!qualifiers.4.type.displayName %in% c("CornerTaken", "ThrowIn", "FreekickTaken")) %>%
  mutate(x = x*1.2, y = y*0.8, endX = endX*1.2, endY = endY*0.8)

pass.data <- pass.data %>% mutate(diffx = x - endX, diffy = y - endY)

pass.data <- mutate(pass.data, distance = sqrt(diffx^2 + diffy^2), 
                    angle = atan2(diffy, diffx))

if(node_pos == "both"){
  nodes <- pass.data %>%
    group_by(playerId) %>%
    dplyr::summarise(xini = median(x, na.rm=T), yini = median(y, na.rm=T),
                     xend = median(endX, na.rm=T), yend = median(endY, na.rm=T),
                     num_pass = n()) %>%
    mutate(x = (xini + xend)/2, y = (yini + yend)/2) %>%
    na.omit() %>%
    mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
  
  location_text <- "Location: origin & end of passes"
}

if(node_pos == "origin"){
  nodes <- pass.data %>%
    group_by(playerId) %>%
    dplyr::summarise(x = median(x, na.rm=T), y = median(y, na.rm=T), num_pass = n()) %>%
    na.omit() %>%
    mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
  
  location_text <- "Location: origin of passes"
}

nodes <- nodes %>% mutate(playerName =
                            case_when(playerId == 25931 ~ "César Azpilicueta", 
                                      playerId == 130903 ~ "Timo Werner",
                                      playerId == 104010 ~ "Antonio Rüdiger",
                                      playerId == 113880 ~ "Kepa Arrizabalaga",
                                      playerId == 106968 ~ "Jorginho",
                                      playerId == 343346 ~ "Mason Mount",
                                      playerId == 115868 ~ "Hakim Ziyech",
                                      playerId == 84008 ~ "Marcos Alonso",
                                      playerId == 361330 ~ "Reece James",
                                      playerId == 114075 ~ "N'Golo Kanté",
                                      playerId == 28550 ~ "Thiago Silva"))

## edges based only on completed passes [pass_dir = T means splitting by direction and ploting arrows]
if(pass_dir){
  
  segmentsDf <- function(data, shorten.start, shorten.end, offset){
    
    data$dx = data$xend - data$x
    data$dy = data$yend - data$y
    data$dist = sqrt( data$dx^2 + data$dy^2 )
    data$px = data$dx/data$dist
    data$py = data$dy/data$dist
    
    data$x = data$x + data$px * shorten.start
    data$y = data$y + data$py * shorten.start
    data$xend = data$xend - data$px * shorten.end
    data$yend = data$yend - data$py * shorten.end
    data$x = data$x - data$py * offset
    data$xend = data$xend - data$py * offset
    data$y = data$y + data$px * offset
    data$yend = data$yend + data$px * offset
    
    return(data)
  }
  
  edgelist <- pass.data %>%
    select(from = playerId, to = pass.receiver) %>% 
    group_by(from, to) %>% 
    dplyr::summarise(pass_value = n()) %>% 
    na.omit()
  
  edges <- edgelist %>%
    left_join(nodes %>% select(playerId, x, y), by = c("from" = "playerId")) %>%
    left_join(nodes %>% select(playerId, xend = x, yend = y), by = c("to" = "playerId")) %>%
    segmentsDf(3, 3, 0.6)     
  
  arrow <- arrow(type = "open", angle = 30, length = unit(0.1, "inches"))
  
}else{
  edgelist <- pass.data %>%
    select(from = playerId, to = pass.receiver) %>% 
    mutate(pairs = paste(pmin(from, to), pmax(from, to), sep = "-")) %>%
    group_by(pairs) %>% 
    summarise(pass_value = n()) %>%
    mutate(name1 = sub("-.*", "", pairs),
           name2 = sub(".*-", "", pairs))
  
  edges <- edgelist %>%
    left_join(nodes %>% select(playerId, x, y), by = c("name1" = "playerId")) %>%
    left_join(nodes %>% select(playerId, xend = x, yend = y), by = c("name2" = "playerId"))
  
  arrow <- NULL
}

edges <- edges %>% 
  filter(pass_value >= minPass) %>%
  ungroup() %>%
  mutate(size = scales::rescale(pass_value, c(0.01, maxEdgeSize), c(min(pass_value), max(pass_value))))

if(convex){
  # convex hull
  if(Flipx == T){
    xown = 105
    xopp = 0
    xGK = max(nodes$x)
  }else{
    xown = 0
    xopp = 105
    xGK = min(nodes$x)
  }
  
  hull_data <- nodes %>%
    filter(x != xGK) %>%  # removing GK
    dplyr::select(x, y) %>%
    slice(chull(x, y))
  
  hull_center <- hull_data %>%
    summarise(xmean = mean(x), ymean = mean(y))
  
  dist_own_goal <- sqrt( (hull_center$xmean - xown)^2 + (hull_center$ymean - 34)^2 )
  dist_opp_goal <- sqrt( (hull_center$xmean - xopp)^2 + (hull_center$ymean - 34)^2 )
  amplitude <- max(hull_data$y) - min(hull_data$y)
  depth <- max(hull_data$x) - min(hull_data$x)
  occupied_area <- areapl(cbind(hull_data$x, hull_data$y))
  
  xlabel <- 104
  ylabel <- 64
  hjust <- 1
}else{
  xlabel <- 1
  ylabel <- 1
  hjust <- 0
}

if(field == 3){
  colText = "black"
  colConvex = "darkblue"
} else{
  colText = "#9BAAAA"
  colConvex = "black"
}

### Stats
## Passing stats 
pass_n <- nrow(allpass)
pass_pc <- nrow(pass.data)/nrow(allpass) * 100
pass_length_m <- median(pass.data$distance, na.rm = T)

xgcolors <- c("#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960","#FCDC5F", "#F5B94D",
              "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000")

p <-   ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb,
                 colour = "#2D4B4B",
                 fill = "#011515") +
  direction_label() +
  
  labs(title = "<span style = 'font-size:12pt'><span style = 'color: #034694;'><b>Chelsea FC's (H)<b></span> Passing Network vs 
       <span style = 'color: #680C1B;'><b>Leicester City FC (A)<b></span></span>
       <span style = 'font-size:9pt'><br>Match Score: 0-1 - Data until first substitution: 1 - 67'</span>",
       x = "", y = "", caption = "Twitter: @atom2r") +
  
  theme_bw() +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#011515", colour = "#011515"),
        panel.background = element_rect(fill = "#011515", colour = "#011515"),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 14, hjust = 0.1, family = font,
                                            colour = "#CBCBCB", padding = margin(1, 1, -15, 48)),
        plot.subtitle = element_text(size = 10, face = "italic", family = font),
        plot.caption = element_text(size = 8, vjust = 7, family = font, colour = "#CBCBCB", hjust = 0.9),
        plot.margin = unit(c(0.6, 0.8, 0.4, 0.2), "cm"))

if (convex == T) {
  p <- p + geom_polygon(data = hull_data, aes(x=x, y=y), col = colConvex, fill = NA, alpha = 0.1, size = 0.5) +
    geom_point(data = hull_center, aes(x=xmean, y=ymean), pch = 21, col = "white",
               fill = "black", stroke = 0.8, size = 3) +
    annotate("text", 0.5, 70, label = paste0("Total Passes: ", round(pass_n, 1),
                                             "\nPasses Completed: ", round(pass_pc, 1)," %",
                                             "\nMedian Pass Length: ", round(pass_length_m, 1), " m"),
             hjust = 0, vjust = 0, size = 3, colour = "#CBCBCB", family = font)
}

p <- p +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend, colour = size), 
               arrow = arrow, alpha = 0.7) +
  
  scale_colour_gradientn(colours = xgcolors, limits = c(0, 2),name = "Pass frequency - low to high") +
  
  geom_point(data = nodes, aes(x = x, y = y, size = size), 
             pch = 21, fill = nodeFill, col = "white", alpha = 0.7, stroke = 0.8) +
  guides(size = FALSE) +
  theme(legend.position = c(0.8, 0.16),
        legend.direction = "horizontal",
        legend.background = element_rect(colour = "transparent", fill = "transparent"),
        legend.key = element_rect(colour = "#CBCBCB", fill = "transparent"),
        legend.text = element_blank(),
        legend.title = element_text(colour = "#CBCBCB", family = font, size = 9)) +
  guides(colour = guide_legend(title.position = "top"))

if(label) {
  p <- p +
    geom_label_repel(data = nodes, aes(x, y, label=playerName), #point.padding = 0.1,
                     size = labelSize, col="black", fill="white", alpha=0.7,
                     min.segment.length = 1, family = font) 
}

p

ggsave("Pass-network.png", device = "png", type = "cairo", bg = "#011515")