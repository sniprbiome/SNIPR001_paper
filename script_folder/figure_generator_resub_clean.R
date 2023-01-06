if(!c("ggplot2") %in% installed.packages()) {install.packages("ggplot2")}
if(!c("readxl") %in% installed.packages()) {install.packages("readxl")}
if(!c("cowplot") %in% installed.packages()) {install.packages("cowplot")}
if(!c("Cairo") %in% installed.packages()) {install.packages("Cairo")}
if(!c("reshape2") %in% installed.packages()) {install.packages("reshape2")}
if(!c("matrixStats") %in% installed.packages()) {install.packages("matrixStats")}
if(!c("ggpubr") %in% installed.packages()) {install.packages("ggpubr")}
if(!c("ggsignif") %in% installed.packages()) {install.packages("ggsignif")}
if(!c("ape") %in% installed.packages()) {install.packages("ape")}
if(!c("RColorBrewer") %in% installed.packages()) {install.packages("RColorBrewer")}
if(!c("patchwork") %in% installed.packages()) {install.packages("patchwork")}
if(!c("dplyr") %in% installed.packages()) {install.packages("dplyr")}


library(ggplot2)
library(ggtree)
library(readxl)
library(cowplot)
library(Cairo)
library(reshape2)
library(matrixStats)
library(ggpubr)
library(ggsignif)
library(ape)
library(RColorBrewer)
library(openxlsx)
library(patchwork)
library(dplyr)



#defines the folder where we would like our outputs, the folder where we have our data, the folder where we have relevant scripts, and the folder in which we have relevant graphical elements
fig_folder <- "your_directory_here/figures/"
data_folder <- "your_directory_here/data/"
art_folder <- "your_directory_here/art_assets/"
script_folder <- "your_directory_here/script/"
summary_stats_folder <- "your_directory_here/summary_stats/"




fig_folder <- "/Users/aoesniprbiome.com/Library/CloudStorage/OneDrive-SniprBiome/projects/SNIPR001 paper/paper_resub_clean/figure_folder/"
data_folder <- "/Users/aoesniprbiome.com/Library/CloudStorage/OneDrive-SniprBiome/projects/SNIPR001 paper/paper_resub_clean/data_folder/"
art_folder <- "/Users/aoesniprbiome.com/Library/CloudStorage/OneDrive-SniprBiome/projects/SNIPR001 paper/paper_resub_clean/art_assets/"
script_folder <- "/Users/aoesniprbiome.com/Library/CloudStorage/OneDrive-SniprBiome/projects/SNIPR001 paper/paper_resub_clean/script_folder/"
summary_stats_folder <- "/Users/aoesniprbiome.com/Library/CloudStorage/OneDrive-SniprBiome/projects/SNIPR001 paper/paper_resub_clean/summary_stat_folder/"


#we define some redcurrant colors
SNIPR_grey <- "#757575"
SNIPR_green <- '#22970F'
SNIPR_pale_green <- "#80c476"
SNIPR_pale_red <- "#f76459"
SNIPR_color1 <- "#9ca7b8"
SNIPR_color2 <- "#7a8ba3"
EcCAS_color <- "#4e80c2"

#The phages are color-coded in the paper, so we define the appropriate color codes here

phage_colors <- c("α15.2"='#2C6BAA',"α20.4"="#ff4265","α48.4"="#5EB7B4","α51.5"="#F6C03B",
                  "α15"="#9ed8d8","α17"="#EF7826",'α20'="#F7B5C4","α48"="#6CB6B6","a51"="#F2AA6D",
                  'SNIPR001'=SNIPR_green,'Control'=SNIPR_grey)
SNIPR001_phages <- c("α15.2","α20.4","α48.4","α51.5")


#we set the colors we wish to use for phylogroups
encountered_phylogroups <- c('A','B1','B2','C','Clade III','D','E','E or clade I','F','G')
phylogroup_colors <- RColorBrewer::brewer.pal(length(encountered_phylogroups),'Paired')
names(phylogroup_colors) <- encountered_phylogroups

#We se the colors we wish to use for MLSTs
MLST_colors <- c(
  '10'="#087546",
  '1193'="#45f7a4",
  '12'="#23BA2B",
  '127'="#9DD494",
  '131'="#daf745",
  '224'="#B9A3FF",
  '38'="#1F35FF",
  '410'="#3576CC",
  '46'="#61DAFF",
  '58'="#ABFFF6",
  '69'="#FFCC26",
  '73'="#FF9600",
  '88'="#CC6B3B",
  '95'="#8C3F38",
  'Other'="#ada9a8"
)


#displays the colors chosen
testdat<- as.data.frame(c(phage_colors,"SNIPR grey"=SNIPR_grey,'SNIPR green'=SNIPR_green,"SNIPR color1"=SNIPR_color1,"SNIPR color2"=SNIPR_color2))
testdat <- cbind(testdat,'phage'=rownames(testdat))
print(ggplot(data=testdat,aes(x=1,y=phage,fill=phage))+geom_tile()+scale_fill_manual(values = c(phage_colors,"SNIPR grey"=SNIPR_grey,'SNIPR green'=SNIPR_green,"SNIPR color1"=SNIPR_color1,"SNIPR color2"=SNIPR_color2)))


#We set a consistent ggplot theme in our figures called custom theme
text_size=9.5
custom_theme <- theme_bw()+#starts black white theme as starting point
  theme(axis.title.y=element_text(hjust=0.5,vjust=0.5,size=text_size),#Sets y axis title to be horizontal and appropriate size
        axis.title.x=element_text(size=text_size,angle=0),#sets x axis title
        axis.text.x = element_text(size=text_size),axis.text.y = element_text(size=text_size),#sets x axis text size
        legend.text = element_text(size=text_size),#sets legend text size
        panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank(),#removes the minor panel grid for a cleaner look
        legend.title = element_text(size=text_size),#sets legend title
        legend.key.size = unit(0.5,'cm'),#sets the sie of the legend keys to be a half centimeter
        strip.background.x=element_rect(fill='#FFFFFF'),strip.background.y = element_rect(fill="#FFFFFF"),#sets facet backgrouns to white
        strip.text.x=element_text(size=text_size),strip.text.y = element_text(size=text_size))#sets facet title square backgrounds to be white


#A function to easily set axis text to scientific notation
to_tenth_power_labels <- function(x_labels){
  return(parse(text=paste0("10^",x_labels)))
}

###########
#Figure 2A#
###########

#This summary figure has a lot of components.
#We need:
#1) a heatmap
#2) genus annotation of phage
#3) surface receptor information of phage
#4) a correlation tree for vira
#5) a ordered MLST heatmap
#6) a barplot showing the genome size of phages

#The trees are based on a clustering of the iAUC data. So first we read the iAUC data
iAUC_data_for_WT_phages_vs_abbreviated_panel <- as.data.frame(read_excel(paste0(data_folder,"iAUC_data_for_WT_phages_vs_abbreviated_panel.xlsx")))
rownames(iAUC_data_for_WT_phages_vs_abbreviated_panel) <- iAUC_data_for_WT_phages_vs_abbreviated_panel[,1]
iAUC_data_for_WT_phages_vs_abbreviated_panel <- iAUC_data_for_WT_phages_vs_abbreviated_panel[,-1]
colnames(iAUC_data_for_WT_phages_vs_abbreviated_panel) <- gsub('a','α',colnames(iAUC_data_for_WT_phages_vs_abbreviated_panel))

#we make the trees to better order future plots
bact_dendrogram <- as.dendrogram(hclust(dist(scale(iAUC_data_for_WT_phages_vs_abbreviated_panel))))
phage_dendrogram <- as.dendrogram(hclust(dist(scale(t(iAUC_data_for_WT_phages_vs_abbreviated_panel)))))

phage_tree <- ggtree(phage_dendrogram)+coord_flip()+scale_x_reverse()
bact_tree <- ggtree(bact_dendrogram)

#we get the order of bacteria and phages
bact_order <- as.vector(na.omit(bact_tree[["data"]][["label"]][order(bact_tree[["data"]][["y"]])]))
phage_order <- as.vector(na.omit(phage_tree[["data"]][["label"]][order(phage_tree[["data"]][["y"]])]))#the tree will be flipped upside down so the order will need to be reversed

#we start with the core feature of the plot - the heatmap
heatmap_data <- reshape2::melt(as.matrix(iAUC_data_for_WT_phages_vs_abbreviated_panel))
colnames(heatmap_data) <- c('bact','phage','iAUC')

#anything exceeding 1 is set to 1 to prevent going outside limits
heatmap_data[heatmap_data[,"iAUC"]>1,"iAUC"] <- 1

#first we reformat and set anything below 0.2 to NA
heatmap_data[heatmap_data[,"iAUC"]<0.2,"iAUC"] <- NA

#then we ensure the proper ordering of bacteria and phages
heatmap_data[,"bact"] <- factor(as.character(heatmap_data[,"bact"]),levels = bact_order)
heatmap_data[,"phage"] <- factor(as.character(heatmap_data[,"phage"]),levels = phage_order)

#now we make the heatmap - at least a version that has a bit more info than we need so we can check if the ordering is correct later
p_heatmap <- ggplot(data = heatmap_data,mapping = aes(x=phage,y=bact,fill=iAUC))+custom_theme+geom_tile()
p_heatmap <- p_heatmap + scale_fill_continuous(high=SNIPR_green,low='#ffffff',na.value='#ffffff',limits=c(0.2,1))

#in order for the y axis titles to line up properly on the right I have to have axis labels even though I don't want any, so we set them to a long set of spaces to make things alogn properly
p_heatmap <- p_heatmap + theme(axis.text.x=element_blank(),axis.title.x = element_blank(),axis.ticks.x=element_blank())
p_heatmap <- p_heatmap + ylab(bquote('Bacterial panel ('~italic("n")~' = '~.(as.character(length(unique(heatmap_data[["bact"]]))))~')'))+scale_y_discrete(position = 'right',labels=rep('              ',length(unique(heatmap_data[["bact"]]))))

#and we load the genome size and the genus info
CAP_genus_genomesize <- read.csv(paste0(data_folder,"list_alpha001_genus_genomesize.csv"),sep=',')
rownames(CAP_genus_genomesize) <- CAP_genus_genomesize[,"alpha"]

#we convert genome size to numeric
CAP_genus_genomesize[,"genome.size"] <- as.numeric(CAP_genus_genomesize[,"genome.size"])

#we make a barchart
CAP_genus_genomesize <- as.matrix(CAP_genus_genomesize)
barchart_plotdat <- reshape2::melt(CAP_genus_genomesize[,c("genome.size")])
barchart_plotdat <- cbind('alpha_name'=rownames(barchart_plotdat),barchart_plotdat)

#we order the alphas to fit the dendrogram
barchart_plotdat[,"alpha_name"] <- factor(as.character(barchart_plotdat[,"alpha_name"]),levels = phage_order)
barchart_plotdat[,"value"] <- as.numeric(barchart_plotdat[,"value"])
barchart_plotdat[is.na(barchart_plotdat[,"value"]),"value"] <- 0

p_barchart <- ggplot(barchart_plotdat,aes(x=alpha_name,y=value))+geom_bar(stat='identity',fill=SNIPR_color2)+custom_theme
p_barchart <- p_barchart+theme(axis.text.x=element_text(angle=90))
p_barchart <- p_barchart + scale_y_continuous(position = 'right')+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+ylab('Genome\nsize')

#now we start with the colors that are below the heatmap - these will retain their phage names (?)
genus_plotdat <- as.data.frame(CAP_genus_genomesize[,c("alpha","genus")])
genus_plotdat[,"alpha"] <- factor(genus_plotdat[,"alpha"],levels = phage_order)
genus_plotdat <- cbind(genus_plotdat,'type'=rep('Genus',nrow(genus_plotdat)))

#we make the plot
p_bottom_bar <- ggplot(genus_plotdat,mapping = aes(x=alpha,y = type,fill=genus))+custom_theme+geom_tile()+scale_fill_manual("Genus",values = RColorBrewer::brewer.pal(length(unique(genus_plotdat[,"genus"])),'Set3'))
p_bottom_bar <-  p_bottom_bar + theme(panel.border = element_blank(),axis.text.x = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y = element_blank())
p_bottom_bar <- p_bottom_bar + xlab(bquote('Phage panel ('~italic("n")~' = '~.(length(unique(genus_plotdat[["alpha"]])))~')'))+scale_y_discrete(position = 'right')



#we'll add some tip colors to indicate MLST
#we load MLSTs
SNIPR_MLST <- read.delim(paste0(data_folder,"abbreviated_panel_MLSTs.tsv"))

#we create the heatmap to indicate the MLST groups of our bacterial tree
MLST_heatmap_data <- as.data.frame(cbind('strain'=rownames(SNIPR_MLST),'MLST'=SNIPR_MLST[,"MLST_type"],'yvar'=rep('MLST',nrow(SNIPR_MLST))))

#we order the strains ti follow the same order as the bacterial tree's y-order
bact_order <- as.vector(na.omit(bact_tree[["data"]][["label"]][order(bact_tree[["data"]][["y"]])]))
MLST_heatmap_data[["strain"]] <- factor(MLST_heatmap_data[["strain"]],levels = bact_order)

#and we make the heatmap itself
bact_MLST_heatmap <- ggplot(data = MLST_heatmap_data,aes(y=strain,x=yvar,fill=MLST))+geom_tile()+scale_fill_manual(values = MLST_colors)+custom_theme
bact_MLST_heatmap <- bact_MLST_heatmap+theme(axis.text.y=element_blank(),axis.ticks.y = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),panel.border = element_blank())

#we wish to indicate on the phage tree that certain phages were chosen for further development
further_development <- c("α15","α17","α20","α31","α33","α46","α48","α51")

#we make a dataframe for the plot data to indicate the phages we used for further development
phage_deveopment_info <- data.frame('phage'=as.vector(na.omit(phage_tree[["data"]][["label"]][order(phage_tree[["data"]][["y"]])])))
#we add data to indicate the development status
phage_deveopment_info[['development_info']] <- rep("Not developed",nrow(phage_deveopment_info))
phage_deveopment_info[phage_deveopment_info[["phage"]]%in%further_development,"development_info"] <- 'Further developed'
phage_deveopment_info[phage_deveopment_info[["phage"]]%in%unlist(strsplit(SNIPR001_phages,'[.]'))[seq(1,2*length(SNIPR001_phages),2)],"development_info"] <- 'Developed to SNIPR001'
#and we set a y-variable for the dotplot which will be the same across the board
phage_deveopment_info[['y_var']] <- rep('Development_info',nrow(phage_deveopment_info))

#we order the phage order to match the tree
phage_deveopment_info[["phage"]] <- factor(as.character(phage_deveopment_info[["phage"]]),levels = as.vector(na.omit(phage_tree[["data"]][["label"]][order(phage_tree[["data"]][["y"]])])))

#we generate dots to indicate if something was used for further development
development_status_dots <- ggplot(phage_deveopment_info,aes(y=y_var,x=phage,color=development_info,alpha=development_info))+geom_point(size=1)
development_status_dots <- development_status_dots+scale_color_manual('Development\nstatus',values = c("Further developed"=SNIPR_color1,"Developed to SNIPR001"=SNIPR_green,"Not developed"="#ffffff"))+scale_alpha_manual(values = c("Further developed"=1,"Developed to SNIPR001"=1,"Not developed"=0))
development_status_dots <- development_status_dots+custom_theme+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank(),axis.ticks.y=element_blank(),axis.title.x = element_blank(),panel.grid = element_blank(),panel.border = element_blank())+guides(alpha=F)+ylab('Development')+scale_y_discrete(position='right')

#reads the receptor info
plot_info_receptor <- as.data.frame(read.delim(paste0(data_folder,"phage_receptor_trial_info.tsv")))

plot_info_receptor[["phage"]] <- factor(plot_info_receptor[["phage"]],levels = phage_order)

#we wish to display the receptors in a certain order
receptor_order <- rev(c('LPS','Tsx','LamB','OmpC','OmpA','TolC','OmpF','Other'))
plot_info_receptor[["receptor"]] <- factor(plot_info_receptor[["receptor"]],levels = receptor_order)


plot_info_receptor[plot_info_receptor[["phage"]]%in%c("α51","α46","α48","α31","α33"),]
write.table(plot_info_receptor,file = 'phage_receptor_trial_info.tsv',row.names = F,quote = F,col.names = T)

#and we plot the receptor info
receptor_plot <- ggplot(plot_info_receptor,mapping = aes(x=phage,y=receptor,fill=presence))+geom_tile()+custom_theme+scale_fill_manual('Phage receptor',values = c('Yes'="#000000","No"='#ffffff','Not determined'="#a1a1a1"))
receptor_plot <- receptor_plot + scale_y_discrete(position = 'right')+theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank())+ylab('Receptor\nprofile')

#now we start assembling stuff
#let's build core out
#we start by adding the barplot to the heatmap


#we extract all the legends
dev_status_legend <- get_legend(development_status_dots)
MLST_legend <- get_legend(bact_MLST_heatmap)
main_heatmap_legend <- get_legend(p_heatmap)
receptor_plot_legend <- get_legend(receptor_plot)
phage_genus_legend <- get_legend(p_bottom_bar)

#we generate the main figure
comboplot <- cowplot::plot_grid(
  plotlist = list(NULL,
                  phage_tree+theme(plot.margin = margin(r=0,l=0,0,0,unit = 'cm')),
                  NULL,
                  development_status_dots+theme(legend.position = 'none'),
                  NULL,
                  p_barchart+theme(plot.margin = margin(b=0,t=0,l=0,r=0,unit='cm')),
                  bact_MLST_heatmap+theme(legend.position = 'none',axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin = margin(0,0,0,0,'cm')),
                  p_heatmap+theme(plot.margin = margin(t=0,b=0,l=0,0,unit = 'cm'),legend.position = 'none'),
                  NULL,
                  receptor_plot+theme(plot.margin = margin(t=0,l=0,b=0,0,unit = 'cm'),legend.position = 'none'),
                  NULL,
                  p_bottom_bar+theme(axis.ticks.y=element_blank(),plot.margin = margin(t=0,l=0,0,0,unit = 'cm'),legend.position = 'none',axis.text.y.left = element_blank(),axis.text.y.right = element_text())),
  
  align = 'v',
  ncol=2,
  nrow=6,
  axis = 'trbl',
  rel_heights = c(2,0.7,1,10,3,1),
  rel_widths = c(1.5,13)
)


#we generate the legends
legend_combo <- cowplot::plot_grid(plotlist = list(dev_status_legend,MLST_legend,main_heatmap_legend,receptor_plot_legend,phage_genus_legend,NULL),ncol = 1,align='hv',rel_heights=c(1,1.7,1.6,1,3))
comboplot_with_legend <- cowplot::plot_grid(plotlist = list(comboplot,legend_combo),ncol=2,rel_widths = c(1,0.3),rel_heights=c(1,0.8))

#we save the plot
ggsave(plot = comboplot_with_legend,filename = paste0(fig_folder,'Figure 2A.png'),width=17,height=9)
ggsave(plot = comboplot_with_legend,filename = paste0(fig_folder,'Figure 2A.pdf'),width=17,height=9)

#we output summary statistics for the phage genus info
CAP_genus_genomesize[is.na(CAP_genus_genomesize[,"genus"]),"genus"] <- 'NA'
genus_summary_info <- as.data.frame(table(CAP_genus_genomesize[,"genus"]))
colnames(genus_summary_info) <- c('genus','frequency')
openxlsx::write.xlsx(genus_summary_info,file = paste0(summary_stats_folder,'figure2a_genus_distribution.xlsx'))

#we output summary statistics phage genome length
genome_size_summary <- summary(as.numeric(CAP_genus_genomesize[,"genome.size"]))
outmat <- data.frame(names(genome_size_summary),as.vector(genome_size_summary))
colnames(outmat) <- c('stat type','stat')
openxlsx::write.xlsx(outmat,paste0(summary_stats_folder,"figure2a_genome_length.xlsx"))

#we output the MLST summary stats
outmat <- as.data.frame(table(SNIPR_MLST[["MLST_type"]]))
colnames(outmat) <- c('MLST','Frequency')
openxlsx::write.xlsx(outmat,paste0(outmat,"figure2a_MLST_summary.xlsx"))

#and we output summary stats for the receptors
outmat <- data.frame(table(plot_info_receptor[,c("receptor","presence")]))
colnames(outmat) <- c('Receptor','Identification','frequency')
openxlsx::write.xlsx(outmat,paste0(outmat,"figure2a_receptor_summary.xlsx"))

###########
#Figure 3A#
###########
#loads the data in an abt the plaquing efficiency of strains with different phage binding motifs
plotdat_unformatted <- as.matrix(read_excel(paste0(data_folder,"EOP_of_a15_a15.2_a17_on_receptor_mutants.xlsx"),col_names = T))

#set the row names to be the bacteria tested against and removes that that column
rownames(plotdat_unformatted) <- plotdat_unformatted[,1]
plotdat_unformatted <- plotdat_unformatted[,-1]

#The ones considered resistant did not have any plaques and were marked with an R.
#In the interest of showing our data we set these points to 1 so we can plot them on a log scale
plotdat_unformatted[plotdat_unformatted=='R'] <- 1

#we log-10 transform the data
plotdat_unformatted[,] <- log(as.numeric(plotdat_unformatted[,]),10)

#gets the names of the phages that are in the columns
phages <- unlist(strsplit(colnames(plotdat_unformatted),'[.][.][.]'))[seq(1,2*ncol(plotdat_unformatted),2)]

#we will calculate mean and SD for each replicate set of phage/bacteria combinations
plotdat <- as.data.frame(matrix(NA,ncol=4,nrow=0))
colnames(plotdat) <- c('bacteria','phage','mean','sd')

#we go through each combination of phage/bacteria, identify the correct columns, and calculate mean & sd from there
for(phage in unique(phages)){
  for(bacteria in rownames(plotdat_unformatted)){
    #gets the right values
    vals <- as.numeric(plotdat_unformatted[bacteria,phages==phage])
    
    #calculates and adds mean and sd
    plotdat[nrow(plotdat)+1,] <- c(bacteria,phage,mean(vals),sd(vals))
  }
}

#formats as numeric
plotdat[["mean"]] <- as.numeric(plotdat[["mean"]])
plotdat[["sd"]] <- as.numeric(plotdat[["sd"]])

#we add data for upper and lower bounds that are mean+/- sd
plotdat <- cbind(plotdat,'upper_bound'=plotdat[["mean"]]+plotdat[["sd"]],'lower_bound'=plotdat[["mean"]]-plotdat[["sd"]])

dodge_width=0.6

#In order to really drive the point home we will add a little zero on top of the variables that are zeroes
#we add the mean as text and then remove all the non-zeroes
plotdat <- cbind(plotdat,'text'=as.character(plotdat[["mean"]]))
plotdat[["text"]][plotdat[["text"]]!="0"] <- ''

#we reformat
plotdat[["bacteria"]] <- factor(plotdat[["bacteria"]])
plotdat[["phage"]] <- factor(plotdat[["phage"]],levels=c('α15','α17','α15.2'))

#we calculate the y-breaks
LOD=log(200,10)
y_breaks <-c(seq(-100,100,1),LOD)
y_break_labels <- c(to_tenth_power_labels((seq(-100,100,1))),'LOD     ')

p <- ggplot(data = plotdat,mapping = aes(x=bacteria,y=mean,fill=phage))#we initialize the plpt
p <- p + geom_bar(stat='identity',position=position_dodge(dodge_width),width=dodge_width)#adding bars that dodge to 0.6 to keep them nicely apart
p <- p + geom_errorbar(aes(ymin=lower_bound,ymax=upper_bound),width=0.17,position=position_dodge(dodge_width))#we add error bars
p <- p + scale_y_continuous(breaks=y_breaks,labels=y_break_labels)#sets the y scale to be log like
p <- p + scale_fill_manual(values = phage_colors)#we set the colors
p <- p + custom_theme+#sets the default theme
  theme(legend.position = c(0.85,0.5),legend.key.size = unit(0.3,'cm'),legend.title = element_blank(),legend.box.background = element_rect(colour = "black"))+#changes the legend to be inside the plot since we have a nice blank spot
  ylab(expression(italic("E. Coli")~ "(PFU/mL)"))+xlab('')
p <- p + geom_text(mapping=aes(x=bacteria,y=mean+1,label=text),position=position_dodge(dodge_width))#we add the zeroes to indicate zero
p <- p + theme(legend.spacing.x = unit(0.1,'cm'),legend.spacing.y = unit(0.1,'cm'),#sets the legend in a new position, removes the spacing
               axis.title.y=element_text(hjust=0.5,vjust=0.95,margin=margin(r=0)),#sets the y axis title in a new place to reflect the other plots
               axis.text.x = element_blank(),axis.ticks.x = element_blank(),#removes x axis text and ticks
               plot.margin = margin(t = 0.5,0,0,0,'cm'))#then lowers the plot margins and removes grid lines
p <- p + geom_hline(yintercept=LOD,linetype="dashed", size=1)#and we add the LOD line

#We wish to add a dot-plot to the bottom of the figure that summarizes which surface proteins/surface polysaccharides a bacteria expresses
bact <- levels(plotdat[["bacteria"]])

#we want to make a row that indicates whether or not the bacteria has a surface protein and one row that shows if it has a polysaccheride
#we assume they have either protein or polysaccharide
protein <- rep('+',length(bact))
polysaccheride <- protein
#and then any that have had their polysaccharide or surface protein removed, as indicated by a delta, have their + switch to a -
protein[grepl('Tsx',bact)] <- '-'
polysaccheride[grepl('LPS',bact)] <- '-'

#then we combine the data and indicate the bacteria by the column names
tile_dat <- rbind('Tsx'=protein,'LPS'=polysaccheride)
colnames(tile_dat) <- bact

#we reformat the data to be a bit more ggplot friendly
tile_dat <- reshape2::melt(tile_dat)
colnames(tile_dat) <- c('gene','bact','text')
#we make sure the order is the same between the two different plots
tile_dat[["bact"]] <- factor(as.character(tile_dat[["bact"]]),levels = bact)

#we then create the gene expression plot
gene_expression_plot <- ggplot(data=tile_dat,mapping = aes(x = bact,y=gene,label=text))+#the plot is initialized
  geom_text(size=5,position = position_nudge(y=0.12))+#adds the text and nudges the text slightly upwards. I think it looks nicer there :)
  custom_theme+#sets the theme 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank())+#and removes any x axis labels as well as the y axis title
  theme(plot.margin = margin(0.1,0.1,0.1,0.1,'cm'),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())#then removes the panel border, reduces the plot margin, and removes any grid lines since they are more or less superfluous 

#we combine the two plots 
comboplot <- ggpubr::ggarrange(p,gene_expression_plot,ncol=1,heights=c(70,15),align='v')

#then we insert an image illustrating our tail fiber swapping
tail_switch_figure <- ggdraw()+draw_image(paste0(art_folder,'phage_tailswitch_illustration.png'))+theme(plot.margin = margin(0,0,0,0,'cm'))

comboplot <- ggpubr::ggarrange(comboplot,tail_switch_figure,ncol=2,widths =c(70,12),align='v')


ggsave(comboplot,filename = paste0(fig_folder,'Figure 3A.pdf'),device=cairo_pdf,height=4,width=4)
ggsave(comboplot,filename = paste0(fig_folder,'Figure 3A.png'),height=4,width=4)



#we computed some of the important summary stats in the plotdat which we will output
summary_stats <- plotdat[,c("bacteria","phage","mean","sd")]
#we check the number of replicates carried out for each phage/bacteria combo
replicates <- table(unlist(strsplit(colnames(plotdat_unformatted),'[.][.][.]'))[seq(1,ncol(plotdat_unformatted)*2,2)])
#and adds the replicate number
summary_stats <- cbind(summary_stats,'replicates'=replicates[as.character(summary_stats[["phage"]])])

openxlsx::write.xlsx(summary_stats,file = paste0(summary_stats_folder,'figure3A.xlsx'))

#################
#Figure 3b, c, d#
#################

#loads the data with the number of surviving e. coli before and after CRIPSR engineering
plotdat <- as.data.frame(read_excel(paste0(data_folder,"number_of_survivors.xlsx"),col_names = T))
signifance_location_relative_to_max <- 1.1


#we generate the p-values by using both replicates
signif_data <- data.frame()
phages <- unique(plotdat[["phage"]])
for(strain in unique(plotdat[["strain"]])){
  #we calculate the p-value
  result <- wilcox.test(plotdat[plotdat[["strain"]] == strain & plotdat[["phage"]] == phages[1],"n_survivors"],
                        plotdat[plotdat[["strain"]] == strain & plotdat[["phage"]] == phages[2],"n_survivors"],)
  
  #we figure out where in the y axis to place the stars
  y_position <- max(c(plotdat[plotdat[["strain"]] == strain & plotdat[["phage"]] == phages[1],"n_survivors"],plotdat[plotdat[["strain"]] == strain & plotdat[["phage"]] == phages[2],"n_survivors"]))*signifance_location_relative_to_max
  
  signif_data <- rbind(signif_data,data.frame('xstart'=phages[1],
                                              'xend'=phages[2],
                                              'pval'=result[["p.value"]],
                                              'y_position'=y_position,
                                              'strain'=strain))
}

signif_data <- cbind(signif_data,'adjusted_pval'=p.adjust(signif_data[["pval"]]))

pval <- signif_data[["adjusted_pval"]]

#we create a string to indicate significance
text <- rep('NS',nrow(signif_data))
text[pval<0.05] <- '*'
text[pval<0.01] <- '**'
text[pval<0.001] <- '***'

signif_data <- as.data.frame(cbind(signif_data,'star_label'=text))


p <- ggplot()
p <- p + geom_boxplot(data = plotdat,aes(x=phage,y=n_survivors))#adds points with range
#we want to add the replicates slightly offset between the start and end variables defined here, so we wish to add each replicate slightly offset
x_offset_start <- -0.2
x_offset_end <- 0.2
offsets <- seq(0,length(unique(plotdat[["replicate"]]))-1,1)
offsets <- offsets*(x_offset_end-x_offset_start)-mean(offsets*(x_offset_end-x_offset_start))

#we add the replicates
for(replicate in unique(plotdat[["replicate"]])){
  p <- p + geom_point(data = plotdat[plotdat[["replicate"]]==replicate,],mapping = aes(x=phage,y=n_survivors,color=phage),position=position_nudge(x=offsets[replicate]))
}
#we create seperate facets for each strain
p <- p + facet_wrap(nrow = 1,facets = vars(strain),scales = 'free')
p <- p + ggsignif::geom_signif(signif_data,mapping = aes(xmin=xstart,xmax=xend,annotations=star_label,y_position=y_position),textsize=4,vjust=0.3,manual=T) #adds significance bar
p <- p + custom_theme+theme(legend.position='none')
p <- p + scale_color_manual(values=phage_colors)


#we make the barcharts
surviving_plaques <- as.data.frame(read_excel(paste0(data_folder,"surviving_plaques.xlsx")))

surviving_plaque_data <- reshape2::melt(surviving_plaques)

#we reformat slightly
surviving_plaque_data[["outcome"]] <- factor(surviving_plaque_data[["outcome"]],levels = c("Resistant","Reduced","Sensitive"))

#we make stacked barplots
barchart <- ggplot(surviving_plaque_data,aes(x=phage,y=value,fill=outcome))+geom_bar(position='stack',stat='identity')+facet_wrap(nrow=1,facets = vars(variable))+custom_theme


replaqing_color_scheme <- c('Resistant'=SNIPR_pale_red,
                            'Reduced'="#a39a98",
                            'Sensitive'=phage_colors[["α15.2"]])

highest_rel_y_pos_box <- 0.9
lowest_rel_y_pos_box <- 0.45

#We set the color scheme for resistances and such for the insert figure
#we go through each strain and add the plot seperately
for(strain in unique(surviving_plaque_data[["variable"]])){
  temp_plotdat <- plotdat[plotdat[["strain"]]==strain,]
  p <- ggplot()
  p <- p + geom_boxplot(data = temp_plotdat,aes(x=phage,y=n_survivors))#adds points with range
  #we want to add the replicates slightly offset between the start and end variables defined here, so we wish to add each replicate slightly offset
  x_offset_start <- -0.2
  x_offset_end <- 0.2
  offsets <- seq(0,length(unique(temp_plotdat[["replicate"]]))-1,1)
  offsets <- offsets*(x_offset_end-x_offset_start)-mean(offsets*(x_offset_end-x_offset_start))
  
  #we add the replicates
  for(replicate in unique(temp_plotdat[["replicate"]])){
    p <- p + geom_point(data = temp_plotdat[temp_plotdat[["replicate"]]==replicate,],mapping = aes(x=phage,y=n_survivors,color=phage),position=position_nudge(x=offsets[replicate]))
  }
  #we create seperate facets for each strain
  p <- p + ggsignif::geom_signif(signif_data[signif_data[["strain"]]==strain,],mapping = aes(xmin=xstart,xmax=xend,annotations=star_label,y_position=y_position),textsize=4,vjust=0.3,manual=T) #adds significance bar
  p <- p + custom_theme+theme(legend.position='none',axis.title.x = element_blank())
  p <- p + scale_color_manual(values=phage_colors)
  p <- p + ylab('Number of surviving colonies')+theme()
  
  #we insert a line to indicate where on the sampling of the leftmost distribution
  #we adjust the y-axis level to be raise if the median of the boxplot is very high
  #these values arent't *exactly* correct, but they are useful values unless the median valus is also the minimum value or the maximum value, so it'll do
  line_max_height=max(temp_plotdat[["n_survivors"]])*signifance_location_relative_to_max*highest_rel_y_pos_box #the first 1.1 multuplication is to ta
  line_min_height=max(temp_plotdat[["n_survivors"]])*signifance_location_relative_to_max*lowest_rel_y_pos_box
  
  #we figure out where the median is for each distribution
  medians <- temp_plotdat %>% group_by(phage) %>% summarise(median=median(n_survivors))
  #let's figure out how far up that is relative to the highest point
  rel_height <- max(medians[,"median"])/max(temp_plotdat[["n_survivors"]])
  #we figure out the location of the line
  line_y_var <- (line_max_height-line_min_height)*rel_height+line_min_height
  
  linedat <- data.frame('xstart'=1.5,'xend'=2.3,'ystart'=line_y_var,'yend'=line_y_var)
  p <- p + geom_segment(data=linedat,aes(x=xstart,y=ystart,xend=xend,yend=yend))
  #and we draw a line to indicate where it was samples from
  
  strain_data <- surviving_plaque_data[surviving_plaque_data[["variable"]]==strain,]
  barchart <- ggplot(strain_data,aes(x=phage,y=value,fill=outcome))+geom_bar(position='stack',stat='identity',color='#000000')+custom_theme+scale_y_continuous(breaks=seq(0,10,2))
  #we add a rectangle around the plot
  barchart <- barchart + ggtitle('Subsequent\ntesting')+theme(plot.title=element_text(hjust=0.5,size=10),plot.background=element_rect(color='#000000',fill='#ffffff',size=1),axis.title.x=element_blank())
  barchart <- barchart + scale_fill_manual('Phage\nsensitivity',values = replaqing_color_scheme)+ylab('Number of colonies')
  
  #we get the legend to put on the big figure 
  barchart_legend <- get_legend(barchart)
  #and then we remove it from the main plot
  barchart <- barchart+theme(legend.position = 'none')
  
  #and we insert it
  insert_plot <- p +theme(plot.margin=margin(r=2.7,unit = 'cm'))+ patchwork::inset_element(barchart, left = 0.85, right= 1.78,
                                                                                           bottom=lowest_rel_y_pos_box,top = highest_rel_y_pos_box)
  
  #we add the legend
  insert_plot <- insert_plot+patchwork::inset_element(barchart_legend, left = 1.2, right= 1.55,
                                                      top=lowest_rel_y_pos_box*0.75,bottom = 0.25)
  
  ggsave(insert_plot,filename = paste0(fig_folder,'Figure 3BCD ',strain,'.pdf'),device=cairo_pdf,width=3,height=4)
  ggsave(insert_plot,filename = paste0(fig_folder,'Figure 3BCD ',strain,'.png'),device=cairo_pdf,width=3,height=4)
}

#we output summary statistics
outmat <- plotdat %>% group_by(strain,phage) %>% summarize('mean'=mean(n_survivors),'sd'=sd(n_survivors))
openxlsx::write.xlsx(outmat,paste0(summary_stats_folder,'figure3BCD_n_survivor_summary.xlsx'))

#the data we loaded for the re-exposure experiments are already in a nice summary format os we just output it as it is
write.xlsx(surviving_plaque_data,paste0(summary_stats_folder,'figure3BCD_re-exposure_summary.xlsx'))

###########
#Figure 4A#
###########
#When phages are grown with an empty vector they grow to a variety of CFU/mL, however when they are grown with a CRIPSR armed phage they never reach above
#the LOD.

#we load the growth data when bacteria are grown with an empty vector
plotdat_unformatted <- read_excel(paste0(data_folder,"CRISPR killing of selected clinical isolates by conjugation of CGV-EcCas.xlsx"))

#we only include the bacteria that could grow at all with the empty vector. It's hard to argue that you prevent something from growing if it never grew in the first place
nonviable_experiments <- rowSums(plotdat_unformatted[,grepl('empty_vector',colnames(plotdat_unformatted))]==0) 
plotdat_unformatted <- plotdat_unformatted[nonviable_experiments<3,]

#each run included our production strain. We would like to remove this positive control, so we do that
plotdat_unformatted <- plotdat_unformatted[plotdat_unformatted[["Strain"]]!=52,]

#alright lets try to make this steaming pile of shit look nice
plotdat_unformatted <- as.data.frame(plotdat_unformatted)
rownames(plotdat_unformatted) <- plotdat_unformatted[,"Strain"]
plotdat_unformatted <- plotdat_unformatted[,-1]

#Let's start with the base and build from there. The base needs to be the same in every iteration anyway.
plotdat_reshaped <- reshape2::melt(as.matrix(plotdat_unformatted[]))
colnames(plotdat_reshaped) <- c('Strain','type_badformat','value')

plotdat_reshaped[,"value"] <- log(plotdat_reshaped[,"value"],10)
plotdat_reshaped[plotdat_reshaped[,"value"]==-Inf,"value"] <- 0

type <- rep('Empty vector',nrow(plotdat_reshaped))
type[!grepl('empty',plotdat_reshaped[,"type_badformat"])] <- 'EcCAS'
plotdat_reshaped <- cbind(plotdat_reshaped,'type'=type)
plotdat_reshaped[["Strain"]] <- as.character(plotdat_reshaped[["Strain"]])

#we compute the avg CFU for each unique combo
plotdat_avgs <- unique(plotdat_reshaped[,c("Strain","type")])
avgs <- c()
sds <- c()
for(row in rownames(plotdat_avgs)){
  plotdat_avgs[row,"Strain"]
  
  values <- plotdat_reshaped[plotdat_reshaped[,"Strain"]==plotdat_avgs[row,"Strain"] & plotdat_reshaped[,"type"]==plotdat_avgs[row,"type"],"value"]
  avgs <- c(avgs,mean(values))
  sds <- c(sds,sd(values))
}
plotdat_avgs <- cbind(plotdat_avgs,'mean'=avgs,'upper_limit'=avgs+sds,'lower_limit'=avgs-sds)

LOD <- 200

#we sort the plot data according to mean so the plot shows highest to lowest
strain_order <- plotdat_avgs[plotdat_avgs[,"type"]=='Empty vector',"Strain"][order(plotdat_avgs[plotdat_avgs[,"type"]=='Empty vector',"mean"],decreasing = T)]
plotdat_reshaped[,"Strain"] <- factor(plotdat_reshaped[["Strain"]],levels=strain_order)
plotdat_avgs[,"Strain"] <- factor(plotdat_avgs[,"Strain"],levels = strain_order)

y_breaks <- seq(-10,10)
y_labels <- to_tenth_power_labels(seq(-10,10))

p <- ggplot(data = plotdat_reshaped,mapping = aes(x=Strain,y=value,color=type,fill=type))
#lets try to add some faded bars
#p <- p+geom_bar(data=plotdat_avgs,mapping = aes(x=Strain,fill=type,y=mean),stat="identity",color=SNIPR_color2,alpha=0.3,width=0.3)+scale_fill_manual(values = c('EcCAS'=SNIPR_green,'Empty vector'=SNIPR_color2))
#we add points
p <- p + geom_jitter(size=0.5,width=0.25)
#change some theme settings and add an LOD label on the right side of the plot
p <- p+custom_theme + scale_y_continuous(breaks=y_breaks,labels = y_labels,sec.axis=sec_axis(trans=~.*1,name='',breaks = log(LOD,10),labels = 'LOD'))
p <- p+theme(axis.text.x=element_blank(),legend.title = element_blank())
#we add a line to indicate the LOD
p <- p + geom_hline(yintercept=log(LOD,10),linetype="dashed", size=1)#and we add the LOD line
#we change the y and x axis plots
p <- p + ylab('CFU/mL')+xlab(bquote('Bacterial panel ('~italic("n")~' = '~.(length(unique(plotdat_reshaped[["Strain"]])))~')'))
#sets the color scales
p <- p + scale_color_manual(values=c('EcCAS'=SNIPR_green,'Empty vector'=SNIPR_grey))

#we put the legend in the free space on the top right of the plot
figure_legend <- get_legend(p+theme(legend.box.background = element_rect(colour = "black")))

p_with_legend <- p+theme(legend.position = 'none')+patchwork::inset_element(figure_legend, left = 0.88, right= 0.95,
                                                                top=0.95,bottom = 0.8)

ggsave(plot = p_with_legend,filename = paste0(fig_folder,'Figure 4A.png'),height=4,width=14)
ggsave(plot = p_with_legend,filename = paste0(fig_folder,'Figure 4A.pdf'),height=4,width=14)


#we will compute the summary stats for each series of experiments
outmat <- cbind('bacteria'=rownames(plotdat_unformatted),
                'avg_empty_vector'=rowMeans(plotdat_unformatted[,c("empty_vector 1 (cfu/ml)","empty_vector 2 (cfu/ml)","empty_vector 3 (cfu/ml)")]),
                'SD_empty_vector'=rowSds(as.matrix(plotdat_unformatted[,c("empty_vector 1 (cfu/ml)","empty_vector 2 (cfu/ml)","empty_vector 3 (cfu/ml)")])),
                'avg_EcCAS'=rowMeans(plotdat_unformatted[,c("EcCAS 1 (cfu/ml)","EcCAS 2 (cfu/ml)","EcCAS 3 (cfu/ml)")]),
                'SD_EcCAS'=rowSds(as.matrix(plotdat_unformatted[,c("EcCAS 1 (cfu/ml)","EcCAS 2 (cfu/ml)","EcCAS 3 (cfu/ml)")])),
                'replicates'=rep(3,nrow(plotdat_unformatted)))

openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'Figure4A.xlsx'))



###########
#Figure 4B#
###########
#we read the data abt biofilm formation using different promoters
biofilm_data <- as.data.frame(read_excel(paste0(data_folder,"biofilm data.xlsx")))

#first we remove the signal that is observed in the the experiments without any cells
#we do this as an average to get a more representative measure of the signal for each of the other experiments
biofilm_data[,colnames(biofilm_data)[grepl('promoter',tolower(colnames(biofilm_data)))]] <- biofilm_data[,colnames(biofilm_data)[grepl('promoter',tolower(colnames(biofilm_data)))]]-mean(biofilm_data[["No cells"]])

#we register the number of promoters we are using
n_promoters <- sum(grepl('Promoter ',colnames(biofilm_data)))

#since we don't want to deal with Taylor expansion of random variables to calculate the standard deviation of a ratio of the mean of two independent samples
#we will simply calculate the avg flouresence value for experiment carried out without a promoter, then normalize promoters A and B to that result
biofilm_data[,paste0('Promoter ',LETTERS[1:n_promoters])] <- biofilm_data[,paste0('Promoter ',LETTERS[1:n_promoters])]/mean(biofilm_data[["No promoter"]])

#we reformat to a more ggplot friendly format
plotdat <- reshape2::melt(biofilm_data[,paste0('Promoter ',LETTERS[1:n_promoters])])
colnames(plotdat) <- c('Promoter','Relative_flouresence')
plotdat_points <- plotdat

#we wish for the dot sto be a slightly different color than the bar so we give them a different set of color
plotdat_points <- cbind(plotdat_points,'plotdat_color'=paste0(plotdat_points[,"Promoter"],' dots'))

#we calculate the means and standard deviations of the measurements
plotdat <- data.frame('Promoter'=paste0('Promoter ',LETTERS[1:n_promoters]),'means'=colMeans(biofilm_data[,paste0('Promoter ',LETTERS[1:n_promoters])]),'sds'=matrixStats::colSds(as.matrix(biofilm_data[,paste0('Promoter ',LETTERS[1:n_promoters])])))

#we add more y-breaks
y_breaks <- seq(0,1,0.2)
#we wish to show percentages
y_labels <- paste0(y_breaks*100,' %')

color_scale <- c(rep(SNIPR_color1,n_promoters),rep(SNIPR_grey,n_promoters))
names(color_scale) <- c(paste0('Promoter ',LETTERS[1:n_promoters]),paste0('Promoter ',LETTERS[1:n_promoters],' dots'))

#we calculate the p-value between the two promoters
pvalue <- t.test(biofilm_data[["Promoter A"]],biofilm_data[["Promoter B"]])[['p.value']]
signif_data <- data.frame('xstart'="Promoter A",'xend'="Promoter B",pvalue=pvalue,'yvalue'=max(plotdat_points[,"Relative_flouresence"])+0.07)

#we add some stars (I hope)
stars=rep('NS',nrow(signif_data))
stars[signif_data[,"pvalue"]<0.05] <- '*'
stars[signif_data[,"pvalue"]<0.01] <- '**'
stars[signif_data[,"pvalue"]<0.001] <- '***'
signif_data <- cbind(signif_data,'stars'=stars)

#we initialize the plot
p <- ggplot()
#we add the bars 
p <- p + geom_bar(data=plotdat,mapping = aes(x=Promoter,y=means,color=Promoter),stat='identity',position = position_dodge2(width = 0.9),width = 0.6,fill='#FFFFFF',size=1.5)
p <- p + geom_point(plotdat_points,mapping = aes(x=Promoter,y=Relative_flouresence,color=plotdat_color))
#we add the error bars
#p <- p + geom_errorbar(width=0.5)
#change the color scheme to be more consistent with other figures
p <- p + scale_color_manual(values=color_scale)
#changes the theme and sets the y axis label
p <- p + custom_theme+theme(legend.position = 'none',axis.title.x=element_blank())+ylab('Relative metabolic activity')
#adds more breaks to the y-axis
p <- p + scale_y_continuous(breaks=y_breaks,labels=y_labels,limits=c(0,1))

p <- p + ggsignif::geom_signif(data = signif_data,mapping = aes(xmin=xstart,xmax=xend,annotations=stars,y_position=yvalue),textsize=3,vjust=0.5,manual = T)



ggsave(plot = p,filename = paste0(fig_folder,'Figure 4B.pdf'),width=4,height=5)
ggsave(plot = p,filename = paste0(fig_folder,'Figure 4B.png'),width=4,height=5)


#we output some summary stats
outdat <- cbind('means'=colMeans(biofilm_data[,c("No promoter","Promoter A","Promoter B","No cells")]),
                'sds'=colSds(as.matrix(biofilm_data[,c("No promoter","Promoter A","Promoter B","No cells")])))

outdat_processed <- plotdat[,c("means","sds")]
rownames(outdat_processed) <- paste0(rownames(outdat_processed),' post processing')

outdat <- rbind(outdat,outdat_processed)

outdat <- rbind('experiment'=rownames(outdat),outdat)

#everything was carried out in triplicate
outdat <- cbind(outdat,'replicates'=3)

openxlsx::write.xlsx(outdat,file = paste0(summary_stats_folder,'figure4b.xlsx'))


####################
#Figure 4C, D, E, F#
####################
#we read the plaqueing results which needs to be displayed as lines
linedat_unformatted <- read_excel(paste0(data_folder,"number_of_unadsorbed_phages_in_CAS3_RT-qPCR_assay.xlsx"),col_names = T)

#we do some light reformatting to make the data more ggplot friendly
linedat_reformatted <- reshape2::melt(as.data.frame(linedat_unformatted))
colnames(linedat_reformatted) <- c('phage','minutes','value')
#we convert the minute columns to numeric minutes
linedat_reformatted[["minutes"]] <- as.numeric(gsub('"','',linedat_reformatted[["minutes"]]))

#Here we will identify unique combinations of phage and time-point to identify replicates
#and calculate summary stats
plot_data_lines <- unique(linedat_reformatted[,c("phage","minutes")])
#we wish to calculate mean and standard deviation after log transformation
logged_means <- c()
logged_sds <- c()

for(row in rownames(plot_data_lines)){
  #now we get the repevant replicates
  values <- linedat_reformatted[linedat_reformatted[["phage"]]==plot_data_lines[row,"phage"] & linedat_reformatted[["minutes"]]==plot_data_lines[row,"minutes"],"value"]
  #then we log-10 transform them
  values <- log(values,10)
  #and calculate mean and sd
  logged_means <- c(logged_means,mean(values))
  logged_sds <- c(logged_sds,sd(values))
  
}
#we then save the data
plot_data_lines <- cbind(plot_data_lines,'logged_mean'=logged_means,'logged_sd'=logged_sds,'lower_bound'=logged_means-logged_sds,'upper_bound'=logged_means+logged_sds)
#we add alphas instead of as
plot_data_lines[["phage"]] <- gsub('a','α',plot_data_lines[["phage"]])


#Now we load the CAS expression data that will be displayed as bars
bardat_unformatted <- read_excel(paste0(data_folder,"CAS3_expression_levels_RT-qPCR_assay.xlsx"),col_names = T)
#Currently both phage and time information is in the same column, so we split this into two columns
splitdat <- unlist(strsplit(gsub('[)]','',bardat_unformatted[["sample"]]),' [(]'))
bardat_unformatted <- cbind("phage"=splitdat[seq(1,length(splitdat),2)],'time'=as.numeric(splitdat[seq(2,length(splitdat),2)]),bardat_unformatted)

#We wish to calculate the mean and sd for each biological replicate
plot_data_bars <- unique(bardat_unformatted[,c("phage","time")])
means <- c()
sds <- c()
for(row in rownames(plot_data_bars)){
  #fetches the values and calculates both mean and sd
  ratios <- unlist(bardat_unformatted[bardat_unformatted[["phage"]]==plot_data_bars[row,"phage"] & bardat_unformatted[["time"]]==plot_data_bars[row,"time"],c("CAS3/G r1","CAS3/G r2","CAS3/G r3")])
  means <- c(means,mean(ratios))
  sds <- c(sds,sd(ratios))
}

#we attach the mean and standard deviation data
plot_data_bars <- cbind(plot_data_bars,'means'=means,'sds'=sds)

#we insert alphas instead of as
plot_data_bars[["phage"]] <- paste0('α',plot_data_bars[["phage"]])

#finds upper and lower bounds
plot_data_bars <- cbind(plot_data_bars,'upper_bound'=plot_data_bars[["means"]]+plot_data_bars[["sds"]],'lower_bound'=plot_data_bars[["means"]]-plot_data_bars[["sds"]])

#Since we wish to have a second y axis type plot, we calculate the scaling coefficient between the line data and the bar data
#since we essentially just add the data on the same scale, but set a y-axis on the right side that is on a different scale
coefficient <- max(plot_data_lines[["upper_bound"]])/max(plot_data_bars[['upper_bound']])

#For the lines we wish for all the lines to be black, so we create a custom color scale, so these register on a color scale different from the bars
line_values <- c('1'="#000000",'2'="#000000",'3'="#000000",phage_colors)
line_values[names(line_values)] <- "#000000"

#we create the breaks for the line data
line_breaks <- seq(0,floor(max(plot_data_lines[["upper_bound"]])),2)
line_breaklabs <- to_tenth_power_labels(line_breaks)

#then we create the line breaks for the other y-axis
bar_breaks <- seq(0,ceiling(max(plot_data_bars[["upper_bound"]])))
bar_breaklabs <- bar_breaks

#we also add some arrows that link the lines to the right y-axis and the bars to the left
#we wish to an arrow from the left-most dot in the bar-plot and have it point left, and 
#an arrow on the top of the right-most bar and have it point right
#first identify the left-most points
lowest_time <- plot_data_lines[["minutes"]]==min(plot_data_lines[,"minutes"])
#and get the x and y coordinates for these points
annotation_dat <- plot_data_lines[lowest_time,c("phage","minutes","logged_mean")]

#then we add another x-axis point that is 5 minutes further to the left for the arrow to point to
annotation_dat <- cbind(annotation_dat,'minutes_end'=rep(min(plot_data_lines[,"minutes"])-5,nrow(annotation_dat)))
annotation_dat <- as.data.frame(annotation_dat)

#then we add arrows on the right-most bar to indicate to read the bars on the right y-axis
rightmost_points <- plot_data_bars[["time"]]==max(plot_data_bars[["time"]])
#we get the relevant data
annotation_to_add <- plot_data_bars[rightmost_points,c("phage","time","means")]
#and we scale the arrow position with the scaling coefficient to make sure the scale is the same
#we also subtract a little bit from the y-axis so the arrow isn't resting on top of the barplot
#because it looks awkward
annotation_to_add[,"means"] <- annotation_to_add[,"means"]*coefficient-0.06
#then we add the end-point on the x-axis to be 5 minutes to the right of the farthest right point
annotation_to_add <- cbind(annotation_to_add,rep(max(plot_data_bars[["time"]])+5,nrow(annotation_to_add)))
#we standardize the column names to allow merging and merge the annotation data from the bars and the lines together
colnames(annotation_to_add) <-  c('phage','minutes','logged_mean','minutes_end')
annotation_dat <- rbind(annotation_dat,annotation_to_add)

#we get the x-axis breaks and labels
x_breaks <- sort(unique(plot_data_lines[["minutes"]]))
x_labels <- x_breaks
x_labels[length(x_labels)] <- paste0(x_labels[length(x_labels)],' min')


#first we initialize the plot
p <- ggplot()#boot the plot

#First we add the arrows to indicate if the bars/lines should be read on the right/left axis in a light grey and have that be on a seperate color axis
p <- p + geom_segment(data = annotation_dat,mapping = aes(x=minutes,y=logged_mean,xend=minutes_end,yend=logged_mean),linetype='solid',color='#9c9c9c', arrow = arrow(length = unit(0.03, "npc")))

#we add the bars, which are identifiex by the second axis and should thus be scaled by the scaling coefficient
p <- p + geom_bar(data = plot_data_bars, mapping = aes(x = time,y=means*coefficient,fill=phage),position='dodge',stat='identity',width=5)#first we add the bars
#similarly we add the error bars and scale then with the scaling coefficient
p <- p + geom_errorbar(data = plot_data_bars,aes(x = time,ymin=lower_bound*coefficient, ymax=upper_bound*coefficient), width=2,position=position_dodge(9))

#now we add the lines which we offset slightly to the left, since the error bars of the line and the barplot are impossible to tell apart if they overlap perfectly
p <- p + geom_line(data=plot_data_lines,mapping = aes(x=minutes-0.7,y=logged_mean,color=phage),size=0.7)+geom_point(data=plot_data_lines,mapping = aes(x=minutes-0.7,y=logged_mean,color=phage),size=1)#then we add the down-scaled and logged lines
p <- p + geom_errorbar(data = plot_data_lines,aes(x = minutes-0.7, color=phage,ymin=(logged_mean-logged_sd), ymax=(logged_mean+logged_sd)), width=1)

#next we use change the colors for the lines and the bars
p <- p + scale_color_manual(values = line_values)+scale_fill_manual(values=phage_colors)
#we remove the color guide from the plot since it is self-explanatory once facets have been implemented
p <- p + guides(color = FALSE)
#we set some theme settings to make the plot look a bit nicer
p <- p + custom_theme+theme(panel.grid.minor = element_blank(),legend.title = element_blank(),
                            axis.title.x = element_blank(),
                            legend.position = 'none',strip.text = element_text(size=text_size),
                            axis.title.y.right=element_text(angle=90,vjust=0.5,hjust=0.5))

#we add the second axis and transform the break points with the scaling coefficient, while also editing the breaks and break labels
p <- p + scale_y_continuous(name = "CAP (PFU/mL)",breaks=line_breaks,labels=line_breaklabs,sec.axis = sec_axis(trans=~./coefficient,name=expression(italic("cas3/gapA")~ "ratio"),breaks=bar_breaks,labels=bar_breaklabs))

#We wish for the annotation arrows to point directly at the bars, so we change the limits of the y-axis to cut off right where the arrows end
#also we set x axis breaks to only display time-points we have measured
p <- p + scale_x_continuous(breaks=x_breaks,labels = x_labels,limits=range(annotation_dat[["minutes_end"]]))

#We create facets for the plot
p <- p+facet_wrap(~phage,ncol = 2)

#and finally set the axis labels in a way so you don't have to turn your head to read them

ggsave(plot = p,filename = paste0(fig_folder,'Figure 4CDEF.png'),height=5,width=6,device=cairo_pdf)
ggsave(plot = p,filename = paste0(fig_folder,'Figure 4CDEF.pdf'),height=5,width=6,device=cairo_pdf)


#we output some summary stats
openxlsx::write.xlsx(cbind(plot_data_lines[,c("phage","minutes","logged_mean","logged_sd")],'replicates'=2),file = paste0(summary_stats_folder,'Figure 4CDEF PFU data.xlsx'))
openxlsx::write.xlsx(cbind(plot_data_bars[,c("phage","time","means")],'replicates'=2),file = paste0(summary_stats_folder,'Figure 4CDEF CAS/ratios data.xlsx'))



###############
#Figure 4G, H#
###############
#we load the data competition assay data where a15 and a20 caps are shown to out compete their wildtype
plotdat <- read.delim(paste0(data_folder,"competition assay data.tsv"))

#we replace the as with alphas
plotdat[["phage_mix"]] <- gsub('a','α',plotdat[["phage_mix"]])

#convert to numeric 
plotdat[["CAPs_as_fraction"]] <- as.numeric(plotdat[["CAPs_as_fraction"]])

#we create the plot and add bars
p <- ggplot(data = plotdat,mapping = aes(x = propagation_step,y = CAPs_as_fraction,fill=color))
p <- p + geom_bar(stat='identity',position = 'dodge')

#and we add custom colors
p <- p + scale_fill_manual(values = phage_colors)
#and change the theme into a more paper-friendly format
p <- p + custom_theme+theme(legend.title=element_blank(),axis.title.x = element_blank(),legend.position = 'none',strip.text.x = element_text(size=text_size),panel.grid.major.x = element_blank())
#we set the y-axis title
p <- p + ylab('CAP fraction of total phage content')
#and add facets to the plot, allowing the x axis to scale freely
p <- p + facet_grid(cols = vars(phage_mix),scales='free_x',space = 'free_x')
#adds the x axis title as a tag on the right
p <- p + theme(plot.tag.position = 'bottomright',plot.tag = element_text(margin=margin(-13.5,0,0,0,'pt'),size=text_size,hjust=0,vjust=0))+labs(tag = 'Propagation\nsteps')


ggsave(plot = p,filename = paste0(fig_folder,'Figure 4GH.pdf'),height=5,width=4,device=cairo_pdf)
ggsave(plot = p,filename = paste0(fig_folder,'Figure 4GH.png'),height=5,width=4)


#in this case, since there are no replicates, there is not really that much to do with the data
outmat <- plotdat[,-grep('color',colnames(plotdat))]
outmat <- cbind(outmat,'replicates'=1)


openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'figure4GH.xlsx'))


###########
#Figure 5A#
###########
#We load the distance matrix generated by MASH
distance_matrix <- read.delim(paste0(data_folder,"jmi_mash_distance_matrix.phy.tsv"),row.names = 1,header = T)

#We use ape's neighbor-joining function
tax_tree <- ape::nj(as.matrix(distance_matrix))


#We load the phylogroup data
JMI_phylogrouping <- read.delim(paste0(data_folder,"JMI_phylogroup.tsv"), header=FALSE)
colnames(JMI_phylogrouping) <- c('strain','alleles','allele_pos_neg','sig_gene','phylogroup','originating_file')

#We set the strain the annotation originated from as row names to be consistent with the distance matrix
JMI_phylogrouping[,"strain"] <- gsub('.fa','',gsub('.fna','',JMI_phylogrouping[,"strain"]))
rownames(JMI_phylogrouping) <- JMI_phylogrouping[,"strain"]

#We load the phylogroup annotations
JMI_MLST_data <- read.delim(paste0(data_folder,"JMI_MLST.tsv"), header=T)
MLST_prevalence_before_othering <- table(JMI_MLST_data[["MLST_type"]])
#We do not have room for every MLST annotation, so we will set any that is not in the top 15 most common as "Other"
top_N_MLSTs <- 15
remove_names <- names(sort(table(JMI_MLST_data[,"MLST_type"]),decreasing = T))[top_N_MLSTs:length(unique(JMI_MLST_data[["MLST_type"]]))]
JMI_MLST_data[JMI_MLST_data[,"MLST_type"]%in%remove_names,"MLST_type"] <- 'Other'

#Reads the data from the plaquing trial
plaquing_data <- as.data.frame(read_excel(paste0(data_folder,"JMI_panel_plaquing.xlsx")))

#Sorts out any data that isn't in the JMI panel
plaquing_data <- plaquing_data[plaquing_data[["Bacterial strain"]]%in%rownames(distance_matrix),]
#Sets the row names to be the bacterial strain
rownames(plaquing_data) <- plaquing_data[["Bacterial strain"]]

#b4038 is so divergent that its long branch disrupts the figure, so we truncate it
to_truncate <- 'b4038'

#we derease the length of the longest branch by 2.7 fold because it's disruptively long. 
tax_tree[["edge.length"]][tax_tree[["edge.length"]]==max(tax_tree[["edge.length"]])] <- tax_tree[["edge.length"]][tax_tree[["edge.length"]]==max(tax_tree[["edge.length"]])]/2.7

#we boot the tree plot
tree_plot <- ggtree(tax_tree,layout = "rectangular",size=0.25) + theme(text = element_text(family = "Helvetica"))

#We find out which node is the node that we truncate and add a graphical element to indicate a truncation of that branch
tip_index <- na.omit(seq(1,nrow(tree_plot[["data"]]))[tree_plot[["data"]][["label"]]==to_truncate])
parent_node_index <- as.numeric(tree_plot[["data"]][tip_index,"parent"])

#now we can figure out how much we need to adjust the inserted graphic by to be somwhere of the horizontal portion of the branch
y_adjust <- as.numeric(tree_plot[["data"]][tip_index,"y"]-tree_plot[["data"]][parent_node_index,"y"])
x_adjust <- as.numeric((tree_plot[["data"]][tip_index,"x"]-tree_plot[["data"]][parent_node_index,"x"])*0.8)

#to adjust for the size of the image we adjust the y-axis a bit further
y_adjust <- y_adjust-0.4

#here we create the dataframe that we add to the tree
#first we get the index of both the parent node and the tip node

truncation_data <- data.frame("node" = parent_node_index,
                              "images" = c(paste0(art_folder,"break2.png")),
                              "nodetext"=as.character(parent_node_index))


#We indicate the colors that we use for MLSTs
names(MLST_colors) <- sort(unique(JMI_MLST_data[["MLST_type"]]))


#we add the truncation data
tree_plot <- tree_plot %<+% truncation_data

#adds the additional graphical data to the ggtree and nudges the graphical element towards the left
tree_plot <- tree_plot + geom_nodelab(aes(image=images), geom="image",nudge_x =x_adjust,nudge_y=y_adjust,vjust=0.5,hjust=0.5,size=0.03)+geom_treescale()

#we add a title indicating of panel size
tree_plot <- tree_plot + ylab(bquote('Phage panel ('~italic("n")~' = '~.(nrow(distance_matrix))~')'))

#we get the order of the strains to ensure the tree and heatmap is ordered in the same way
strain_order <- as.vector(na.omit(tree_plot[["data"]][["label"]][order(tree_plot[["data"]][["y"]])]))

#Adds the phylogroup using pheatmap
phylogroup_data <- data.frame('strain'=factor(JMI_phylogrouping[strain_order,"strain"],levels = strain_order),'phylogroup'=JMI_phylogrouping[strain_order,"phylogroup"])
phylogroup_plot <- ggplot(phylogroup_data,aes(y=strain,x='Phylogroup',fill=phylogroup))+geom_tile()+scale_fill_manual('Phylogroup',values=phylogroup_colors)+custom_theme+theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.border = element_blank(),axis.title.y=element_blank())

#we create ordered heatmaps for MLST
MLST_data <- data.frame('strain'=factor(rownames(JMI_MLST_data[strain_order,]),levels = strain_order),'MLST'=JMI_MLST_data[strain_order,"MLST_type"])
MLST_plot <- ggplot(MLST_data,aes(y=strain,x='MLST',fill=MLST))+geom_tile()+scale_fill_manual('MLST',values=MLST_colors)+custom_theme+theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.border = element_blank(),axis.title.y=element_blank())

#we create ordered heatmaps for the plaquing data
plaquing_data <- data.frame('strain'=factor(rownames(plaquing_data),levels = strain_order),'plaquing_result'=plaquing_data[,"n1"])
plaquing_plot <- ggplot(plaquing_data,aes(y=strain,x='Plaquing',fill=plaquing_result))+geom_tile()+scale_fill_manual('Plaquing result',values=c('Plaques'=SNIPR_green,'Lysis zone'=SNIPR_pale_green,'Negative'=SNIPR_pale_red))+custom_theme+theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.border = element_blank(),axis.title.y=element_blank())


#we combine the plots
combination_plot <- cowplot::plot_grid(plotlist = list(tree_plot,
                                              phylogroup_plot+theme(legend.position = 'none',axis.text.x=element_text(angle=50,vjust=1,hjust=1),plot.margin = margin(r = 0,l = 0,unit = 'cm')),
                                              MLST_plot+theme(legend.position = 'none',axis.text.x=element_text(angle=50,vjust=1,hjust=1),plot.margin = margin(r = 0,l = 0,unit = 'cm')),
                                              plaquing_plot+theme(legend.position = 'none',axis.text.x=element_text(angle=50,vjust=1,hjust=1),plot.margin = margin(r = 0,l = 0,unit = 'cm'))),
                                       nrow=1,align = 'hv',rel_widths =c(4,0.5,0.5,0.5),axis='trbl')

#we get the legends and combine those
legends <- cowplot::plot_grid(plotlist = list(NULL,
                                              get_legend(phylogroup_plot),
                                              get_legend(MLST_plot),
                                              get_legend(plaquing_plot),
                                              NULL),
                   ncol=1,align='hv',rel_heights = c(0,2,1.5,1,0.2)
)

#we add the legends to the combination plot
figure <- cowplot::plot_grid(plotlist = list(combination_plot,legends),rel_widths = c(1,0.2),nrow=1)

#we save the plot
ggsave(figure,file=paste0(fig_folder,'Figure 5A.pdf'),height=8,width=7)
ggsave(figure,file=paste0(fig_folder,'Figure 5A.png'),height=8,width=7)



#we output some summary stats in separate files
#first plaquing data
plaquing_summary <- table(plaquing_data[["n1"]])
openxlsx::write.xlsx(plaquing_summary,file = paste0(summary_stats_folder,'figure5a_plaquinginfo.xlsx'))

#then the phylogrouping
openxlsx::write.xlsx(table(JMI_phylogrouping[["phylogroup"]]),file = paste0(summary_stats_folder,'figure5a_phylogrouping.xlsx'))

#then the MLSTs
MLST_others_binned <- names(MLST_prevalence_before_othering)[!names(MLST_prevalence_before_othering)%in%JMI_MLST_data[["MLST_type"]]]
names(MLST_prevalence_before_othering)[names(MLST_prevalence_before_othering)%in%MLST_others_binned] <- 
  paste0(names(MLST_prevalence_before_othering)[names(MLST_prevalence_before_othering)%in%MLST_others_binned],' (others)')

openxlsx::write.xlsx(MLST_prevalence_before_othering,file = paste0(summary_stats_folder,'figure5a_MLST.xlsx'))



###########
#Figure 5B#
###########
#we load our own plaquing experiments
snipr_plaquing <- read_excel(paste0(data_folder,"SNIPR_panel_plaquing.xlsx"))
#we figure out the distribution of plaquing results from the two SNIPR plaquing runs we did
snipr_plaq_freq <- t(rbind(table(snipr_plaquing[["Potency 1"]]),table(snipr_plaquing[["Potency 2"]])))
#use the descrepency between the first and the second run to create standard deviations and means, thereafter we scale those to be a fraction of the total bacteria run for ease of comparing
#additionally, we write a stat with the total number of bacteria tested in this panel to be used as the facet header
SNIPR_dat <- data.frame('sds'=matrixStats::rowSds(snipr_plaq_freq)/nrow(snipr_plaquing),'means'=rowMeans(snipr_plaq_freq)/nrow(snipr_plaquing),'source'=rep('SNIPR',nrow(snipr_plaq_freq)))

#We load the plaquing data we recieved from JMI's independent experiments
jmi_plaques <- read_excel(paste0(data_folder,"JMI_panel_plaquing.xlsx"))

#we calculate the frequency of the types of plaques for each replicate
jmi_plaq_freq <- t(rbind(table(jmi_plaques[["n2"]]),table(jmi_plaques[["n1"]])))
#much like before, we use the discrepancy between the two runs to calculate standard deviations, and them scale those to the bactrial panel so they appear as fractions
JMI_dat <- data.frame('sds'=matrixStats::rowSds(jmi_plaq_freq)/nrow(jmi_plaques),'means'=rowMeans(jmi_plaq_freq)/nrow(jmi_plaques),'source'=rep("JMI",nrow(jmi_plaq_freq)))


#Since we wish for this to be illustrated as a waterfall plot we programatically have to decide on an order now as far as I can tell, so we go plaque->lysis zone->negatives
x_order <- c('Plaques','Lysis zone','Negative')

#we combine the results from JMI and SNIPR plaquing runs to a single plotting element
plotdat <- as.data.frame(
  rbind(cbind(JMI_dat,'type'=rownames(JMI_dat)),
        cbind(SNIPR_dat,'type'=rownames(SNIPR_dat))))
plotdat <- cbind(plotdat,'color_var'=plotdat[["type"]])
invisible_data_to_add <- data.frame()

#a variable to keep track of where to put the sd bar
real_height_data <- c()
for(row in rownames(plotdat)){
  #we isolate the appropriate panel data
  panel_dat <- plotdat[plotdat[["source"]]==plotdat[row,"source"],]
  
  #we get the results that need to be stacked underneath
  x_to_stack <- x_order[seq(0,grep(panel_dat[row,"type"],x_order)-1)]
  
  #and add those results together.
  stack_underneath <- sum(panel_dat[panel_dat[["type"]]%in%x_to_stack,"means"])
  
  #we also get the total height so we know where to render the sd from
  real_height_data <- c(real_height_data,plotdat[row,"means"]+stack_underneath)
  
  invisible_data_to_add <- rbind(invisible_data_to_add,data.frame(
    'sds'=NA,
    'means'=stack_underneath,
    'source'=plotdat[row,"source"],
    'type'=plotdat[row,"type"],
    'color_var'='invisible',
    "real_height"=NA
  ))
  
}
#we add the data together
plotdat <- rbind(cbind(plotdat,'real_height'=real_height_data),invisible_data_to_add)


plotdat[["type"]] <- factor(plotdat[["type"]],levels=c("Plaques","Lysis zone","Negative"))
plotdat[["color_var"]] <- factor(plotdat[["color_var"]],levels=c("Plaques","Lysis zone","Negative",'invisible'))


#we reformat the labels
plotdat[["source"]] <- factor(plotdat[["source"]])

new_levels <- c('SNIPR'=paste0("'SNIPR panel '*italic('n = ')*",length(unique(snipr_plaquing[["Bacterial strain"]]))),
                'JMI'=paste0("'JMI panel '*italic('n = ')*",length(unique(jmi_plaques[["Bacterial strain"]]))))

levels(plotdat[["source"]]) <- new_levels[levels(plotdat[["source"]])]


#we set the breaks be every 0.1 in fractions, and then relabel those to percentages
breaks <- seq(0,1,0.2)
breaklabs <- paste0(breaks*100,'%')
#we initialize the plot
p <- ggplot(data=plotdat,mapping = aes(x=type,y=means,ymax=real_height+sds,ymin=real_height-sds,fill=color_var))
#we add the grey bars to indicate plaquing type frequency
p <- p+geom_bar(stat='identity',position='stack',width=1)
#we add the error bars
p <- p+geom_errorbar()
#and facet based on whether the results were from SNIPR or JMI
p <- p+facet_wrap(~source,labeller=label_parsed)
#then we add the breaks in our own custom format
p <- p+scale_y_continuous(breaks=breaks,labels = breaklabs)
#change the fill scale
p <- p+scale_fill_manual(values=c("Lysis zone"=SNIPR_pale_green,"Plaques"=SNIPR_green,"Negative"=SNIPR_pale_red,'invisible'=NA))
#set the theme
p <- p+custom_theme+theme(strip.text = element_text(size=text_size),strip.background = element_rect(fill='white'),axis.title.x = element_blank(),legend.title = element_blank(),legend.position ='none')
#adds the y axis label
p <- p+theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1))+ylab(substitute(paste('Percentage of ',italic('E. coli'), ' panel',sep = ' ')))
#and saves the plot
ggsave(p,filename = paste0(fig_folder,'Figure 5B.pdf'),width=4.5,height=4,device=cairo_pdf)
ggsave(p,filename = paste0(fig_folder,'Figure 5B.png'),width=4.5,height=4)


#we'll create a summary stats file where we treat each run separately
outdat_SNIPR_replicates<-cbind('panel'='SNIPR','replicate'=c(1,2),rbind(table(snipr_plaquing[,c("Potency 1")]),table(snipr_plaquing[,c("Potency 2")])))
outdat_JMI_replicates<-cbind('panel'='JMI','replicate'=c(1,2),rbind(table(jmi_plaques[,c("n1")]),table(jmi_plaques[,c("n2")])))

outdat_replicates <- rbind(outdat_SNIPR_replicates,outdat_JMI_replicates)

openxlsx::write.xlsx(outdat_replicates,file = paste0(summary_stats_folder,'figure_5B_runs_separate.xlsx'))

#and we'll do one with the runs collapsed and with SDs
SNIPR_data <-rbind(table(snipr_plaquing[,c("Potency 1")]),table(snipr_plaquing[,c("Potency 2")]))
SNIPR_data <- cbind('panel'='SNIPR','means'=colMeans(SNIPR_data),'sds'=colSds(SNIPR_data))
SNIPR_data <- cbind('plaquing result'=rownames(SNIPR_data),SNIPR_data)

JMI_data <-rbind(table(jmi_plaques[,c("n1")]),table(jmi_plaques[,c("n2")]))
JMI_data <- cbind('panel'='JMI','means'=colMeans(JMI_data),'sds'=colSds(JMI_data))
JMI_data <- cbind('plaquing result'=rownames(JMI_data),JMI_data)


openxlsx::write.xlsx(rbind(SNIPR_data,JMI_data),file = paste0(summary_stats_folder,'figure5B_averages.xlsx'))




###########
#Figure 5C#
###########
#first we load the resistance data obtained from JMI
jmi_resistance_dat <- read_excel(paste0(data_folder,"JMI_strain_characteristics.xlsx"))

#We would like to include certain resistances, that are already binarized
binary_resistance_cols <- c("Carbapenem nonsusceptible CLSI","ESBL Phenotype","EUCAST 2021 MDR")

#For certain antibiotics we will binarize resistant/non-resistant around MIC50 as defined by EUCAST
resistance_cutoff_data <- read.csv2(paste0(data_folder,"EUCAST_cutoffs.csv"))

#We wish to include a column indicating our effectiveness against strains that are resistant to any fluoroquinolone
fluoroquinolones <- c('Ciprofloxacin','Levofloxacin','Moxifloxacin')


#We load data generated from our internal plaquing trials
plaquing_data <- read.csv(paste0(data_folder,"SNIPR_plaquing_trial_one.tsv"), sep="\t")

#remove missing values
plaquing_data <- plaquing_data[plaquing_data[,"Susceptibility"]!='#VALUE!',]


#We start gathering the nessecary data to create the plot 
resistance_data <- c()
#first we gather data for the resistance columns that have already been binarized
for(col in binary_resistance_cols){
  #we identify the strains that are resistant. Non-resistant bacteria are all indicated by "non", so those that do not contain "non" must be resistant
  resistant_bacteria <- jmi_resistance_dat[["ID"]][!grepl('non',jmi_resistance_dat[[col]])]
  
  #we check our 
  hits_of_resistant <- plaquing_data[plaquing_data[,"Bacterial.strain"]%in%resistant_bacteria,"Susceptibility"]=="yes"
  
  resistance_data <- rbind(resistance_data,cbind('resistance_type'=col,'n_hit'=sum(hits_of_resistant),'n_total'=length(hits_of_resistant),'percent_hit'=(sum(hits_of_resistant)/length(hits_of_resistant))*100))
}


#We evaluate which portion of flouroquinolone resistant bacteria 
all_resistants <- c()
for(FQ in fluoroquinolones){
  resistance_cutoff <- as.numeric(resistance_cutoff_data[["MIC50..mg.L."]][resistance_cutoff_data[["Antimicrobial.agent"]]==FQ])
  #gets the MIC values. Any time where the MIC value has not been reached in the range tested is denoted with an > and is not immediately relevant to us
  #we therefore remove the >s
  MIC_values <- as.numeric(gsub('>','',jmi_resistance_dat[[FQ]]))
  names(MIC_values) <- jmi_resistance_dat[["ID"]]
  
  #we test if the MIC value are above the EUCAST cut-offs
  resistant_bacteria <- names(MIC_values)[MIC_values>resistance_cutoff]
  
  #we save the strains that are found to be resistant to a flouroquinolone
  all_resistants <- c(all_resistants,resistant_bacteria)
}
#in case some are found to be multi-resistant we remove some
all_resistants <- unique(na.omit(all_resistants))

#we check if these resistant strains are susceptible in plaquing
hits_of_resistant <- plaquing_data[plaquing_data[,"Bacterial.strain"]%in%all_resistants,"Susceptibility"]=='yes'

resistance_data <- rbind(resistance_data,cbind('resistance_type'="Any.fluoroquinolone",'n_hit'=sum(hits_of_resistant),'n_total'=length(hits_of_resistant),'percent_hit'=(sum(hits_of_resistant)/length(hits_of_resistant))*100))

#we convert to data frame for plotting and ensure correct formatting of numbers
resistance_data <- as.data.frame(resistance_data)
for(col in colnames(resistance_data)[2:4]){
  resistance_data[[col]] <- as.numeric(resistance_data[[col]])
}

#We wish to create a stacked barplot with the percentage of strains hit written above the individual bars
#We create matrix that helps organize the data in a format that's a bit more plottable
plotdat <- matrix(NA,ncol = 4,nrow=0)
colnames(plotdat) <- c('AMR_name','n','n_type','bar_height')
for(row in rownames(resistance_data)){
  #for each row we figure out which fraction of resistant bacteria are susceptible
  plotdat <- rbind(plotdat,cbind('AMR_name'=resistance_data[row,"resistance_type"],#we get the name of the AMR type
                                 'n'=resistance_data[row,"n_hit"],#the number that is susceptible total
                                 'n_type'='Susceptible',#note that these numbers are for susceptibility
                                 'bar_height'=resistance_data[row,"n_hit"]/resistance_data[row,"n_total"]))#and create a column for the height of the bar for susceptible strains
  #and the ones that aren't hit
  n_not_hit <- resistance_data[row,"n_total"]-resistance_data[row,"n_hit"]
  plotdat <- rbind(plotdat,cbind('AMR_name'=resistance_data[row,"resistance_type"],#we note the type of resistance
                                 'n'=n_not_hit,#the amount of strains that are not susceptible in total
                                 'n_type'='Not susceptible',#we note that these are for not-susceptible strains
                                 'bar_height'=n_not_hit/resistance_data[row,"n_total"]))#and calculate the bar height for not-susceptible strains
  
}

#we change the x axis text labels
x_axis_labels <- c("Carbapenem nonsusceptible CLSI"="Carbapenems")
x_axis_labels <- c(x_axis_labels,"ESBL Phenotype"="ESBL")
x_axis_labels <- c(x_axis_labels,"EUCAST 2021 MDR"="MDR")
x_axis_labels <- c(x_axis_labels,"Any fluoroquinolone"="Fluoroquinolones")

#ensures the correct formatting for ggplot
plotdat <- as.data.frame(plotdat)
plotdat[["n"]] <- as.numeric(plotdat[["n"]])
plotdat[["bar_height"]] <- as.numeric(plotdat[["bar_height"]])*100
plotdat[["n_type"]] <- factor(plotdat[["n_type"]],c('Not susceptible','Susceptible'))
plotdat[["AMR_name"]] <- gsub('[.]',' ',plotdat[["AMR_name"]])

#we remove any bar that does not have a "size" in the plot, thus preventing floating 0s that do not have a set background
plotdat <- plotdat[plotdat[["n"]]!=0,]

#we wish to order the bars in decreasing height of the susceptible bar
susceptible_data_only <- plotdat[plotdat[["n_type"]]=='Susceptible',]

#we rename the AMR names slightly
plotdat[["AMR_name"]] <- gsub('Any\n f','F',gsub('EUCAST\n 2021 ','',gsub('\n Phenotype','',gsub('\nnonsusceptible CLSI','s',plotdat[["AMR_name"]]))))

#we choose the order of the bars by changing the factor levels
plotdat[["AMR_name"]] <- factor(plotdat[["AMR_name"]],levels=plotdat[plotdat[["n_type"]]=='Susceptible',"AMR_name"][order(plotdat[plotdat[["n_type"]]=='Susceptible',"bar_height"],decreasing = T)])

#we set the colors we intend to use
colors=c(SNIPR_grey,SNIPR_green)


#we add text stating the percentage of resistant strains that are susceptible
plotdat <- cbind(plotdat,'text'=paste0(round(plotdat[["bar_height"]],2),' %'))
plotdat[["text"]][plotdat[["n_type"]]=='Not susceptible'] <- ''


#We want y-axis breaks at 10%, 20%, etc. 
y_breaks <- seq(0,100,20)
y_labs <- paste0(y_breaks,'%')

#we start by adding the stacked bars to the plot and add the text in the middle of the bars themselves
p <- ggplot(plotdat, aes(x = AMR_name, y =bar_height , fill = n_type, label = n)) +
  geom_bar(stat = "identity") +
  geom_text(data=plotdat[plotdat[["n_type"]]=='Susceptible',],size = 3, aes(y=50),color='white')+
  geom_text(data=plotdat[plotdat[["n_type"]]!='Susceptible',],size = 3, y=plotdat[plotdat[["n_type"]]=='Susceptible' & plotdat[["bar_height"]]!=100,"bar_height"]+plotdat[plotdat[["n_type"]]!='Susceptible',"bar_height"]/2,color='white')



#the we change the x and y axis names and set some theme-related settings
p <- p+ylab(substitute(paste('Percentage of ',italic('E. coli'), ' panel',sep = ' ')))+xlab('AMR type')+custom_theme+#sets x axis title, y axis title, and the custom theme
  ggplot2::scale_fill_manual(values = colors,name='Susceptibility\nto SNIPR001')+#sets  color scale
  ggplot2::scale_color_manual(values='#FFFFFF')#sets text color

#adds the text with the percentage info. We set the y-coordinate to above 100% to set it atop the bars
p <- p+geom_text(data = plotdat,aes(label=text,y=105),size=3)
#sets the breaks in the y axis scale
p <- p+scale_y_continuous(breaks = y_breaks,labels = y_labs)+scale_x_discrete(labels=x_axis_labels)
#removes the legend
p <- p+theme(legend.position = 'none',axis.text.x=element_text(angle=50,hjust=1,vjust=1))

ggsave(p,filename = paste0(fig_folder,'Figure 5C.pdf'),width = 3.5,height=4)
ggsave(p,filename = paste0(fig_folder,'Figure 5C.png'),width = 3.5,height=4)


outdat <- plotdat[,c("AMR_name","n","n_type")]
openxlsx::write.xlsx(x = outdat,file = paste0(summary_stats,'figure5c.xlsx'))


###########
#Figure 5D#
###########
reformat_column <- function(col,matrix){
  oldcol <- matrix[[col]]
  newcol <- rep(NA,length(oldcol))
  newcol[grep('lo',oldcol)] <- 'Lysis Zone'
  newcol[oldcol=='0'] <- 'Negative'
  newcol[as.numeric(oldcol)>0] <- 'Plaque'
  return(newcol)
}

#we load the satlin matrix and rename the sequences to fit the format of the other files
satlin_distance_matrix <- read.delim(paste0(data_folder,"satlin_distances.dist"),header = T)

satlin_tree <- ape::nj(as.matrix(satlin_distance_matrix))

#we create a plot of the tree to get the order of strains therein
satlin_treeplot <- ggtree(phytools::midpoint.root(satlin_tree))
order_of_leaves_in_tree <- as.vector(na.omit(satlin_treeplot[["data"]][["label"]][order(satlin_treeplot[["data"]][["y"]])]))

#We load the MLST data for the satlin panel
MLST_summary <- as.data.frame(read_excel(paste0(data_folder,"JMI_MLST_summary.xlsx")))
rownames(MLST_summary) <- MLST_summary[,"strain"]
#we only show the ones we used in 
MLST_summary[!MLST_summary[,"MLST"]%in%names(MLST_colors),"MLST"] <- 'Other'


#we load the phylogroup of the satlin panel
satlin_phylogroups <- as.data.frame(read_excel(paste0(data_folder,"satlin_phylogroups.xlsx")))
rownames(satlin_phylogroups) <- satlin_phylogroups[,"strain"]


#we load the bacteremia data
bacteriemia_data <- as.data.frame(read.delim(paste0(data_folder,"bacteremia.tsv"),header = F))
colnames(bacteriemia_data) <- c('Strain','Bacteremia')
rownames(bacteriemia_data) <- bacteriemia_data[["Strain"]]


#individual plaquing 1
Satlin_individual_n1_reformat <- as.data.frame(read_excel(paste0(data_folder,"Satlin individual n1 trimmed.xlsx")))
Satlin_individual_n2_reformat <- as.data.frame(read_excel(paste0(data_folder,"Satlin individual n2 trimmed.xlsx")))

rownames(Satlin_individual_n1_reformat) <- Satlin_individual_n1_reformat[,"strain"]
rownames(Satlin_individual_n2_reformat) <- Satlin_individual_n2_reformat[,"strain"]

Satlin_individual_n1_reformat <- Satlin_individual_n1_reformat[,-grep('strain',colnames(Satlin_individual_n1_reformat))]
Satlin_individual_n2_reformat <- Satlin_individual_n2_reformat[,-grep('strain',colnames(Satlin_individual_n2_reformat))]


#the plaquing of the combo
Satlin_001_n1 <- as.data.frame(read_excel(paste0(data_folder,"Satlin SNIPR001 n1.xlsx")))
Satlin_001_n2 <- as.data.frame(read_excel(paste0(data_folder,"Satlin SNIPR001 n2.xlsx")))


#reformat run 1
rownames(Satlin_001_n1) <- Satlin_001_n1[,"strain"]
colnames(Satlin_001_n1) <- c('strain','SNIPR001_1')
Satlin_001_n1[,"SNIPR001_1"] <- reformat_column(matrix = Satlin_001_n1,col = "SNIPR001_1")

#and run2
rownames(Satlin_001_n2) <- Satlin_001_n2[,"strain"]
colnames(Satlin_001_n2) <- c('strain','SNIPR001_2')
Satlin_001_n2[,"SNIPR001_2"] <- reformat_column(matrix = Satlin_001_n2,col = "SNIPR001_2")


all_data <- cbind(Satlin_individual_n1_reformat,Satlin_individual_n2_reformat,'SNIPR001_1'=Satlin_001_n1[,"SNIPR001_1"],'SNIPR001_2'=Satlin_001_n2[,"SNIPR001_2"])


#we reformat the combo data to be conservative consensus based
types <- unique(unlist(lapply(colnames(all_data),FUN = function(x){return(strsplit(x,'_')[[1]][1])})))
plaque_ranking <- c('Plaque'=1,'Lysis Zone'=2,'Negative'=3)
convert_to_rank <- function(x,plaque_ranking){
  return(plaque_ranking[x])
}
conservative_plaquing_results <- c()

for(type in types){
  relevant_data <- all_data[,grepl(type,colnames(all_data))]
  ranked_data <- apply(X = relevant_data,MARGIN = 2,FUN = convert_to_rank,"plaque_ranking"=plaque_ranking)
  conservative_estimate <- names(plaque_ranking)[matrixStats::rowMaxs(ranked_data,na.rm = T)]
  conservative_plaquing_results <- cbind(conservative_plaquing_results,conservative_estimate)
}
colnames(conservative_plaquing_results) <- types

rownames(conservative_plaquing_results) <- rownames(all_data)


#we trim the tree to only include what we have results for
satlin_tree <- ape::drop.tip(phy = satlin_tree,tip = satlin_tree[["tip.label"]][!satlin_tree[["tip.label"]]%in%rownames(conservative_plaquing_results)])


#we re-root the tree to it's midpoint
satlin_tree <- phytools::midpoint.root(satlin_tree)
satlin_tree_figure <- ggtree::ggtree(satlin_tree)
#we get the order of the labels
satlin_leaf_order <- na.omit(satlin_tree_figure[["data"]][["label"]][order(satlin_tree_figure[["data"]][["y"]])])

#We plot the MLST data
MLST_data <- data.frame('strain'=factor(rownames(MLST_summary[satlin_leaf_order,]),levels = satlin_leaf_order),'MLST'=MLST_summary[satlin_leaf_order,"MLST"])
MLST_plot <- ggplot(MLST_data,aes(y=strain,x='MLST',fill=MLST))+geom_tile()+scale_fill_manual('MLST',values=MLST_colors)+custom_theme+theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.border = element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(angle=50,vjust=1,hjust=1))

#We plot the phylogroup data
phylogroup_data <- data.frame('strain'=factor(satlin_phylogroups[satlin_leaf_order,"strain"],levels = satlin_leaf_order),'phylogroup'=satlin_phylogroups[satlin_leaf_order,"phylogroup"])
phylogroup_plot <- ggplot(phylogroup_data,aes(y=strain,x='Phylogroup',fill=phylogroup))+geom_tile()+scale_fill_manual('Phylogroup',values=phylogroup_colors)+custom_theme+theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.border = element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(angle=50,vjust=1,hjust=1))

#we plot the bacteriemia data
bacteriemia_data[,"Strain"] <- factor(bacteriemia_data[,"Strain"],levels = satlin_leaf_order)
bacteriemia_data <- bacteriemia_data[satlin_leaf_order,]
bacteriemia_plot <- ggplot(bacteriemia_data,aes(y=Strain,x='Bacteriemia',fill=Bacteremia))+geom_tile()+scale_fill_manual('Bacteremia',values = c("#ffffff","#a10e0e"))+custom_theme+theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),panel.border = element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(angle=50,vjust=1,hjust=1))


#we load the plaquing results
conservative_plaquing_results <- conservative_plaquing_results[rownames(conservative_plaquing_results)%in%satlin_tree[["tip.label"]],]
plotdat <- reshape2::melt(conservative_plaquing_results)
#we wish to facet the plot between inividual and collection
type <- rep('Individual CAP',nrow(plotdat))
type[plotdat[,"Var2"]=='SNIPR001'] <- 'SNIPR001'


plotdat <- cbind(plotdat,'type'=type)
plotdat[["Var2"]] <- gsub('a','α',as.character(plotdat[,"Var2"]))

#we set the order of the bacterial panel
plotdat[["Var1"]] <- factor(as.character(plotdat[["Var1"]]),levels=satlin_leaf_order)

#we set the color scheme
color_scheme <-  c('Lysis Zone'=SNIPR_pale_green,'Plaque'=SNIPR_green,'Negative'=SNIPR_pale_red)


#we plot
heatmap <- ggplot(plotdat,aes(x=Var2,y=Var1,fill=value))+geom_tile()+facet_grid(cols = vars(type),space = 'free',scales = 'free')+scale_fill_manual("Susceptibility",values =color_scheme)
heatmap <- heatmap+custom_theme+theme(axis.text.y=element_blank(),axis.title.x=element_blank(),strip.text.x = element_text(size=9.5),panel.grid = element_blank())+ylab(bquote('Bacterial panel ('~italic("n")~' = '~.(as.character(length(unique(plotdat[["Var1"]]))))~')'))
heatmap <- heatmap+scale_y_discrete(position='right')

#we get the legends for the plots. The color scheme is the same as 5a, so we only need to transfer the bacteriemia elgend
lr_margin=-2
plot_without_legends <- cowplot::plot_grid(plotlist = list(satlin_tree_figure,
                                                           phylogroup_plot+theme(legend.position = 'none',plot.margin = margin(l=lr_margin,r=lr_margin,unit='cm')),
                                                           MLST_plot+theme(legend.position = 'none',plot.margin = margin(l=lr_margin,r=lr_margin,unit='cm')),
                                                           bacteriemia_plot+theme(legend.position = 'none',plot.margin = margin(l=lr_margin,r=lr_margin,unit='cm')),
                                                           heatmap+theme(strip.text.x = element_text(size=7.5),legend.position = 'none',plot.margin = margin(l=lr_margin,r=lr_margin,0,0,'cm')))
                                           ,nrow=1,rel_widths=c(2,1,1,1,6),align = 'hv',axis = 'trbl')


plot_with_legend <- cowplot::plot_grid(plotlist = list(plot_without_legends,NULL,get_legend(bacteriemia_plot)),ncol=3,rel_widths = c(5,0.1,1))


ggsave(plot_with_legend,filename = paste0(fig_folder,"Figure 5D.pdf"),width=5.5,height=4,device = cairo_pdf)
ggsave(plot_with_legend,filename = paste0(fig_folder,"Figure 5D.png"),width=5.5,height=4)


###########
#Figure 5E#
###########
#we load the plaquing data and reformat it
plaquing_results <- read.xlsx(paste0(data_folder,"satlin_plaquing_individual_001_phages.xlsx"))
rownames(plaquing_results) <- plaquing_results[["strain"]]
plaquing_results <- plaquing_results[,-1]
colnames(plaquing_results) <- substr(colnames(plaquing_results),1,5)

#we reformat the combo data to be conservative consensus based
types <- unique(unlist(lapply(colnames(plaquing_results),FUN = function(x){return(strsplit(x,'_')[[1]][1])})))
plaque_ranking <- c('Plaque'=1,'Lysis Zone'=2,'Negative'=3)
convert_to_rank <- function(x,plaque_ranking){
  return(plaque_ranking[x])
}
conservative_plaquing_results <- c()

for(type in types){
  relevant_data <- plaquing_results[,grepl(type,colnames(plaquing_results))]
  ranked_data <- apply(X = relevant_data,MARGIN = 2,FUN = convert_to_rank,"plaque_ranking"=plaque_ranking)
  conservative_estimate <- names(plaque_ranking)[matrixStats::rowMaxs(ranked_data,na.rm = T)]
  conservative_plaquing_results <- cbind(conservative_plaquing_results,conservative_estimate)
}
colnames(conservative_plaquing_results) <- types

rownames(conservative_plaquing_results) <- rownames(plaquing_results)


#we calculate how many of these are not negative, i.e. lysis zones or positive
redundancy_data <- sort(rowSums(!conservative_plaquing_results=='Negative'))
redundancy_data <- table(redundancy_data)/length(redundancy_data)

#we make a stacked barplot
plotdat <- data.frame('redundancy'=names(redundancy_data),'frequency'=as.numeric(redundancy_data))
#we want to show the 1-4s as hit in green and the 0 as missed in red
color_var <- rep('hit',nrow(plotdat))
color_var[plotdat[["redundancy"]]==0] <- 'missed'
plotdat <- cbind(plotdat,'color_var'=color_var)

#we also add some blanks below. The 4 field, we should show without anything below it, but the 3 should have the value of the previous
#bars (i.e. 4) be below the 3, the 1 should have 2,3, and 4 below it
blanks <- cumsum(rev(redundancy_data))
blanks <- c(0,blanks[seq(0,length(blanks)-1)])
names(blanks) <-rev(names(redundancy_data))

hidden_data <- data.frame('redundancy'=names(blanks),'frequency'=blanks,'color_var'=rep('hide',length(blanks)))

plotdat <- rbind(plotdat,hidden_data)


#we add a hidden variable for the missing variable
required_x_variables <- seq(min(as.numeric(plotdat[["redundancy"]])),max(as.numeric(plotdat[["redundancy"]])),)
missing_redundancy <- required_x_variables[!required_x_variables%in%as.numeric(plotdat[["redundancy"]])]
plotdat <- rbind(plotdat,data.frame('redundancy'=missing_redundancy,'frequency'=1,'color_var'='hide'))




plotdat[["redundancy"]] <- factor(plotdat[["redundancy"]],seq(4,0,-1))
plotdat[["color_var"]] <- factor(plotdat[["color_var"]],c('hit','missed','hide'))


p <- ggplot()+geom_bar(data = plotdat,mapping = aes(x=redundancy,y=frequency,fill=color_var),position = 'stack',stat='identity',width=1)+scale_fill_manual(values = c('hit'=SNIPR_green,'missed'=SNIPR_pale_red,'hide'=NA))
#we add breaks to indicate
p <- p+custom_theme+scale_y_continuous(breaks=cumsum(rev(redundancy_data)),labels = paste0(round(cumsum(rev(redundancy_data))*100),'%'))+theme(legend.position = 'none')+xlab('Redundancy')+ylab('Cumulative host range (%)')

#we add a dotted line to indicate the missing 2nd 
linedat <- data.frame('x'=c(2.5,3.5),"y"=c(cumsum(rev(redundancy_data))["3"],cumsum(rev(redundancy_data))["3"]))
p <- p+geom_line(data = linedat,aes(x=x,y=y),linetype='dashed')
ggsave(plot = p,filename = paste0(fig_folder,"Figure 5E.pdf"),width=2.5,height=4)
ggsave(plot = p,filename = paste0(fig_folder,"Figure 5E.png"),width=2.5,height=4)

#we output the redundancies as summary statistics
redundancies <- sort(rowSums(!conservative_plaquing_results=='Negative'))
outdat_redundancies <- data.frame(table(redundancies))
colnames(outdat_redundancies) <- c('redundancy','freq')

write.xlsx(outdat_redundancies,file = paste0(summary_stats_folder,'figure5E_redundancy_summary.xlsx'))

###########
#Figure 6A#
###########
#loads the data for the amount of SNIPR001 and vehicle recovered from feces over 7 days for multiple replicates
plotdat_unformatted <- as.data.frame(read_excel(paste0(data_folder,"SNIPR001 recovery in pigs.xlsx")))
#for ease of use in reshape2::melt we set row names as plot data and remove it again
rownames(plotdat_unformatted) <- plotdat_unformatted[["time"]]
plotdat_unformatted <- plotdat_unformatted[,-match('time',colnames(plotdat_unformatted))]

#we set format in a more ggplot friendly format
plotdat <- reshape2::melt(as.matrix(plotdat_unformatted),timevar='time')
colnames(plotdat) <- c('time','dosed','measure')

#We make the names of vehicle and phage consistent instead of phage (1/2/3/...)
for(name in c('Phage','Vehicle')){
  plotdat[["dosed"]] <- gsub(paste0(paste0(name,' ',seq(1,99)),collapse='|'),name,plotdat[["dosed"]])
}

#We get each unique combination of time and administered solution to calculate means and standard deviations
time_dosed_combo <- unique(plotdat[,c("time","dosed")])

#for each time/administered solution intersect we calculate standard deviation and mean value
sds <- c()
means <- c()
for(row in rownames(time_dosed_combo)){
  day <- as.character(time_dosed_combo[row,"time"])
  administered_solution <- as.character(time_dosed_combo[row,"dosed"])
  
  #we find the rows in the plot data that administered 
  correct_rows <- plotdat[["time"]]==day & plotdat[["dosed"]]==administered_solution
  #gets the PFU/g measures from those rows and log10 transform the data
  measure <- plotdat[correct_rows,"measure"]
  measure <- log(measure,10)
  
  #we calculate the means and standard deviations and add the data
  means <- c(means,mean(measure,na.rm = T))
  sds <- c(sds,sd(measure,na.rm = T))
  
}

#we combine the data we just calcualted for plotting
plotdat_means_sds <- cbind(time_dosed_combo,'sds'=sds,'means'=means,'lower_bound'=means-sds,'upper_bound'=means+sds)

#we reformat a little to make the data more ggplot friendly
plotdat_means_sds <- as.data.frame(plotdat_means_sds)
plotdat_means_sds[["dosed"]] <- factor(plotdat_means_sds[["dosed"]],labels = c('SNIPR001','Vehicle'))

#we convert the days to numeric
plotdat_means_sds[["time"]] <- as.numeric(gsub('Day ','',as.character(plotdat_means_sds[["time"]])))

#sets the LOD
LOD=3.30E+01

#we set the y-scale breaks
breaks_y <- c(seq(0,max(ceiling(plotdat_means_sds[["upper_bound"]]))),log(LOD,10))
breaks_y_labels <- c(to_tenth_power_labels(seq(0,max(ceiling(plotdat_means_sds[["upper_bound"]])))),'LOD')

#and x-axis breaks
breaks_x <- plotdat_means_sds[plotdat_means_sds[["dosed"]]=='Vehicle',"time"]
labels_x <- breaks_x
labels_x[length(labels_x)] <- paste0(labels_x[length(labels_x)],'\ndays')


#we reformat the time
plotdat[["time"]] <- as.numeric(gsub('Day ','',as.character(plotdat[["time"]])))
plotdat[["dosed"]] <- gsub('Phage','SNIPR001',plotdat[["dosed"]])

#we want to slightly offset the green points so we can more easily see the green points that are below LOD 

plotdat_means_sds[plotdat_means_sds[["dosed"]]=="SNIPR001","time"] <- plotdat_means_sds[plotdat_means_sds[["dosed"]]=="SNIPR001","time"]+0.1
plotdat[plotdat[["dosed"]]=="SNIPR001","time"] <- plotdat[plotdat[["dosed"]]=="SNIPR001","time"]+0.1



p <- ggplot(data = plotdat_means_sds)#starts the plot
p <- p+geom_line(mapping = aes(x=time,y=means,color=dosed),size=0.5) #adds the lines connecting the dots
p <- p+geom_point(data = plotdat,mapping = aes(x=time,y=log(measure,10),color=dosed),size=0.5)#adds the dot points with standard deviations
p <- p+scale_x_continuous(breaks=breaks_x,labels = labels_x)#changes the x axis scales
p <- p+scale_y_continuous(breaks=breaks_y,labels=breaks_y_labels)#and the y axis
p <- p+geom_hline(yintercept=log(LOD,10),linetype="dashed", size=0.7)#adds the line for the LOD
p <- p+custom_theme+theme(legend.title = element_blank(),
                          legend.position = c(0.75,0.85),
                          legend.box.background = element_rect(colour = "black"),
                          axis.text = element_text(size=text_size),
                          legend.text = element_text(size=text_size))#we mess around with the theme
p <- p+ylab('SNIPR001 in feces (PFU/g)')+xlab('')
p <- p+scale_color_manual(values = c(SNIPR_green,SNIPR_grey))#sets the colors

#saves the figure
ggsave(p,filename = paste0(fig_folder,'Figure 6A.pdf'),width=3,height=4)
ggsave(p,filename = paste0(fig_folder,'Figure 6A.png'),width=3,height=4)

#we output the summary stats
outmat <- plotdat_means_sds[,c("time","dosed","sds","means")]

#we want to add the group sizes

n_vehicle <- sum(grepl('Vehicle',colnames(plotdat_unformatted)))
n_phage <- sum(grepl('Phage',colnames(plotdat_unformatted)))

group_size <- rep(n_vehicle,nrow(outmat))
group_size[outmat[["dosed"]]=='SNIPR001'] <- n_phage

outmat <- cbind(outmat,'group size'=group_size)

openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'figure6a.xlsx'))

###########
#Figure 6B#
###########
#we load the unformatted data for the Mini pig experiment for reformatting
plotdat_unformatted <- as.data.frame(read_excel(paste0(data_folder,"Pig kinetic.xlsx")))

#goes through each timepoint and gatheres the relevant data
plotdat_reformatted <- c()
for(time in unique(plotdat_unformatted[["Time after PO dosing (Days)"]])){
  #gets the PFU data
  PFU_data <- as.vector(plotdat_unformatted[plotdat_unformatted[["Time after PO dosing (Days)"]]==time,seq(2,ncol(plotdat_unformatted))])
  
  #extracts the phage names of the corresponding cols
  phage_vect <- unlist(strsplit(colnames(PFU_data),' replicate'))[seq(1,(ncol(plotdat_unformatted)-1)*2,2)]
  
  #Adds the data to reformatted plot data
  PFU_data <- unlist(PFU_data)
  PFU_data[PFU_data=='NA'] <- NA
  PFU_data <- as.numeric(PFU_data)
  plotdat_reformatted <- rbind(plotdat_reformatted,cbind('time'=rep(time,length(phage_vect)),'phage'=phage_vect,'measure'=PFU_data))
}


#changes the "alphas" to actual alphas
plotdat_reformatted[,"phage"] <- gsub('Alpha ','α',plotdat_reformatted[,"phage"])

#sets the format to data frame
plotdat_reformatted <- as.data.frame(plotdat_reformatted)
plotdat_reformatted[,"phage"] <- factor(plotdat_reformatted[,"phage"])

#ensures the formatting of numerical data
plotdat_reformatted[,"time"] <- as.numeric(plotdat_reformatted[,"time"])
plotdat_reformatted[,"measure"] <- as.numeric(plotdat_reformatted[,"measure"])

#Sets the limit of detection that we would like to include as a dotted line
LOD=log(3.30E+01,10)

#we need the lines to be the mean and the points to be the indiviudal points
unique_combinations <- unique(plotdat_reformatted[,c("time","phage")])
mean_dat <- cbind(unique_combinations,"mean_val"=rep(NA,nrow(unique_combinations)),'sd'=rep(NA,nrow(unique_combinations)))
for(row in rownames(unique_combinations)){
  #we get the time and the phage names
  time <- unique_combinations[row,"time"]
  phage <- unique_combinations[row,"phage"]
  
  #gets the data for that particular time/phage intersection
  measures <- plotdat_reformatted[plotdat_reformatted[["time"]]==time & plotdat_reformatted[["phage"]]==phage,"measure"]
  
  #we log10 transform the PFU measurements
  measures <- log(measures,10)
  
  #calculates the mean value and the standard deviations
  mean_dat[row,"mean_val"] <- mean(measures,na.rm = T)
  mean_dat[row,"sd"] <- sd(measures,na.rm = T)
}

mean_dat <- cbind(mean_dat,'upper_sd'=mean_dat[,"mean_val"]+mean_dat[,"sd"])
mean_dat <- cbind(mean_dat,'lower_sd'=mean_dat[,"mean_val"]-mean_dat[,"sd"])
mean_dat <- mean_dat[rowSums(is.na(mean_dat))==0,]

#sets the y-breaks
y_breaks <- c(seq(-100,100),LOD)
y_labels <- c(to_tenth_power_labels(seq(-100,100)),'LOD    ')
#and the x-breaks
x_breaks <- unique(mean_dat[["time"]])

#starts the plot
p <- ggplot(data = mean_dat,mapping = aes(x=time,y=mean_val,color=phage))
#adds the LOD line
p <- p + geom_hline(yintercept=LOD,linetype="dashed", size=1)
#adds the line
p <- p + geom_line()
#and points with the range of indicated by SDs
p <- p + geom_point(data = plotdat_reformatted,mapping = aes(x=time,y=log(measure,10),color=phage))
#sets the color scale to be consistent with other plots
p <- p + scale_color_manual(values = phage_colors)
#and divides into facets based on administered phage
p <- p + facet_grid(cols=vars(phage))
#sets the axis breaks 
p <- p+scale_y_continuous(breaks=y_breaks,labels = y_labels,limits = c(-0.2,10))
p <- p+scale_x_continuous(breaks=x_breaks)
#sets our custom theme and makes some ammendments
p <- p+custom_theme+theme(plot.tag.position = 'bottomright',
                          plot.tag = element_text(size=text_size,vjust=0,hjust=0,margin=margin(t = -8.4,l=2,0,0,'pt')),
                          strip.background.x = element_rect(fill='#FFFFFF'),
                          strip.background.y = element_rect(fill='#FFFFFF'),
                          strip.text.y=element_text(angle=0),
                          legend.title = element_blank(),
                          plot.title = element_text(size=text_size,hjust=0.5))

p <- p + ylab('CAP recovery in feces (PFU/g)')+xlab('Days')
#and saves the plot
ggsave(plot=p,filename = paste0(fig_folder,'Figure 6B.pdf'),device=cairo_pdf,height=4,width=7)
ggsave(plot=p,filename = paste0(fig_folder,'Figure 6B.png'),device=cairo_pdf,height=4,width=7)

#we wish to output summary stats
output <- mean_dat[,c("time","phage","mean_val","sd")]

n15.2 <- sum(grepl('Alpha 15.2',colnames(plotdat_unformatted)))
n51.5 <- sum(grepl('Alpha 51.5',colnames(plotdat_unformatted)))
n48.4 <- sum(grepl('Alpha 48.4',colnames(plotdat_unformatted)))
n20.4 <- sum(grepl('Alpha 20.4',colnames(plotdat_unformatted)))


group_sizes <- rep(n15.2,nrow(output))
group_sizes[output[["phage"]]=="α48.4"] <- n48.4
group_sizes[output[["phage"]]=="α51.5"] <- n51.5
group_sizes[output[["phage"]]=="α20.4"] <- n20.4

output <- cbind(output,'group size'=group_sizes)


openxlsx::write.xlsx(output,file = paste0(summary_stats_folder,'figure6b.xlsx'))



###########
#Figure 6C#
###########
#reads the plot data, which is already in a ggplot friendly format
plotdat <- read_excel(paste0(data_folder,"Mouse efficacy_Phage.xlsx"))
#I prefer underscores to spaces and slashes so I implement that
colnames(plotdat) <- gsub('/','_',gsub(' ','_',colnames(plotdat)))
plotdat <- as.data.frame(plotdat)

#We want to use colors to display whether SNIPR001 or something else was administered, so we add a column indicating that
plotdat <- cbind(plotdat,'SNIPR001'=grepl('SNIPR001',plotdat[["administered_solution"]]))

#we wish for things to be plotted in a logical order, so we change the levels for the administered solution
plotdat[["administered_solution"]] <- factor(plotdat[["administered_solution"]],levels=c("Vehicle",'SNIPR001 High','SNIPR001 Medium','SNIPR001 Low','Gentamicin'))
#we also give the time-variable some slightly more informative names
plotdat[["hours_after_dosage"]] <- factor(plotdat[["hours_after_dosage"]],levels = sort(unique(plotdat[["hours_after_dosage"]])),labels=c('Day 2\n8 Hours\nAfter treatment\n(1 dose)','Day 3\n24 Hours\nAfter treatment\n(3 doses)','Day 4\n48 Hours\nAfter treatment\n(6 doses)'))


#we set the limit of detection
LOD=3.71E+02

#we se the y axis breaks
y_breaks <- c(10^seq(0,10),LOD)
y_break_labels <- c(to_tenth_power_labels(seq(0,10)),'LOD     ')

#we would like to create a dotplot with another plot below the dot-plot that indicates which thing was admininistered in a less text-heavy way.
#first we create the dot-plot
dotplot <- ggplot(data=plotdat,aes(x=administered_solution,y=PFU_g_feces))+geom_jitter(aes(color=SNIPR001),width=0.1,size=1) #we add the points to the dotplot
dotplot <- dotplot+scale_y_log10(breaks=y_breaks,labels=y_break_labels)#introduces a log scale on the y axis with custom breaks
dotplot <- dotplot+facet_wrap(~hours_after_dosage) #wraps it around the time since dosage
dotplot <- dotplot+custom_theme+theme(legend.position = 'none',strip.text = element_text(size=text_size),axis.title.y=element_text(margin = margin(r = -20,t=-10))) #sets theme presets, removes the color legend, sets the facet text
dotplot <- dotplot+scale_color_manual(values = c(SNIPR_grey,SNIPR_green)) #changes the color scheme
dotplot <- dotplot+stat_summary(fun = mean, fun.min = mean, fun.max = mean,geom = "crossbar", width = 0.7)#adds a black bar to indicate the mean value
dotplot <- dotplot+xlab('')+ylab('Total phages in feces (PFU/g)')#changes the axxis labels
dotplot <- dotplot + geom_hline(yintercept=LOD,linetype="dashed", size=1)#adds the line and LOD label
dotplot <- dotplot + theme(axis.title.y=element_text(margin = margin(r = 0)),axis.text.x.bottom=element_text(angle=50,vjust=1,hjust=1),plot.margin = margin(0,l=0,0,0,'cm'),strip.text = element_text(size=text_size))#fixes the axis titles and plot margin

ggsave(plot = dotplot,filename = paste0(fig_folder,'Figure 6C.pdf'),width=4,height=4,device=cairo_pdf)
ggsave(plot = dotplot,filename = paste0(fig_folder,'Figure 6C.png'),width=4,height=4)

#and a matrix to output
unique_dat <- unique(plotdat[,c("hours_after_dosage","administered_solution")])
outmat <- c()
for(row in rownames(unique_dat)){
  time <- as.character(unique_dat[row,"hours_after_dosage"])
  solution <- as.character(unique_dat[row,"administered_solution"])
  
  datapoints <- log(plotdat[plotdat[["hours_after_dosage"]]==time & plotdat[["administered_solution"]]==solution,"PFU_g_feces"],10)
  
  outmat <- rbind(outmat,cbind(
    'time'=gsub('\n',' ',time),
    'administered_solution'=solution,
    'mean (log10 PFU/g)'=mean(datapoints),
    'sd (log10 PFU/g)'=sd(datapoints),
    'n'=length(datapoints)
  ))
}


openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'figure6c.xlsx'))


###########
#Figure 6D#
###########
#reads the plot data, which is already in a ggplot reasonably friendly format
plotdat <- as.data.frame(read_excel(paste0(data_folder,"Mouse efficacy_E.coli recovery.xlsx")))

#We wish to color-code the SNIPR0001 points from the non-SNIPR point. We create a column to indicate if SNIPR001 was administered
plotdat <- cbind(plotdat,'SNIPR001'=grepl('SNIPR001',plotdat[["administered solution"]]))

#The test subjects that would later be administered SNIPR001 are also colored green for pre-administration time-points. We set those to FALSE as well
plotdat[plotdat[["Hours since dose"]]<0,"SNIPR001"] <- F

#to simplify plotting we change the column names slightly
colnames(plotdat) <- gsub('/','_',gsub(' ','_',colnames(plotdat)))

#we change the order of the dosage points to make it more intuitive and make the averages have a more visually pleasing flow
plotdat[["administered_solution"]] <- factor(plotdat[["administered_solution"]],levels=c("Vehicle",'SNIPR001 High','SNIPR001 Medium','SNIPR001 Low','Gentamicin'))

#we wish to make our facet headers slightly more informative than simply relative time to first dosage, so we change the labels
plotdat[["Hours_since_dose"]] <- factor(plotdat[["Hours_since_dose"]],levels = sort(unique(plotdat[["Hours_since_dose"]])),labels=
                                          c('Day 1\nBefore dose',
                                            'Day 2\nBefore dose',
                                            'Day 2\n8 hours after\nfirst dose\n(1 dose)',
                                            'Day 3\n24 hours after\nfirst dose\n(3 doses)',
                                            'Day 4\n48 hours after\nfirst dose\n(6 doses)'))

#we customize the y axis albels to a more legible standard
y_breaks <- c(10^seq(1,10),LOD)
y_break_labels <- c(to_tenth_power_labels(seq(1,10)),'LOD     ')

#we would like to compare certain points and indicate significance. We create a seperate data object for that
signif_data <- c()
#we only wish to compare the datapoints after dosage, since pre-dosage comparisons are not meaningful
post_dose_days <- levels(plotdat[["Hours_since_dose"]])[!grepl('Before',levels(plotdat[["Hours_since_dose"]]))]

#the the significance values should be on top of other points we use, so we get the current y variable
current_y=max(log(plotdat[["CFU_mL"]],10))

#we get the SNIPR001-related solutions for later use
SNIPR001_solutions <- levels(plotdat[["administered_solution"]])[grepl('SNIPR',levels(plotdat[["administered_solution"]]))]

#we adjust the p-value separately for each day
adjusted_pvals <- c()
for(day in post_dose_days){
  #we get the vehicle concentration, which SNIPR001 should be compared to
  vehicle_conc <- plotdat[plotdat[["Hours_since_dose"]]==day & plotdat[["administered_solution"]]=='Vehicle',"CFU_mL"]
  
  for(dosed in SNIPR001_solutions){
    #we get the concentration of SNIPR001 at this point
    SNIPR_conc <- plotdat[plotdat[["Hours_since_dose"]]==day & plotdat[["administered_solution"]]==dosed,"CFU_mL"]
    #then we carry out the Wilcoxon test and add relevant data to signif_data
    stats <- wilcox.test(vehicle_conc,SNIPR_conc)
    signif_data <- rbind(signif_data,
                         cbind('Hours_since_dose'=day,
                               'start'='Vehicle',
                               'end'=dosed,
                               'y'=current_y+0.7*match(dosed,SNIPR001_solutions), #we want the y-axis coordinate to be different depending on the comparison so they don't stack annoyingly
                               'pvalue'=stats[["p.value"]]))
  }
  #for a given day we compute the adjusted p-values using p.adjust
  adjusted_pvals <- c(adjusted_pvals,p.adjust(as.numeric(signif_data[signif_data[,"Hours_since_dose"]==day,"pvalue"])))
}

#we set significance asterices depending on the adjusted p-value, *:<0.05,**:<0.01,***:<0.001
text <- rep('NS',nrow(signif_data))
text[adjusted_pvals<0.05] <- '*'
text[adjusted_pvals<0.01] <- '**'
text[adjusted_pvals<0.001] <- '***'

#we add the labels
signif_data <- as.data.frame(cbind(signif_data,'label'=text,'p_adj'=adjusted_pvals))

#we also wish to show that the low, medium, and high dosages are different from one-another by a Kruskal Wallis test.

#so we add that
KW_pvalues <- c()
for(day in post_dose_days){
  #we extract the data we need
  testdata <- plotdat[plotdat[["Hours_since_dose"]]==day & grepl("SNIPR001",plotdat[["administered_solution"]]),c("CFU_mL","administered_solution")]
  #and we perform the kruskal-wallis test
  kruskal_results <- kruskal.test(CFU_mL~administered_solution,data=testdata)
  #and save the p-value 
  KW_pvalues <- c(KW_pvalues,kruskal_results[["p.value"]])
}
#we adjust the significance
KW_pvalues_adj <- p.adjust(KW_pvalues)
text <- rep('NS',length(KW_pvalues))
text[KW_pvalues<0.05] <- '*'
text[KW_pvalues<0.01] <- '**'
text[KW_pvalues<0.001] <- '***'

names(KW_pvalues) <- post_dose_days
#since we wish to indicate we are comparing all SNIPR administered populations we 
#first we add the line between the low and high dose
signif_data <- rbind(signif_data,
                     cbind("Hours_since_dose"=post_dose_days,"start"="SNIPR001 Low","end"="SNIPR001 High",y=current_y+0.7*4,"pvalue"=KW_pvalues,"label"=text,'p_adj'=KW_pvalues_adj))

#since we wish to indicate that we compare both low, medium, and high, we add a comparison between medium and low at the same height to make it look like a three-pronged comparison
signif_data <- rbind(signif_data,
                     cbind("Hours_since_dose"=post_dose_days,"start"="SNIPR001 Medium","end"="SNIPR001 High",y=current_y+0.7*4,"pvalue"=KW_pvalues,"label"=rep("",length(post_dose_days)),'p_adj'=KW_pvalues_adj))


#we set the limit of detection for the plot
LOD=3.71E+02
#we set the y axis breaks
y_breaks <-c(10^seq(0,ceiling(current_y)),LOD)
y_break_labels <-c(to_tenth_power_labels(seq(0,ceiling(current_y))),"LOD      ")


#we make sure the order of facets matches between the two datasets
signif_data[["Hours_since_dose"]] <- factor(signif_data[["Hours_since_dose"]],levels=levels(plotdat[["Hours_since_dose"]]))
#then we set the y variable to be numeric
signif_data[["y"]] <- as.numeric(signif_data[["y"]])

p <- ggplot(data=plotdat,aes(x=administered_solution,y=CFU_mL))+geom_jitter(aes(color=SNIPR001),width = 0.3,size=1)+scale_color_manual(values = c("FALSE"=SNIPR_grey,"TRUE"=SNIPR_green)) #starts the points in green
p <- p+scale_y_log10(breaks=y_breaks,labels=y_break_labels,limits=c(0.99,10^ceiling(max(signif_data[["y"]]))))#introduces a log scale
p <- p+custom_theme #sets theme presets
p <- p+theme(legend.position = 'none',strip.text.x = element_text(size=text_size))#removes the legend, turns the x axis labels
p <- p+stat_summary(fun = mean, fun.min = mean, fun.max = mean,geom = "crossbar", width = 0.5)#adding a mean bar
p <- p+xlab('')+ylab(expression(paste('Total ',italic('E. coli'),' count (CFU/g)')))#changes the axxis labels
p <- p + geom_hline(yintercept=LOD,linetype="dashed", size=1)#adds the line and LOD label
p <- p+theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1))
#this gives off a warning saying it will ignore every aesthetic input, but without the things it claims to ignore, nothing works, soooo... It does seem to use them
p <- p+ggsignif::geom_signif(data=signif_data,aes(xmin=start,xmax=end,annotations=label,y_position=y),manual=T,textsize=3,vjust=0.1)
p <- p+facet_grid(~Hours_since_dose) #wraos it around the time since dosage


ggsave(p,filename = paste0(fig_folder,'Figure 6D.pdf'),width=8,height=4)
ggsave(p,filename = paste0(fig_folder,'Figure 6D.png'),width=8,height=4)



#and a matrix to output
unique_dat <- unique(plotdat[,c("Hours_since_dose","administered_solution")])
outmat <- c()
for(row in rownames(unique_dat)){
  time <- as.character(unique_dat[row,"Hours_since_dose"])
  solution <- as.character(unique_dat[row,"administered_solution"])
  
  datapoints <- log(plotdat[plotdat[["Hours_since_dose"]]==time & plotdat[["administered_solution"]]==solution,"CFU_mL"],10)
  
  outmat <- rbind(outmat,cbind(
    'time'=gsub('\n',' ',time),
    'administered_solution'=solution,
    'mean (log10 PFU/g)'=mean(datapoints),
    'sd (log10 PFU/g)'=sd(datapoints),
    'n'=length(datapoints)
  ))
}

openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'figure6d.xlsx'))




###########
#Figure 6E#
###########
plotdat <- as.data.frame(read_excel(paste0(data_folder,"mouse_cfu_data.xlsx")))

#we set the order of the x variables
plotdat[["dosed"]] <- factor(plotdat[["dosed"]],levels = c("Vehicle","SNIPR001",SNIPR001_phages,'Gentamicin'))

#we reformat the time variables and set the order
plotdat[["time"]] <- gsub('\\\\n','\n',plotdat[["time"]])
plotdat[["time"]] <- factor(plotdat[["time"]],levels = unique(plotdat[["time"]]))

#we wish to show all pre-dosage timepoint as grey
color_var <- rep("Grey",nrow(plotdat))

#we set the colors to be used in this plot
local_color_scale <- c(phage_colors,'Vehicle'=phage_colors[["Control"]],'Gentamicin'=phage_colors[["Control"]],'Grey'=SNIPR_grey)

#we set the after timepoints to be not grey
after_timepoints <- grepl('after',plotdat[["time"]])
color_var[after_timepoints] <- as.character(plotdat[after_timepoints,"dosed"])

#we add the color variable to the dataset
plotdat <- cbind(plotdat,'color_var'=color_var)

LOD <- 300

#we set the y-axis breaks
y_breaks <- c(10^seq(-20,max(ceiling(log(plotdat[["CFU_g"]],10)),na.rm = T),1),LOD)
y_labels <- c(to_tenth_power_labels(seq(-20,max(ceiling(log(plotdat[["CFU_g"]],10)),na.rm = T),1)),'LOD   ')

#we carry out the statistical tests after administartion started
signif_data <- data.frame()
#we set the y-variable location keeping in mind it's all in log scale
bar_position <- max(plotdat[,"CFU_g"],na.rm = T)*10
increment=5
for(timepoint in levels(plotdat[["time"]])[grepl('after',levels(plotdat[["time"]]))]){
  current_y <- bar_position
  #we compare the non-vehicles to vehicle
  vehicle_CFUs <- plotdat[plotdat[["time"]]==timepoint & plotdat[["dosed"]]=="Vehicle","CFU_g"]
  for(dosed_item in levels(plotdat[["dosed"]])[levels(plotdat[["dosed"]])!='Vehicle']){
    #we carry out the test
    dosed_CFUs <- plotdat[plotdat[["time"]]==timepoint & plotdat[["dosed"]]==dosed_item,"CFU_g"]
    result <- wilcox.test(vehicle_CFUs,dosed_CFUs)
    
    #and we save the data
    signif_data <- rbind(signif_data,data.frame('pval'=result[["p.value"]],'x_start'='Vehicle','x_end'=dosed_item,"y"=current_y,'time'=timepoint))
    current_y <- current_y*increment
  }
}

#we fdr adjust
adjusted_pvals <- signif_data %>% group_by(time) %>% summarise('adjusted_pval'=p.adjust(pval))
signif_data <- cbind(signif_data,'adjusted_pval'=adjusted_pvals[["adjusted_pval"]])

signif_data <- signif_data[signif_data[["adjusted_pval"]]<0.05,]

#we set the stars
stars <- rep('NS',nrow(signif_data))
stars[signif_data[["adjusted_pval"]]<0.05] <- '*'
stars[signif_data[["adjusted_pval"]]<0.01] <- '**'
stars[signif_data[["adjusted_pval"]]<0.001] <- '***'

signif_data <- cbind(signif_data,'stars_labels'=stars)
signif_data[["time"]] <- factor(signif_data[["time"]],levels = levels(plotdat[["time"]]))

#to do: add mean bars!
#we calculate mean bars
meanvals <- plotdat %>% group_by(dosed,time) %>% summarise("mean"=10^mean(log(CFU_g,10),na.rm=T))
meanvals <- as.data.frame(meanvals)

p <- ggplot()+geom_jitter(plotdat,mapping = aes(x=dosed,y=CFU_g,color=color_var),width=0.15)+facet_grid(cols=vars(time))
p <- p + scale_color_manual(values=local_color_scale)
p <- p + scale_y_continuous(name = expression(paste(italic('E. coli'),' count (CFU/g)')),trans = 'log10',breaks = y_breaks,labels = y_labels)+geom_hline(yintercept = LOD,linetype='dashed')
p <- p + custom_theme+theme(legend.position = 'none',axis.text.x=element_text(angle=50,vjust=1,hjust=1),axis.title.x=element_blank())
p <- p + ggsignif::geom_signif(data = signif_data,
                               mapping = aes(xmin=x_start,xmax=x_end,y_position=log(y,10),annotations=stars_labels),manual=T,textsize=4,vjust=0.3)
p <- p+geom_crossbar(data = meanvals,aes(ymin=mean,ymax=mean,xmin=dosed,xmax=dosed,x=dosed,y=mean),width=0.4)
ggsave(p,filename = paste0(fig_folder,"Figure 6E.pdf"),width=6,height=4,device = cairo_pdf)
ggsave(p,filename = paste0(fig_folder,"Figure 6E.png"),width=6,height=4,device = cairo_pdf)

#we output the summary statistics
summary_output <- plotdat %>% group_by(dosed,time) %>% summarise('log10_mean'=mean(log(CFU_g,10),na.rm=T),'log10_sd'=sd(log(CFU_g,10),na.rm=T))
write.xlsx(summary_output,paste0(summary_stats_folder,'figure6e_summary_stats.xlsx'))

########################
#Supplementary figure 3#
########################
#we load the data
plotdat_unformatted <- read_excel(paste0(data_folder,"Killing efficiency of target E. coli strain by conjugation of CGV-EcCas.xlsx"))

#We would like to use averages and standard deviations to illustrate multiple replicates, so we get each unique combination of phage and bacteria 
plotdat <- unique(plotdat_unformatted[,c("conjugate","bact")])
means <- c()
sds <- c()
for(row in rownames(plotdat)){
  #we find out which rows in the unformatted plot data contain the data we are interested in
  correct_rows <- as.character(plotdat[row,"conjugate"])==plotdat_unformatted[["conjugate"]] & as.character(plotdat[row,"bact"])==plotdat_unformatted[["bact"]]
  #then we get the correct CFU/mL values and log transform them
  vals <- plotdat_unformatted[["cfu/ml"]][correct_rows]
  #for graphical purposes we set 0's to be 1's as to not create -Inf when log transforming
  vals[vals==0] <- 1
  vals <- log(vals,10)
  #we then append the means and standard deviations
  means <- c(means,mean(vals))
  sds <- c(sds,sd(vals))
}
#and we attach the means and standard deviatias to the plotdata, as well as upper and lower bounds of the standard deviation in the plot
plotdat <- cbind(plotdat,'means'=means,'sds'=sds,'lower_bound'=means-sds,'upper_bound'=means+sds)

#we make the plotdata into something more ggplot friendly
plotdat <- as.data.frame(plotdat)

#we create custom y-axis breaks for 
y_breaks <- seq(floor(min(plotdat[["lower_bound"]])),ceiling(max(plotdat[["upper_bound"]])))
y_labels <- to_tenth_power_labels(y_breaks)


point_data <- as.data.frame(plotdat_unformatted)
colnames(point_data) <- c('conjugate','bact','cfu','replicate')

point_data[["cfu"]] <- log(point_data[["cfu"]],10)
point_data[point_data[["cfu"]]==-Inf,"cfu"] <- 0
#first we create the actual barplot
#we initialize plot
p <- ggplot()
#we add bars and errorbars
p <- p+geom_bar(data=plotdat,mapping = aes(x=bact,y=means,color=conjugate),size=1.5,stat='identity',position = 'dodge2',fill='#FFFFFF')
#adds the points
p <- p+geom_point(data=point_data[point_data[["conjugate"]]=='Empty vector',],mapping = aes(x=bact,y=cfu,color=conjugate),position =position_nudge(x=+0.225))
p <- p+geom_point(data=point_data[point_data[["conjugate"]]!='Empty vector',],mapping = aes(x=bact,y=cfu,color=conjugate),position =position_nudge(x=-0.225))
#we set our theme
p <- p+custom_theme+ylab('CFU/mL')
#and customize it a bit to prepare it for having another explanatory plot added in the bottom of it
p <- p+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin = margin(b=0),axis.title.x=element_blank(),legend.title = element_blank())+
  scale_color_manual(values=c("EcCAS"=EcCAS_color,SNIPR_color1))
#adds customized y-axis breaks
p <- p+scale_y_continuous(breaks=y_breaks,labels = y_labels)

#the important difference between the two tested bacteria, b52 and b2361, is that b2361 does not have the binding motifs our conjugate uses, while b52 does.
#in order to communicate this more clearly we will, aside from 
textdat <- plotdat[,c("conjugate","bact")]
#we make the b52's have +'s and the b2361's have -'s to indicate this difference
text <- rep('-',nrow(textdat))
text[textdat[,"bact"]=='b52'] <- '+'
textdat <- cbind(textdat,text,'y'=rep('Target sequence',nrow(textdat)))
textdat <- as.data.frame(textdat)

#Since the bars are in duplicate and I can't figure out a smarter way to do this, we will simply shift the text slightly to the left and right so it appears under the bars
offsets <- 0.225
#we create a simple text-plott o allow for easier reading of the bar chart metadata
#first we start the plot
plusminus_plot <- ggplot(data = textdat,mapping = aes(x=bact,y=y,label=text))
#adds the text on the left
plusminus_plot <- plusminus_plot + geom_text(position = position_nudge(x=-offsets),size=4)
#adds the text to the right
plusminus_plot <- plusminus_plot + geom_text(position = position_nudge(x=offsets),size=4)
#sets a very minimal theme
plusminus_plot <- plusminus_plot + custom_theme + theme(panel.grid.major.y = element_blank(),plot.margin = margin(t=-2.5),axis.ticks.y = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.grid.major.x = element_blank(),panel.border = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ylab('Target sequence')

#we combine the plots and align them
comboplot <- cowplot::plot_grid(p,plusminus_plot,ncol=1,rel_heights=c(90,5),align='v',axis = 'lr')
#we then save the combination plot
ggsave(plot = comboplot,filename = paste0(fig_folder,'Supplementary figure 3.pdf'),width=4,height=3)
ggsave(plot = comboplot,filename = paste0(fig_folder,'Supplementary figure 3.png'),width=4,height=3)


#we output some summary stats
outmat <- plotdat[,c("conjugate","bact","means","sds")]
#everything is done in replicates of tow
outmat <- cbind(outmat,'replicates'=2)

openxlsx::write.xlsx(x = outmat,file = paste0(summary_stats_folder,'supplementary figure 3.xlsx'))


########################
#Supplementary figure 3#
########################
#we load the data
plotdat_unformatted <- read_excel(paste0(data_folder,"Killing efficiency of target E. coli strain by conjugation of CGV-EcCas.xlsx"))

#We would like to use averages and standard deviations to illustrate multiple replicates, so we get each unique combination of phage and bacteria 
plotdat <- unique(plotdat_unformatted[,c("conjugate","bact")])
means <- c()
sds <- c()
for(row in rownames(plotdat)){
  #we find out which rows in the unformatted plot data contain the data we are interested in
  correct_rows <- as.character(plotdat[row,"conjugate"])==plotdat_unformatted[["conjugate"]] & as.character(plotdat[row,"bact"])==plotdat_unformatted[["bact"]]
  #then we get the correct CFU/mL values and log transform them
  vals <- plotdat_unformatted[["cfu/ml"]][correct_rows]
  #for graphical purposes we set 0's to be 1's as to not create -Inf when log transforming
  vals[vals==0] <- 1
  vals <- log(vals,10)
  #we then append the means and standard deviations
  means <- c(means,mean(vals))
  sds <- c(sds,sd(vals))
}
#and we attach the means and standard deviatias to the plotdata, as well as upper and lower bounds of the standard deviation in the plot
plotdat <- cbind(plotdat,'means'=means,'sds'=sds,'lower_bound'=means-sds,'upper_bound'=means+sds)

#we make the plotdata into something more ggplot friendly
plotdat <- as.data.frame(plotdat)

#we create custom y-axis breaks for 
y_breaks <- seq(floor(min(plotdat[["lower_bound"]])),ceiling(max(plotdat[["upper_bound"]])))
y_labels <- to_tenth_power_labels(y_breaks)


point_data <- as.data.frame(plotdat_unformatted)
colnames(point_data) <- c('conjugate','bact','cfu','replicate')

point_data[["cfu"]] <- log(point_data[["cfu"]],10)
point_data[point_data[["cfu"]]==-Inf,"cfu"] <- 0
#first we create the actual barplot
#we initialize plot
p <- ggplot()
#we add bars and errorbars
p <- p+geom_bar(data=plotdat,mapping = aes(x=bact,y=means,color=conjugate),size=1.5,stat='identity',position = 'dodge2',fill='#FFFFFF')
#adds the points
p <- p+geom_point(data=point_data[point_data[["conjugate"]]=='Empty vector',],mapping = aes(x=bact,y=cfu,color=conjugate),position =position_nudge(x=+0.225))
p <- p+geom_point(data=point_data[point_data[["conjugate"]]!='Empty vector',],mapping = aes(x=bact,y=cfu,color=conjugate),position =position_nudge(x=-0.225))
#we set our theme
p <- p+custom_theme+ylab('CFU/mL')
#and customize it a bit to prepare it for having another explanatory plot added in the bottom of it
p <- p+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin = margin(b=0),axis.title.x=element_blank(),legend.title = element_blank())+
  scale_color_manual(values=c("EcCAS"=EcCAS_color,SNIPR_color1))
#adds customized y-axis breaks
p <- p+scale_y_continuous(breaks=y_breaks,labels = y_labels)

#the important difference between the two tested bacteria, b52 and b2361, is that b2361 does not have the binding motifs our conjugate uses, while b52 does.
#in order to communicate this more clearly we will, aside from 
textdat <- plotdat[,c("conjugate","bact")]
#we make the b52's have +'s and the b2361's have -'s to indicate this difference
text <- rep('-',nrow(textdat))
text[textdat[,"bact"]=='b52'] <- '+'
textdat <- cbind(textdat,text,'y'=rep('Target sequence',nrow(textdat)))
textdat <- as.data.frame(textdat)

#Since the bars are in duplicate and I can't figure out a smarter way to do this, we will simply shift the text slightly to the left and right so it appears under the bars
offsets <- 0.225
#we create a simple text-plott o allow for easier reading of the bar chart metadata
#first we start the plot
plusminus_plot <- ggplot(data = textdat,mapping = aes(x=bact,y=y,label=text))
#adds the text on the left
plusminus_plot <- plusminus_plot + geom_text(position = position_nudge(x=-offsets),size=4)
#adds the text to the right
plusminus_plot <- plusminus_plot + geom_text(position = position_nudge(x=offsets),size=4)
#sets a very minimal theme
plusminus_plot <- plusminus_plot + custom_theme + theme(panel.grid.major.y = element_blank(),plot.margin = margin(t=-2.5),axis.ticks.y = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.grid.major.x = element_blank(),panel.border = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ylab('Target sequence')

#we combine the plots and align them
comboplot <- cowplot::plot_grid(p,plusminus_plot,ncol=1,rel_heights=c(90,5),align='v',axis = 'lr')
#we then save the combination plot
ggsave(plot = comboplot,filename = paste0(fig_folder,'Supplementary figure 3.pdf'),width=4,height=3)
ggsave(plot = comboplot,filename = paste0(fig_folder,'Supplementary figure 3.png'),width=4,height=3)


#we output some summary stats
outmat <- plotdat[,c("conjugate","bact","means","sds")]
#everything is done in replicates of tow
outmat <- cbind(outmat,'replicates'=2)

openxlsx::write.xlsx(x = outmat,file = paste0(summary_stats_folder,'supplementary figure 3.xlsx'))


########################
#Supplementary figure 4#
########################
#we read the data on CAS3 expression between phages with and without CAS3
plotdat <- as.data.frame(read_excel(paste0(data_folder,"comparison_of_CAS3_expression_levels_of_WT_a15_vs_CAP_a15.2_RT-qPCR_assay.xlsx")))

#the first column has both phage, time, and bacteria information. since the bacteria is the same all across the column we remove that element and split b
phage_time_dat <- strsplit(plotdat[,1],' [(]b1419[)] ')
#now that we have the relevant info we delete the column again
plotdat <- plotdat[,-1]
#we add the phage and time data as two seperate columns and rename the columns to reflect this
plotdat <- cbind(plotdat,t(matrix(unlist(phage_time_dat),nrow=2)))
colnames(plotdat)[seq(ncol(plotdat)-1,ncol(plotdat))] <- c('phage','time')


#we wish to calculate the CAS3/GADPH expression for each of the three replicates
for(replicate in seq(1,3,1)){
  CAS3_col <- grepl(paste0('CAS3 ',replicate),colnames(plotdat))
  GAPDH_col <- grepl(paste0('GAPDH ',replicate),colnames(plotdat))
  ratio <- as.numeric(plotdat[,CAS3_col])/as.numeric(plotdat[,GAPDH_col])
  plotdat <- cbind(plotdat,ratio)
  colnames(plotdat)[ncol(plotdat)] <- paste0('CAS3/GAPDH ',replicate)
}


#we calculate the means and sds of the ratio between CAS3 and GAPDH
plotdat <- cbind(plotdat,'sds'=apply(plotdat[,grepl('CAS3/GAPDH',colnames(plotdat))],1,sd))
plotdat <- cbind(plotdat,'means'=rowMeans(plotdat[,grepl('CAS3/GAPDH',colnames(plotdat))]))

#we order the time-points to make the distance between columns reflect the distance between time-points
plotdat[["time"]] <- as.numeric(gsub(' min','',plotdat[["time"]] ))
#we replace the a in phage name with actual alphas
plotdat[["phage"]] <- gsub('a','α',plotdat[["phage"]]) 

x_breaks <- unique(plotdat[["time"]])
x_labels <- x_breaks
x_labels[x_labels==max(x_labels)] <- paste0(x_labels[x_labels==max(x_labels)],' min')

#we initialize the plot
p <- ggplot(data = plotdat,mapping = aes(x = time,y = means,fill=phage))
p <- p+geom_bar(stat='identity')+scale_fill_manual(values = c(phage_colors))#adds bars and colors them correctly
p <- p + geom_errorbar(aes(ymin=means-sds,ymax=means+sds),width=4)#adds the error bars
p <- p+facet_grid(cols = vars(phage))#divides tha a15 and a15.2
p <- p+scale_x_continuous(breaks=unique(plotdat[["time"]]),labels = x_labels)#sets the x axis labels to only reflect the time-poits we sampled
p <- p+custom_theme+theme(legend.position = 'none',panel.grid.minor = element_blank(),strip.text = element_text(size=text_size),axis.title.y=element_text(margin = margin(0,r=5,0,0,'pt')),panel.grid.major.y = element_line())#sets the theme
p <- p+ylab(substitute(paste(italic('cas3/gapA'), ' ratio',sep = ' ')))+xlab('Time since infection')#adds the y-label and x-label
#saves the plot
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 4.pdf'),device = cairo_pdf,width=3.5,height=2)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 4.png'),width=3.5,height=2)

#and we output some summary statistics
outmat <- cbind(plotdat[,c("phage","time","sds","means")],'replicates'=3)

openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'supplementary figure 4.xlsx'))



########################
#Supplementary figure 5#
########################
#This plot has the following components::
#1) a heatmap
#2) a taxonomic tree for vira that indictes the clusters we created
#3) MLST anotations for the bacterial panel
#4) a series of dots indicating the phages that were used in further development

#we start with the trees and the heatmap, since the order of phages and the tree is based on the data in the heatmap
#First we read the iAUC data
library(readxl)
iAUC_data <- as.data.frame(read_excel(paste0(data_folder,"iAUC_data_SNIPR_panel_CAP_screening.xlsx")))
rownames(iAUC_data) <- iAUC_data[,1]
iAUC_data <- iAUC_data[,-1]

#we average out the two runs
heatmap_data <- reshape2::melt(as.matrix(iAUC_data))
colnames(heatmap_data) <- c('bact','phage','iAUC')

#we remove the .1s that indicated that a column existed in duplicate
heatmap_data[,"phage"] <- gsub('[.]1','',as.character(heatmap_data[,"phage"]))

#we average the iAUCs
heatmap_data <- heatmap_data %>% group_by(bact,phage) %>% summarise(iAUC=mean(iAUC,na.rm=T))
heatmap_data <- as.data.frame(heatmap_data)

#we'll add some tip colors to indicate MLST
#we load MLSTs
SNIPR_MLST <- read.delim(paste0(data_folder,"SNIPR_panel_ecoli_mlst.tsv"),sep='\t')

#we reformat to a matrix format to get a tree order
iAUC_data <- reshape2::acast(heatmap_data,formula = list(names(heatmap_data)[1],names(heatmap_data)[2]))


#Now we cluster based on the distance between iAUC profiles for the bacteria and the phages
bact_dendrogram <- as.dendrogram(hclust(dist(scale(iAUC_data))))
phage_dendrogram <- as.dendrogram(hclust(dist(scale(t(iAUC_data)))))

#we make the trees which is used for ordering
phage_tree <- ggtree(phage_dendrogram)+coord_flip()+scale_x_reverse()+scale_y_reverse()
bact_tree <- ggtree(bact_dendrogram)

#the cut-off used to create the 4 clusters is made at this height
x_intercept_height <- 11.5

#we add a dotted line to indicate our four clusters
phage_tree <- phage_tree+geom_vline(xintercept = x_intercept_height,linetype='dashed',size=1)

#we get the order of bacteria and phages
bact_order <- na.omit(bact_tree[["data"]][order(bact_tree[["data"]][["y"]]),"label"])[[1]]
phage_order <- rev(na.omit(phage_tree[["data"]][order(phage_tree[["data"]][["y"]]),"label"])[[1]])#the tree will be flipped upside down so the order will need to be reversed

#we wish to add labels to indicate the the number of the cluster
#which means that we have to find the set of parent-child nodes that intersect the line we've drawn
#first we get a list of the nodes that are abive the line
node_data_above_line <- phage_tree[["data"]][phage_tree[["data"]][["x"]]<x_intercept_height,]

#then the nodes below the line
node_data_below_line <- phage_tree[["data"]][phage_tree[["data"]][["x"]]>x_intercept_height,]

#then we get a list of the nodes below the line whose parents are above the line
node_data_that_crossed_the_line <- node_data_below_line[node_data_below_line[["parent"]]%in%node_data_above_line[["node"]],]

#we wish to add a dot with a number of the cluster at the line intersection - that means the x value will be x_intercept_height, and the y variable
#will be whatever is in the line
cluster_label_data <- data.frame('yvar'=node_data_that_crossed_the_line[["y"]],
                                 'xvar'=rep(x_intercept_height,nrow(node_data_that_crossed_the_line)))

#we set the labels to be 1-4 in reverse order because the tree is flipped, that way it looks like it goes from 1-4 on the actual plot
cluster_label_data <- cbind(cluster_label_data,'label'=rev(seq(1,nrow(cluster_label_data)))[order(cluster_label_data[["yvar"]])])

#we add large black points to serve as backgroud and then white text atop of the dots
phage_tree <- phage_tree+geom_point(data=cluster_label_data,aes(x=xvar,y=yvar),color='black',size=10)+geom_text(data=cluster_label_data,aes(x=xvar,y=yvar,label=label),color='white',size=6)


#anything exceeding 1 is set to 1 to prevent going outside limits
heatmap_data[heatmap_data[,"iAUC"]>1&!is.na(heatmap_data[,"iAUC"]),"iAUC"] <- 1

#first we reformat and set anything below 0.2 to NA
heatmap_data[heatmap_data[,"iAUC"]<0.2&!is.na(heatmap_data[,"iAUC"]),"iAUC"] <- NA

#then we ensure the proper ordering of bacteria and phages
heatmap_data[,"bact"] <- factor(as.character(heatmap_data[,"bact"]),levels = bact_order)
heatmap_data[,"phage"] <- factor(as.character(heatmap_data[,"phage"]),levels = phage_order)

#now we make the heatmap - at least a version that has a bit more info than we need so we can check if the ordering is correct later
p_heatmap <- ggplot(data = heatmap_data,mapping = aes(x=phage,y=bact,fill=iAUC))+custom_theme+geom_tile()
p_heatmap <- p_heatmap + scale_fill_continuous(high=SNIPR_green,low='#ffffff',na.value='#ffffff',limits=c(0.2,1))

#in order for the y axis titles to line up properly on the right I have to have axis labels even though I don't want any, so we set them to a long set of spaces to make things align properly
p_heatmap <- p_heatmap + theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1))+xlab('Phage panel')+ylab(bquote('Bacterial panel ('~italic("n")~' = '~.(as.character(length(unique(heatmap_data[["bact"]]))))~')'))+scale_y_discrete(position = 'right',labels=rep('',length(unique(heatmap_data[["bact"]]))))


#we add MISSING values to the ones that ricardo is looking for
missing_strains <- rownames(iAUC_data)[!rownames(iAUC_data)%in%rownames(SNIPR_MLST)]
add_data <- matrix(data=rep("Not sequenced",ncol(SNIPR_MLST)*length(missing_strains)),ncol=ncol(SNIPR_MLST),nrow=length(missing_strains))
rownames(add_data) <- missing_strains

#we use the same MLSTs as we did before
MLST_colors <- c(
  '10'="#087546",
  '1193'="#45f7a4",
  '12'="#23BA2B",
  '127'="#9DD494",
  '131'="#daf745",
  '224'="#B9A3FF",
  '38'="#1F35FF",
  '410'="#3576CC",
  '46'="#61DAFF",
  '58'="#ABFFF6",
  '69'="#FFCC26",
  '73'="#FF9600",
  '88'="#CC6B3B",
  '95'="#8C3F38",
  'Other'="#ada9a8",
  "Not sequenced"='#000000'
)

SNIPR_MLST[!SNIPR_MLST[,"MLST_type"]%in%names(MLST_colors),"MLST_type"] <- 'Other'

colnames(add_data) <- colnames(SNIPR_MLST)
SNIPR_MLST <- rbind(SNIPR_MLST,add_data)

#we create the heatmap to indicate the MLST groups of our bacterial tree
MLST_heatmap_data <- as.data.frame(cbind('strain'=rownames(SNIPR_MLST),'MLST'=SNIPR_MLST[,"MLST_type"],'yvar'=rep('MLST',nrow(SNIPR_MLST))))

#we order the strains ti follow the same order as the bacterial tree's y-order
bact_order <- as.vector(na.omit(bact_tree[["data"]][["label"]][order(bact_tree[["data"]][["y"]])]))
#we trim the set
MLST_heatmap_data <- MLST_heatmap_data[MLST_heatmap_data[["strain"]]%in%bact_order,]


MLST_heatmap_data[["strain"]] <- factor(MLST_heatmap_data[["strain"]],levels = bact_order)
bact_MLST_heatmap <- ggplot(data = MLST_heatmap_data,aes(y=strain,x=yvar,fill=MLST))+geom_tile()+scale_fill_manual(values = MLST_colors)+custom_theme
bact_MLST_heatmap <- bact_MLST_heatmap+theme(axis.text.y=element_blank(),axis.ticks.y = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),panel.border = element_blank(),panel.background = element_blank(),panel.grid.major = element_blank())


#we make a dotplot to indicate the phages used in SNIPR001
used_in_001_string <- rep("Not used in further testing",ncol(iAUC_data))


#we indicate the ones we used in further in vivo testing
used_for_in_vivo_studies <- c('α15.4','α17.2','α46.4','α51.6','α15.2','α20.4','α48.4','α51.5')
used_in_001_string[colnames(iAUC_data)%in%used_for_in_vivo_studies] <- 'Used in further testing'

#we indicate the ones we used in SNIPR001
used_in_001_TF <- colnames(iAUC_data)%in%SNIPR001_phages
used_in_001_string[used_in_001_TF] <- 'Used in SNIPR001'

plotdat_used_in_001 <- as.data.frame(cbind('phage'=colnames(iAUC_data),'development_status'=used_in_001_string,'yvar'=rep(1,length(used_in_001_string))))
plotdat_used_in_001[["phage"]] <- factor(plotdat_used_in_001[["phage"]],levels=phage_order)
plotdat_used_in_001[["development_status"]] <- factor(plotdat_used_in_001[["development_status"]] ,levels=c('Used in SNIPR001','Used in further testing','Not used in further testing'))

color_scale <- c("Used in SNIPR001"=SNIPR_green,"Not used in further testing"='#ffffff',"Used in further testing"=SNIPR_grey)
final_cocktail_plot <- ggplot(plotdat_used_in_001,aes(x=phage,y=yvar,color=development_status))+geom_point()+custom_theme
final_cocktail_plot <- final_cocktail_plot+theme(panel.border = element_blank(),panel.grid = element_blank(),axis.text.x=element_text(angle=50,vjust=1,hjust=1),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
final_cocktail_plot <- final_cocktail_plot + scale_color_manual('Development status',values = color_scale)+xlab('Phage')
final_cocktail_plot
#now we start assembling stuff
#let's build core outmi
#we start by adding the barplot to the heatmap


#we extract all the legends
MLST_legend <- get_legend(bact_MLST_heatmap)
main_heatmap_legend <- get_legend(p_heatmap)
dots_heatmap_legend <- get_legend(final_cocktail_plot)


#we make a version without the tree and MLST
#we combine the legends
all_the_legends <- cowplot::plot_grid(
  plotlist = list(MLST_legend,
                  main_heatmap_legend,
                  dots_heatmap_legend),
  ncol = 1,
  rel_heights = c(2.5,1,1),
  align = 'hv')



comboplot <- cowplot::plot_grid(
  plotlist = list(NULL,
                  phage_tree+theme(plot.margin = margin(r=0,l=0,0,0,unit = 'cm')),
                  bact_MLST_heatmap+theme(legend.position = 'none',axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin = margin(0,0,0,0,'cm')),
                  p_heatmap+theme(plot.margin = margin(t=0,b=0,l=0,r = 0,unit = 'cm'),legend.position = 'none',axis.text.x = element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank()),
                  NULL,final_cocktail_plot+theme(plot.margin = margin(0,0,0,0,unit = 'cm'),legend.position = 'none')),
  
  align = 'vh',
  ncol=2,
  nrow=3,
  axis = 'trbl',
  rel_heights = c(0.4,1,0.15),
  rel_widths = c(0.08,0.6)
)


comboplot <- cowplot::plot_grid(plotlist = list(comboplot,all_the_legends),nrow=1,rel_widths = c(1,0.5))

ggsave(plot = comboplot,filename = paste0(fig_folder,'Supplementary figure 5.pdf'),width=8,height=8,device=cairo_pdf)
ggsave(plot = comboplot,filename = paste0(fig_folder,'Supplementary figure 5.png'),width=8,height=8,device=cairo_pdf)

#we output the MLST distribution
summary_MLST <- table(SNIPR_MLST[,"MLST_type"])
out <- data.frame('MLST'=names(summary_MLST),'Frequency'=as.vector(summary_MLST))
write.xlsx(out,file = paste0(summary_stats_folder,'supplementary figure 5 MSLT distribution.xlsx'))

########################
#Supplementary figure 6#
########################
#we read the combination data
individual_and_combination_data <- read.delim(file = "all_individual_and_combination_data.tsv",sep = '\t')

#we merge the plates which test different sets of the panel and average out the iAUCs
individual_and_combination_data <- individual_and_combination_data %>% group_by(phage_combination,strain) %>% summarise("mean_iAUC"=mean(iAUC))

iAUC_matrix <- reshape2::acast(individual_and_combination_data,list('strain',"phage_combination"),value.var = 'mean_iAUC')

#we calculate ranges for the tested combinations of phages
combination_data <- iAUC_matrix[,grepl('[+]',colnames(iAUC_matrix))]
individual_data <- iAUC_matrix[,!grepl('[+]',colnames(iAUC_matrix))]

combination_host_range_data <- colMeans(combination_data>0.2)

imputed_ranges <- c()
for(combination in names(combination_host_range_data)){
  #we get the composing phages in the combination
  composing_phages <- strsplit(combination,'[+]')[[1]]
  
  #we impute the range
  imputed_ranges <- c(imputed_ranges,mean(rowSums(individual_data[,composing_phages]>0.2)>0))
}

#we combine the data for plotting purposes
plotdat <- data.frame('combination'=names(combination_host_range_data),'actual_host_range'=unname(combination_host_range_data),'imputed_host_range'=imputed_ranges)

#we make a linear fit
model <- lm(actual_host_range~imputed_host_range, data=plotdat)
R2=summary(model)[["r.squared"]]
R2_to_display=as.character(round(R2,3))

#we put a labe in the plot
label_to_add <- parse(text=paste("R^2:",R2_to_display))

p <- ggplot(plotdat,mapping = aes(x=actual_host_range,y=imputed_host_range))+geom_point()+coord_equal()
p <- p+custom_theme+ylab('Imputed host range')+xlab('Actual host range')
p <- p+geom_smooth(method='lm',color=SNIPR_grey,fill=SNIPR_color1)+geom_label(x=0.3,y=0.4,label=label_to_add)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 6.pdf'),device=cairo_pdf,width=4.5,height=4.5)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 6.png'),device=cairo_pdf,width=4.5,height=4.5)

#the important stats here are the things obtained from the linear model - at least I think so
#so we write the model stats
out <- summary(model)[['coefficients']]
write.xlsx(out,file = paste0(summary_stats_folder,'supplementary figure 5 linear fit.xlsx'))


########################
#Supplementary figure 7# 
########################
calculate_marginal_combination_one_phage_with_exclusion_groups <- function(n_comb,iAUC_array,exclude_phage){
  temp_fun <- function(combo,iAUC_array){
    panel_hits <- rowSums(iAUC_array[,combo]>0.2,na.rm = T)
    score <- mean(as.character(panel_hits))
    
    return(score)
  }
  
  expand_phage_groups <- function(combo,phages_grouped){
    list_to_expand <- list()
    for(group in combo){
      list_to_expand[[length(list_to_expand)+1]] <- phages_grouped[names(phages_grouped)==group]
    }
    return(expand.grid(list_to_expand))
  }
  
  phages <- colnames(iAUC_array)
  
  #we only add one phage originating from each wild type, so we create a vector to access the WT info
  phage_groups <- unlist(lapply(phages,FUN = function(x){strsplit(x,'[.]')[[1]][1]}))
  phages_grouped <- phages
  names(phages_grouped) <- phage_groups
  
  #we wish to calculate the marginal utility of a given phage so the
  #group that it belongs to should not be added, so we exclude that 
  #WT group
  excluded_group <- strsplit(exclude_phage,'[.]')[[1]][1]
  groups_to_combine <- unique(phage_groups[phage_groups!=excluded_group])
  
  #we create the combinations of all the WT phage groups except the one we are interested in of size n-1, so we an ad
  #the phage of interest later
  phage_groups_combs_without <- combn(groups_to_combine,n_comb-1)
  
  #we "expand" the groups from WT groups into individual CAPs
  expanded_combinations <- apply(phage_groups_combs_without,2,expand_phage_groups,'phages_grouped'=phages_grouped)
  expanded_combs <- do.call('rbind',expanded_combinations)
  
  #we then calculate the ranges of these combinations
  scores_without <- apply(expanded_combs,1,
                          FUN = function(x,iAUC_array){
                            return(mean(rowSums(iAUC_array[,x]>0.2,na.rm = T)>0))
                          },
                          'iAUC_array'=iAUC_array)
  
  #then we calculate the ranges but we add the phage of interest to all combinations
  scores_with <- apply(cbind(expanded_combs,rep(exclude_phage,nrow(expanded_combs))),1,
                       FUN = function(x,iAUC_array){
                         return(mean(rowSums(iAUC_array[,x]>0.2,na.rm = T)>0))
                         },
                       'iAUC_array'=iAUC_array)
  
  #the difference between the host range with and without the phage of interest i the marginal utility
  marginal_utility <- scores_with-scores_without
  
  #we combine the data so we can keep track of exactly which combinations have which marginal utility
  returdf <- data.frame('combination'=apply(expanded_combs,MARGIN = 1,FUN = paste,'collapse'='&'),
                          'added'=rep(exclude_phage,nrow(expanded_combs)),
                          'marginal_utlity'=marginal_utility  
  )
  
  #and return that dataframe
  return(returdf)
}

getmode <- function(x) {
  xdens = density(x)
  modex = xdens$x[which.max(xdens$y)]
}

#we wish to create combinations of 4
n_comb <- 4

#we load the CAP screening data
iAUC_data <- as.data.frame(read_excel(paste0(data_folder,"iAUC_data_SNIPR_panel_CAP_screening.xlsx")))
rownames(iAUC_data) <- iAUC_data[,1]
iAUC_data <- iAUC_data[,-1]

#we average out the iAUC replicates
#we start with the core feature of the plot - the heatmap
heatmap_data <- reshape2::melt(as.matrix(iAUC_data))
colnames(heatmap_data) <- c('bact','phage','iAUC')

#we average out the multiple runs
heatmap_data[["phage"]] <- gsub('[.]1','',heatmap_data[["phage"]])
heatmap_data <- heatmap_data %>% group_by(bact,phage) %>% summarise(iAUC=median(iAUC,na.rm=T))
heatmap_data <- as.data.frame(heatmap_data)

#we reformat to an array again
iAUC_array <- reshape2::acast(heatmap_data,formula = list(names(heatmap_data)[1],names(heatmap_data)[2]))

#we calculate the maginal utility
temp_dat <- lapply(X=colnames(iAUC_array),
                   FUN=calculate_marginal_combination_one_phage_with_exclusion_groups,
                   "n_comb"=n_comb,
                   'iAUC_array'=iAUC_array)
plotdat <- do.call('rbind',temp_dat)


#we apply the clustering that we show in supplementary figure 5
phage_dendrogram <- as.dendrogram(hclust(dist(scale(t(iAUC_array)))))

#we make the trees
phage_tree <- ggtree(phage_dendrogram)+coord_flip()+scale_x_reverse()+scale_y_reverse()

x_intercept_height <- 11.5
#we add a dotted line to indicate our four clusters
phage_tree <- phage_tree+geom_vline(xintercept = x_intercept_height,linetype='dashed',size=1)
bact_tree <- ggtree(bact_dendrogram)

#we get the order of bacteria andphages
bact_order <- na.omit(bact_tree[["data"]][order(bact_tree[["data"]][["y"]]),"label"])[[1]]
phage_order <- na.omit(phage_tree[["data"]][order(phage_tree[["data"]][["y"]]),"label"])[[1]]#the tree will be flipped upside down so the order will need to be reversed


#we also generate the phage clusters - we don't wish to pick any that have similar panel inhbition profiles
#so we split the data into the number of phages we intend do have in our final cocktail - 4
cluster_definitions <- cutree(hclust(dist(scale(t(iAUC_array)))),k = n_comb)

#the names in the cluster definitions isn't ordered, so we re-order them according to a left-to-right scheme
cluster_change <- seq(1,length(unique(cluster_definitions)))
names(cluster_change) <- as.character(rev(unique(cluster_definitions[phage_order])))

#we change the names of the clusters so the first cluster is found left-most on the plot, and the last cluster is found right-most in the plot
#mostly for aesthetic reasons and for it to be consistent with a previous ploy
new_cluster_definitions <- cluster_change[as.character(cluster_definitions)]
names(new_cluster_definitions) <- names(cluster_definitions)
cluster_definitions <- new_cluster_definitions


#we order them internally based on their mode i.e. the utility occours the most, since we find that to be a good measure
#of the phages we should move on with in development
order_data <- plotdat %>% group_by(added) %>% summarise('mode'=getmode(marginal_utlity))
order_data <- as.character(order_data[["added"]][order(order_data[["mode"]])])
plotdat[["added"]] <- factor(plotdat[["added"]],levels = as.character(order_data))

#we prepare to plot and assemble yhe final data
plotdat <- cbind(plotdat,'grouped'=cluster_definitions[as.character(plotdat[["added"]])])


#the following CAPs were picked for in vivo studies based on a previous uncolored version of this plot
used_for_in_vivo_studies <- c('α15.4','α17.2','α46.4','α51.6','α15.2','α20.4','α48.4','α51.5')
#we add a color variable
color_variable <- rep("Not used for further study",nrow(plotdat))
color_variable[plotdat[["added"]]%in%used_for_in_vivo_studies] <- "Used for further study"

#we also wish to highlight which phages were eventually turned into the final cocktail
for(SNIPR001_CAP in SNIPR001_phages){
  color_variable[plotdat[["added"]]%in%SNIPR001_CAP] <- SNIPR001_CAP
}
plotdat <- cbind(plotdat,'color_variable'=color_variable)

#we set the color scheme
local_color_scheme <- c("Not used for further study"=SNIPR_grey,
                        'Used for further study'=SNIPR_color1,
                        phage_colors)

#and we start plotting. We initialze the plot with a violin plot where the fill indicates what the phage was eventually used for
p <- ggplot(data = plotdat,mapping = aes(x=added,y=marginal_utlity,fill=color_variable,color=color_variable))+geom_violin()+scale_fill_manual('',values = local_color_scheme)+scale_color_manual('',values = local_color_scheme)
#we create facets to indicate the iAUC profile groups that we pick from
p <- p+facet_grid(cols = vars(grouped),space = 'free_x',scales = 'free_x')
#we remove the legend and set the y axis title
p <- p+ylab('Marginal host range gain')+theme(legend.position = 'none')
#we set some final style choices
p <- p+custom_theme+theme(axis.text.x = element_text(angle=50,vjust=1,hjust=1),axis.title.x=element_blank())

#and save the figure
ggsave(p,filename = paste0(paste0(fig_folder,'Supplementary figure 7.pdf')),device = cairo_pdf,width=6,height=4)
ggsave(p,filename = paste0(paste0(fig_folder,'Supplementary figure 7.png')),device = cairo_pdf,width=6,height=4)


#we output summary statistics on the added utility
outdat <- plotdat %>% group_by(added) %>% summarise('mean'=mean(marginal_utlity),'sd'=sd(marginal_utlity))
write.xlsx(outdat,file = paste0(summary_stats_folder,'supplementary figure 7 summary stats.xlsx'))

########################
#Supplementary figure 8#
########################
#for this figure we wish to illustrate our in vivo results
#by indicating how many PFU/g our CAPs create relative to the initial
#dose after a some time to estimate so pharmacodynamics

#we load the relevant data
dosages <- read_excel(paste0(data_folder,"dosages.xlsx"))
PFU_counts <- as.data.frame(read_excel(paste0(data_folder,"PK_recovery_per_gram.xlsx")))
LOD_data <- read_excel(paste0(data_folder,"LOD_data.xlsx"))

#we reformat the dosage vector
dosage_vector <- dosages[["intial dose (PFU)"]]
names(dosage_vector) <- dosages[["Dosed phage"]]

#we set the x axis to be the time since dosage
rownames(PFU_counts) <- PFU_counts[,"Time (h)"]
PFU_counts <- PFU_counts[,-1]

#we reformat for plotting
PFU_counts_melt <- reshape2::melt(as.matrix(PFU_counts))
colnames(PFU_counts_melt) <- c('time','phage','PFU_g')

PFU_counts_melt[["phage"]] <- unlist(strsplit(as.character(PFU_counts_melt[["phage"]]),'[.][.][.]'))[seq(1,nrow(PFU_counts_melt)*2,2)]

#we adjust the PFU counts to the initial dosage
PFU_counts_melt <- cbind(PFU_counts_melt,'dose_adjusted_PFU_g'=PFU_counts_melt[["PFU_g"]]/dosage_vector[PFU_counts_melt[,"phage"]])

#and we log transform the data
PFU_counts_melt <- cbind(PFU_counts_melt,'log10_dose_adjusted_PFU_g'=log(PFU_counts_melt[,"dose_adjusted_PFU_g"],10))

#finally some of the points were initially set to zero. The purposes of calculating an average this is fine, but we will set these to some lower value that we indicate as zero on the plot
PFU_counts_melt <- cbind(PFU_counts_melt,'plot_variable'=PFU_counts_melt[,"log10_dose_adjusted_PFU_g"])
one_rows <- PFU_counts_melt[,"PFU_g"]==1
one_rows[is.na(one_rows)] <- F

#we would like to add some dotted lines to indicate the LOD values. Of course these would also need to be adjusted to the dosages
LOD_data <- cbind(LOD_data,'adjusted_LOD'=LOD_data[["LOD"]]/dosage_vector[LOD_data[["phage"]]])

#since we use pre-calculated log10'd values and just set the y labels differently, we'll also scale the LOD data
LOD_data <- cbind(LOD_data,'logged_adjusted_LOD'=log(LOD_data[["adjusted_LOD"]],10))
#and then we average out the runs
mean_data <- PFU_counts_melt %>% group_by(time,phage) %>% summarise(avg=mean(log10_dose_adjusted_PFU_g,na.rm=T))
mean_data <- as.data.frame(mean_data)

#we get the plot's minimum value
lowest_val <- min(c(PFU_counts_melt[!one_rows,"plot_variable"],LOD_data[["logged_adjusted_LOD"]]),na.rm = T)
#we'll only have breaks every second value so we get the lowest break that is divisible by two and that belongs to the "real values" that we have to plot
lowest_plot_value <- floor(lowest_val/2)*2

#we insert a horizontal bar at the break below the lowest number on the "real" scale and then set the zero values to be two below that
PFU_counts_melt[one_rows,"plot_variable"] <- lowest_plot_value-2


#for calculating the means that are to be plotted as a line, we set the PFU/G=0 to 1, in order to avoid NAs and not artificially inflate our average lines
#however, there are time points that are entirely composed of 0s, leading a scenario where all the points are in a seperate section to indicate 0, while
#the mean awkwardly hovers above it. We would like to set y-coordinate for the mean at these points to the LOD
below_LOD <- mean_data[["avg"]]<LOD_data[mean_data[["phage"]],"logged_adjusted_LOD"]
mean_data[below_LOD,"avg"] <- LOD_data[mean_data[below_LOD,"phage"],"logged_adjusted_LOD"]

#for plotting purposes we set the values that were initially 1's to some other low value that we indicate on the plot
breaks <- -seq(0,-(lowest_plot_value)+2,2)
labels <- to_tenth_power_labels(breaks)
#we set the label just above the -12 to be "" and the -12 one to say 0
labels[length(labels)] <- c('Below LOD')


#we use the old colors where applicable, the others are just grey
phages_without_color <- unique(mean_data[["phage"]])[!unique(mean_data[["phage"]])%in%names(phage_colors)]
color_scale <- rep('#000000',length(phages_without_color))
names(color_scale) <- phages_without_color
color_scale <- c(color_scale,phage_colors)



#we order our data according to LOD to make the line jump around a little less awkwardly
phage_order <- LOD_data[order(LOD_data[["adjusted_LOD"]]),"phage"]
PFU_counts_melt[["phage"]] <- factor(PFU_counts_melt[["phage"]],levels = phage_order)
mean_data[["phage"]] <- factor(mean_data[["phage"]],levels = phage_order)
LOD_data[["phage"]] <- factor(LOD_data[["phage"]],levels=phage_order)

#we wish for the dashed line to have a legend indicating that it is the LOD, so we have to do a little messy messing around
LOD_data <- cbind(LOD_data,'linetype'=rep('LOD',nrow(LOD_data)))


p <- ggplot()+geom_point(data = PFU_counts_melt,aes(x=time,y=plot_variable,color=phage),alpha=0.5,position = position_jitter(width = 1))+facet_wrap(~phage,nrow=2)+geom_path(data = mean_data,mapping = aes(group=phage,color=phage,x=time,y=avg))
p <- p + scale_y_continuous(breaks=breaks,labels = labels,limits=c(min(breaks)-1,max(breaks)))+custom_theme+scale_x_continuous(breaks=unique(PFU_counts_melt[["time"]]))+scale_color_manual(values = color_scale)+theme(legend.position = 'none')#+geom_hline(yintercept = breaks[length(breaks)-1])
p <- p+ylab(expression(frac(PFU/g, PFU[dose])))+xlab('Hours')
#we add the LODs as a dashed line
p <- p + geom_hline(data = as.data.frame(LOD_data),mapping = aes(yintercept = logged_adjusted_LOD),linetype='solid')


ggsave(p,filename = paste0(fig_folder,"Supplementary figure 8.pdf"),device=cairo_pdf,width=5,height=3)
ggsave(p,filename = paste0(fig_folder,"Supplementary figure 8.png"),width=5,height=3)

#we output summary statistics
out <- PFU_counts_melt %>% group_by(time,phage) %>% summarise('mean'=mean(PFU_g,na.rm=T),'sd'=sd(PFU_g,na.rm = T))
colnames(out) <- c('hours since dosage','phage','mean CFU/g','sd CFU/g')
write.xlsx(out,file = paste0(summary_stats_folder,'supplementary figure 8 summary stats.xlsx'))

#we also output the adjusted LODs
out <- LOD_data[,c("phage","logged_adjusted_LOD")]
write.xlsx(out,file = paste0(summary_stats_folder,'supplementary figure 8 adjusted LODs.xlsx'))

########################
#Supplementary figure 9#
########################
#we load the stability date, which is the PFU/mL relative to thje initial PFU/mL
stability_data <- data.frame(read_excel(paste0(data_folder,"phage_stability_data.xlsx")))

PFU_ml_cols <- colnames(stability_data)[grepl('Relative',colnames(stability_data))]
#we wish to plot the figure with log10 on the y axis so we compute the averages and sds on log10 to be consistent with other plots
stability_data[,PFU_ml_cols] <- log(stability_data[,PFU_ml_cols],10)

#we calculate the averages and standard deviations
plotdat <- cbind(stability_data,'avg_data'=rowMeans(as.matrix(stability_data[,PFU_ml_cols])),'sd_data'=matrixStats::rowSds(as.matrix(stability_data[,PFU_ml_cols])))

#we define the breaks for the plot
y_breaks <- seq(0,-5,-1)
y_labels <- paste0(10^breaks*100,' %')

#any color that does not have a color set, we set those to zero
new_phages <- unique(plotdat[["Phage"]])[!unique(plotdat[["Phage"]])%in%names(phage_colors)]
additional_colors <- rep('#000000',length(new_phages))
names(additional_colors) <- new_phages
color_scale <- c(phage_colors,additional_colors)

p <- ggplot(plotdat,mapping = aes(color=Phage,x=Days,y=avg_data,ymax=avg_data+sd_data,ymin=avg_data-sd_data,group=Phage))+geom_path()
p <- p + geom_errorbar()
p <- p + scale_y_continuous(breaks = y_breaks,labels = y_labels)+scale_x_continuous(breaks=unique(plotdat[["Days"]]))
p <- p + custom_theme+ylab('Concentration relative to stating concentraion')+xlab('Days since storage start')
p <- p + facet_wrap(~Phage,nrow=2)+geom_hline(yintercept = log(0.5,10),linetype='dashed')+scale_color_manual(values = color_scale)+theme(legend.position = 'none')
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 9.pdf'),device = cairo_pdf,height=5.64*0.6,width=6)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 9.png'),height=5.64*0.6,width=6)

#we output summary statistics
stability_data_melt <- reshape2::melt(stability_data,id.vars=c("Phage","Days"))
out <- stability_data_melt %>% group_by(Phage,Days) %>% summarise('mean'=mean(value,na.rm=T),'sd'=sd(value,na.rm=T))
colnames(out) <- c('Phage','Days since storage start','mean log10 reduction','sd log10 reduction')

write.xlsx(out,file = paste0(summary_stats_folder,'supplementary figure 9 summary stats.xlsx'))

#########################
#Supplementary figure 10#
########################
#we load the in vivo data
CFU_data <- as.data.frame(read_excel(paste0(data_folder,"CFU_data_CAP_screening.xlsx")))

#we reformat the data to a more plotabble format
CFU_plotdat <- reshape2::melt(CFU_data,id.vars=c('day','dosed','Study'))

#the experiment was carried out in two separate batches and we would like to highlight this 
CFU_plotdat[["Study"]] <- paste0('Study ',CFU_plotdat[["Study"]])

#we set the order so that the gentamicin thing is on the right to and vehicles on the left
x_axis_levels <- unique(CFU_plotdat[["dosed"]])
x_axis_levels <- c('Vehicle',x_axis_levels[grepl('α',x_axis_levels)],'Gentamicin')
CFU_plotdat[["dosed"]] <- factor(CFU_plotdat[["dosed"]],levels = x_axis_levels)


#we calculate significance for the third day in both studies at day three
signif_data <- data.frame()
for(Study in unique(CFU_plotdat[["Study"]])){
  #we get all the data for  day 3 - the relevant time point
  relevant_data <- CFU_plotdat[CFU_plotdat[["Study"]]==Study&CFU_plotdat[["day"]]=='Day 3',]
  #we get the relevant phages
  administered_items <- unique(as.character(relevant_data[,"dosed"]))
  #we are only interestsed in comparing to phages
  administered_phages <- administered_items[grepl("α",administered_items)]
  
  #we get the vehicle results
  control_values <- relevant_data[relevant_data[["dosed"]]=='Vehicle',"value"]  
  #we compare each phage to the control values
  
  for(phage in administered_phages){
    phage_values <- relevant_data[relevant_data[["dosed"]]==phage,"value"]
    #we compare using a wilcoxon test
    result <- wilcox.test(control_values,phage_values)
    pval <- result[["p.value"]]
    
    #we add the relevant data
    signif_data <- rbind(signif_data,data.frame('x_start'='Vehicle',"x_stop"=phage,'pval'=pval,'Study'=Study,'day'='Day 3'))
    
  }
}

adjusted_data <- signif_data %>% group_by(Study) %>% summarise(padjust=p.adjust(pval))
signif_data <- cbind(signif_data,'padj'=adjusted_data[["padjust"]])
stars <- rep('NS',nrow(signif_data))
stars[signif_data[["padj"]]<0.05] <- '*'
stars[signif_data[["padj"]]<0.01] <- '**'
stars[signif_data[["padj"]]<0.001] <- '***'

signif_data <- cbind(signif_data,'stars'=stars)

#we wish to show the significance above the highest point
signif_data <- signif_data[signif_data[["stars"]]!='NS',]

#we add a y variable as the position of the stars. These should be above the max point
#for each facet and be incrementally increased to avoid overlaps. We set an increment we think looks nice in this context
increment <- 7
#and start going through each facet
y_position <- c()
phage_order <- c()
for(study in unique(signif_data[["Study"]])){
  #we get the starting value
  y_value <- max(CFU_plotdat[CFU_plotdat[["Study"]]==study & CFU_plotdat[["day"]]=='Day 3',"value"],na.rm = T)*increment
  
  
  #we go through each data point and get the appropriate y-value
  #we go through the phages in reverse order for the nicer visual effect 
  for(phage in rev(signif_data[signif_data[["Study"]]==study,"x_stop"])){
    print(phage)
    y_position <- c(y_position,y_value)
    y_position <- y_position*increment
    
    phage_order <- c(phage_order,phage)
    
  }
}
#we attach the data
names(y_position) <- phage_order

signif_data <- cbind(signif_data,'y'=y_position[signif_data[["x_stop"]]])


#we set the y axis ticks and set their labels to a scientific format, also adding an indicator of LOD
LOD=2.40E+02
y_axis_ticks <- 10^seq(0,100,2)
y_axis_labels <- to_tenth_power_labels(seq(0,100,2))
y_axis_ticks <- c(y_axis_ticks,LOD)
y_axis_labels <- c(y_axis_labels,'LOD     ')

additional_colors <- unique(CFU_data[["dosed"]])[!unique(CFU_data[["dosed"]])%in%names(phage_colors)]
new_colors <- rep(SNIPR_grey,length(additional_colors))
names(new_colors) <- additional_colors


p <- ggplot() + geom_jitter(CFU_plotdat,mapping = aes(x=dosed,y=value,color=dosed),width=0.2)+facet_wrap(~Study+day,ncol=3,scales = 'free_x')
p <- p + custom_theme + scale_color_manual('',values = c(phage_colors,new_colors))
p <- p + theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1),legend.position = 'none')+ylab('CFU/g')+xlab('Treatment')
p <- p + geom_hline(yintercept = LOD,linetype='dashed')
p <- p + scale_y_continuous(trans = 'log10',breaks = y_axis_ticks,labels = y_axis_labels)
p <- p + ggsignif::geom_signif(data=signif_data,mapping = aes(xmin=x_start,xmax=x_stop,annotations=stars,y_position=log(y,10)),textsize=3,vjust=0.5,manual = T)

ggsave(p,filename = paste0(fig_folder,'Supplementary figure 10.pdf'),device = cairo_pdf,width=6,height=7)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 10.png'),width=6,height=7)

#we output summary statstics
out <- CFU_plotdat %>% group_by(day,dosed,Study) %>% summarise('mean'=mean(value,na.rm=T),'sd'=sd(value,na.rm=T))
write.xlsx(out,file = paste0(summary_stats_folder,'supplementary figure 10 summary stats.xlsx'))

#########################
#Supplementary figure 11#
#########################
OL_dat <- read.csv(paste0(data_folder,"SNIPR433_vs_CAP_panel.csv"), row.names=1)#IS THIS THE SAME AS SOME OTHER FILE WE HAVE??

#we compute the averages of the replicates
OL_dat_melt <- reshape2::melt(as.matrix(OL_dat))
colnames(OL_dat_melt) <- c('bact','phage','iAUC')

#we'll reformat the names of the phages to remove replicate data

OL_dat_melt[["phage"]] <- unlist(lapply(as.character(OL_dat_melt[["phage"]]),FUN = function(x){
  paste(strsplit(x,'[.]')[[1]][1:2],collapse='.')
}))

#we average out the runs and cats it back into an array
mean_OL_dat_melt <- OL_dat_melt %>% group_by(bact,phage) %>% summarise(mean=mean(iAUC,na.rm=T))
mean_OL_dat_melt <- as.data.frame(mean_OL_dat_melt)
OL_dat_mean <- reshape2::acast(data = mean_OL_dat_melt,formula = bact~phage,value.var = "mean")

#we read the in vivo data to compute the day3 effectiveness
in_vivo_CFU_data <- as.data.frame(read_excel(paste0(data_folder,"CFU_data_CAP_screening.xlsx")))
day3_data <- in_vivo_CFU_data[in_vivo_CFU_data[["day"]]=="Day 3",]

#we compute the average effect of each phage by comparing the CAP results
#to the vehicle results for each study
in_vivo_effect_sizes <- c()
tested <- c()
relevant_cols <- colnames(day3_data)[grepl('subject',colnames(day3_data))]

for(study in unique(day3_data[["Study"]])){
  #we isolate the study data
  study_data <- day3_data[day3_data[["Study"]]==study,]
  
  #and isolate the CFU values for the control population
  vehicle_CFUs <- study_data[study_data[["dosed"]]=='Vehicle',relevant_cols]
  
  #we wish to report the effect in log10 reductions, so we log10 transform the data and then comput ethe average log10 CFUs
  vehicle_mean <- mean(log(unlist(vehicle_CFUs),10))
  
  #we go through the phages used in this study
  for(phage in study_data[grepl('α',study_data[["dosed"]]),"dosed"]){
    #we isolate the phage CFU values
    phage_CFUs <- study_data[study_data[["dosed"]]==phage,relevant_cols]
    
    #and calculat ethe average log10 effect
    phage_mean <- mean(log(unlist(phage_CFUs),10),na.rm = T)
    log10_diff <- vehicle_mean-phage_mean
    
    #and save the results
    in_vivo_effect_sizes <- c(in_vivo_effect_sizes,log10_diff)
    tested <- c(tested,phage)
  }
}
names(in_vivo_effect_sizes) <- tested


#we generate every combinations of CAPs 
combinations <- combn(colnames(OL_dat_mean),4)

#we create a function to compute the in vitro host range for each combination
compute_in_vitro_measures <- function(combo,OL_dat_mean){
  return(mean(rowSums(OL_dat_mean[,combo]>0.2,na.rm = T)>0,na.rm = T))
}

#we compute the total in vivo effect for each combo
compute_in_vivo_measures <- function(combo,in_vivo_effect_sizes){
  return(sum(in_vivo_effect_sizes[combo]))
}

#we generate the total in vivo effect for a combination
plotdat_in_vivo <- apply(combinations,2,FUN=compute_in_vivo_measures,'in_vivo_effect_sizes'=in_vivo_effect_sizes)

#we generate the total in vitro effect for a combination
plotdat_in_vitro <- apply(combinations,2,FUN=compute_in_vitro_measures,'OL_dat_mean'=OL_dat_mean)


#We generate the combination names and we also highlight the combination that
#was eventually chosen for SNIPR001
SNIPR001_combo <- apply(combinations,2,FUN = function(x,SNIPR001_phages){
  return(
    #sees if all 4 SNIPR001 strains are in the combination
    c('SNIPR001'=sum(x%in%SNIPR001_phages)==4,
      #as well as the combination of phages
      'combo'=paste0(x,collapse = '&')
    )
  )
},'SNIPR001_phages'=SNIPR001_phages)


#we merge the elements of the plot data
SNIPR001_combo <- as.data.frame(t(SNIPR001_combo))
plotdat <- data.frame("avg_effect"=plotdat_in_vivo,"range"=plotdat_in_vitro,SNIPR001_combo)

#we reformat slightly
plotdat[["SNIPR001"]] <- factor(plotdat[["SNIPR001"]],levels = c('TRUE','FALSE'))

#we'll add a label to highlight SNIRP001
textdat <- plotdat[plotdat[["SNIPR001"]]=='TRUE',]
textdat <- cbind(textdat,'label'='SNIPR001')

#we initialize the plot 
p <- ggplot(plotdat,mapping = aes(x=range,y=avg_effect,color=SNIPR001,alpha=SNIPR001,size=SNIPR001))
#we point, color them, and increase the size of the SNIPR001 combination to extra highlight it
p <- p + geom_point()+scale_alpha_manual(values = c('FALSE'=1,'TRUE'=1))+scale_color_manual(values = c('FALSE'="#000000",'TRUE'=SNIPR_green))+scale_size_manual(values = c('FALSE'=1,"TRUE"=3))
#We set the custom theme to be consistent with the rest of the plot and change the x and y axis titles
p <- p + custom_theme+ylab(expression(paste("Total log"[10]*" CFU/g reduction",italic(' in vivo'))))+xlab(expression(paste(italic('In vitro'),' host range')))
#we add the label that highlights SNIPR001 
p <- p + ggrepel::geom_label_repel(data = textdat,mapping = aes(x=range,y=avg_effect,label=label),box.padding=3,nudge_x=-0.02,nudge_y=0.3,segment.curvature=-0)+theme(legend.position = 'none')#we insert a label highlighting SNIPR001 and we nudge the label a little to the top left
#we change the x axis labels
p <- p + scale_x_continuous(breaks = seq(0,100,2.5)/100,labels = paste0(seq(0,100,2.5),'%'))

#and we save the plot
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 11.pdf'),width=4,height=4)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 11.png'),width=4,height=4)


#I don't really think there is a good way of summarising this so I will just write the plot data, as it doesn't contain that much data anyway
write.xlsx(plotdat,file = paste0(summary_stats_folder,'supplementary figure 11 summary stats.xlsx'))

#########################
#Supplementary figure 14#
#########################
#we load the phage stability data 
plotdat <- read_excel(paste0(data_folder,"CAP_stability_data.xls"))

#we log-transform the PFU measurements
logged_reps <- log(plotdat[,c("rep 1","rep 2","rep 3")],10)
colnames(logged_reps) <- paste0('logged ',colnames(logged_reps))

#we calculate the avg and sd log PFU/mL and add that to the plot data
logged_reps <- cbind(logged_reps,'sds'=matrixStats::rowSds(as.matrix(logged_reps)),'means'=rowMeans(as.matrix(logged_reps)))
plotdat <- as.data.frame(cbind(plotdat,logged_reps))

#we replace the a's with alphas
plotdat[["phage"]] <- gsub('a','α',plotdat[["phage"]])

#we create the y-scale breaks and labels
y_breaks=seq(0,ceiling(max(plotdat[["means"]]+plotdat[["sds"]])))
y_break_labels <- to_tenth_power_labels(y_breaks)

#We modify the break labels on the x axis to indicate months
x_breaks=seq(floor(min(plotdat[["month"]])),ceiling(max(plotdat[["month"]])))
#and these labels
x_labels=x_breaks
x_labels[x_labels=="0"] <- 'Storage\nstart'
x_labels[length(x_labels)] <- paste0(x_labels[length(x_labels)],' months')

#we initialize the plot
p <- ggplot(data = plotdat,mapping = aes(x = month,y=means,ymax=means+sds,ymin=means-sds,fill=phage,color=phage))#boots plot

#adds both line and point-ranges
p <- p+geom_line()+geom_pointrange(size=0.3)

#we change the colors 
p <- p + scale_color_manual(values = phage_colors)#sets colors
p <- p+custom_theme+ylab('PFU/mL')+theme(legend.title=element_blank(),legend.spacing.x = unit(0,'cm'),legend.spacing.y = unit(0,'cm'),axis.title.x = element_blank())#sets the theme and makes some minor adjustments
p <- p + scale_x_continuous(breaks=x_breaks,labels = x_labels)#sets the x-axis and creates custom labels
p <- p + scale_y_continuous(breaks = y_breaks,labels = y_break_labels,limits=c(min(y_breaks),max(y_breaks)))+theme(legend.title=element_blank())#sets the y scale and creates custom labels


#saves the plot
ggsave(plot = p,filename = paste0(fig_folder,'Supplementary figure 14.pdf'),device=cairo_pdf,height=3,width=5)
ggsave(plot = p,filename = paste0(fig_folder,'Supplementary figure 14.png'),device=cairo_pdf,height=3,width=5)





#########################
#Supplementary figure 15#
#########################
#we load the unformatted plot data of how SNIPR001 and phages inhibit the growth of aerogic bacteria
plotdat_unformatted <- read.csv(paste0(data_folder,"SNIPR001 v Aerobic panel.csv"), row.names=1)
#we melt to convert it into a more ggplot friendly format
plotdat_reformatted <- reshape2::melt(reshape2::melt(as.matrix(plotdat_unformatted)))

#we remove the column that just says "value" and alter the formatting slightly
plotdat_reformatted <- as.data.frame(plotdat_reformatted[,-3])
colnames(plotdat_reformatted) <- c('Bacteria','treatment','inhib')

#renames the replicates to have consistent naming for all phages, control and SNIPR001
for(name in c(SNIPR001_phages,'Control','SNIPR001')){
  plotdat_reformatted[["treatment"]] <- gsub(paste0(paste(name,seq(1,10),sep='.'),collapse='|'),name,plotdat_reformatted[["treatment"]])
}

#We get each combination of bacteria and treatment so we can calculate means and standard deviations for
#each set of replicates
unique_combos <- unique(plotdat_reformatted[,c("Bacteria","treatment")])
means <- c()
sds <- c()
for(row in rownames(unique_combos)){
  correct_rows <- plotdat_reformatted[["Bacteria"]]==unique_combos[row,"Bacteria"] &   plotdat_reformatted[["treatment"]]==unique_combos[row,"treatment"]
  
  
  measures <- plotdat_reformatted[correct_rows,"inhib"]
  means <- c(means,mean(measures,na.rm = T))
  sds <- c(sds,sd(measures,na.rm = T))
}

#attaches the mean and sd data to the unique combinations to get means, lower bounds, and upper bounds
fig4a_dat <- cbind(unique_combos,'means'=means,'sds'=sds,'upper_bound'=means+sds,'lower_bound'=means-sds,'Growth_conditions'=rep('Aerobically\ngrown'))
adj_pvals <- c()
pvals <- c()
#we calculate if differences are significant for the aerobic treatments
bacteria <- unique(plotdat_reformatted[,"Bacteria"])
non_control_treatments <- unique(plotdat_reformatted[plotdat_reformatted[["treatment"]]!='Control',"treatment"])
for(bact in bacteria){
  for(treatment in non_control_treatments){
    result <- t.test(as.numeric(plotdat_unformatted[bact,paste0(treatment,'.',seq(1,3))]),as.numeric(plotdat_unformatted[bact,paste0('Control.',seq(1,3))]))
    pvals <- rbind(pvals,cbind('pval'=result[["p.value"]],'bact'=bact,"treatment"=treatment))
  }
  adj_pvals <- c(adj_pvals,as.numeric(pvals[pvals[,"bact"]==bact,"pval"]))
  
}
pvals <- cbind(pvals,adj_pvals)

#now we load and format the anaerobic panel
#we load the data in an unformatted version
plotdat_unformatted <- read.csv(paste0(data_folder,"SNIPR001 v Anearobic panel.csv"), row.names=1)
#we reformat with melt
plotdat_unformatted <- reshape2::melt(reshape2::melt(as.matrix(plotdat_unformatted)))
plotdat_unformatted[["variable"]] <- as.character(plotdat_unformatted[["variable"]])
plotdat_unformatted[["Var2"]] <- as.character(plotdat_unformatted[["Var2"]])
#changes the names back
for(name in c(SNIPR001_phages,'Control','SNIPR001')){
  plotdat_unformatted[["Var2"]] <- gsub(paste0(paste(name,seq(1,10),sep='.'),collapse='|'),name,plotdat_unformatted[["Var2"]])
}
plotdat_unformatted <- as.data.frame(plotdat_unformatted[,-3])

colnames(plotdat_unformatted) <- c('Bacteria','treatment','inhib')

#we calc the mean for each unique combo
unique_combos <- unique(plotdat_unformatted[,c("Bacteria","treatment")])
means <- c()
sds <- c()
for(row in rownames(unique_combos)){
  correct_rows <- plotdat_unformatted[["Bacteria"]]==unique_combos[row,"Bacteria"] & plotdat_unformatted[["treatment"]]==unique_combos[row,"treatment"]
  
  
  measures <- plotdat_unformatted[correct_rows,"inhib"]
  means <- c(means,mean(measures,na.rm = T))
  sds <- c(sds,sd(measures,na.rm = T))
}



pvals <- c()
adj_pvals <- c()
#we calculate if differences are significant for the aerobic treatments
bacteria <- unique(as.character(plotdat_unformatted[,"Bacteria"]))
non_control_treatments <- unique(plotdat_unformatted[["treatment"]])[unique(plotdat_unformatted[["treatment"]])!='Control']
for(bact in bacteria){
  for(treatment in non_control_treatments){
    treatment_reduction <- plotdat_unformatted[plotdat_unformatted[,"treatment"]==treatment & plotdat_unformatted[,"Bacteria"]==bact,"inhib"]
    untreatment_reduction <- plotdat_unformatted[plotdat_unformatted[,"treatment"]=='Control' & plotdat_unformatted[,"Bacteria"]==bact,"inhib"]
    result <- t.test(treatment_reduction,untreatment_reduction)
    pvals <- rbind(pvals,cbind('pval'=result[["p.value"]],'bact'=bact,"treatment"=treatment))
  }
  adj_pvals <- c(adj_pvals,as.numeric(pvals[pvals[,"bact"]==bact,"pval"]))
}
pvals <- cbind(pvals,adj_pvals)

#attaches the mean and sd data
fig4b_dat <- cbind(unique_combos,'means'=means,'sds'=sds,'upper_bound'=means+sds,'lower_bound'=means-sds,'Growth_conditions'=rep('Anaerobically\ngrown'))

#now we merge the data 
plotdat <- as.data.frame(rbind(fig4b_dat,fig4a_dat))

#we add whether they were added for being e coli or for testing against clinically relevant strains
bacteria_type <- rep('Clinically relevant bacteria',nrow(plotdat))
bacteria_type[plotdat[["Bacteria"]]%in%c("b2480")] <- 'E. Coli'
plotdat <- cbind(plotdat,'bacteria_type'=bacteria_type)

#here is how we should change the labels
plot_labels <- c("E. faecalis"=substitute(paste(' ',italic('E. faecalis'), ' ',sep = '')),
                 "A. baylyi"=expression(italic("A. baylyi")),
                 "B. coagulans"=expression(italic("B. coagulans")),
                 "S. thermophilus"=expression(italic("S. thermophilus")),
                 "S. aureus"=expression(italic("S. aureus")),
                 "K. pneumonia"=expression(italic("K. pneumonia")),
                 "B. thetaiotaomicron"=expression(italic("B. thetaiotaomicron")),
                 "B. vulgatus"=expression(italic("B. vulgatus")),
                 "E. limosum"=substitute(paste(' ',italic('E. limosum'), ' ',sep = '')),
                 'b2480'=substitute(paste(italic('E. coli'), ' b2480',sep = '')))

#we set the y axis labels to something nicer
y_breaks <- c(seq(-50,50))
y_break_names <- c(to_tenth_power_labels(seq(-50,50)))


plotdat[["bacteria_type"]] <- factor(plotdat[["bacteria_type"]],levels = c("Clinically relevant bacteria","E. Coli"),labels = c("'Clinically relevant bacteria'","paste(' ',italic('E. Coli'),' ',sep='')"))
plotdat[["Growth_conditions"]] <- factor(plotdat[["Growth_conditions"]],levels = c("Anaerobically\ngrown","Aerobically\ngrown"),labels = c("'Anaerobically\ngrown'","'Aerobically\ngrown'"))



plotdat_ecoli <- plotdat[!grepl('Clinically',plotdat[["bacteria_type"]]),]

plotdat <- plotdat[grepl('Clinically',plotdat[["bacteria_type"]]),]


ylimits <- c(-4.6,2.5)
width_transform <- 3/5*0.775
#we make two different facet grid plots with either the aerobic or the anaerobic data
p_anaerob <- ggplot(data=plotdat[grepl("Anaerobically",plotdat[["Growth_conditions"]]),],aes(x = Bacteria,y=means,fill=treatment))#initializes plot
p_anaerob <- p_anaerob + geom_bar(stat='identity',position = position_dodge(),width = width_transform)+scale_fill_manual(values = phage_colors)#adds bars and colors them correctly
p_anaerob <- p_anaerob + geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=.2,position=position_dodge(width_transform))#adds the error bars
p_anaerob <- p_anaerob + custom_theme+theme(plot.margin = margin(t=0.5,0,0,0,'cm'),strip.background.x=element_rect(fill="#FFFFFF"),strip.background.y=element_blank(),strip.text.y = element_blank(),legend.title = element_blank(),legend.box.background = element_blank(),legend.text = element_text(size=text_size),axis.title.x=element_blank())#Changes theme settings
p_anaerob <- p_anaerob + scale_y_continuous(breaks=y_breaks,labels = y_break_names,limits=ylimits)+scale_x_discrete(labels=plot_labels)#changes the axis text
p_anaerob <- p_anaerob + ylab('ΔCFU/mL\n4h-0h')+xlab('Strain')#adds the y and y axis title
p_anaerob <- p_anaerob + facet_grid(Growth_conditions~bacteria_type,labeller = label_parsed,scales = 'free',space='free')+theme(strip.text=element_text(size = text_size))#facets the plot into e coli and non-ecoli


#we make two different facet grid plots with either the aerobic or the aerobic data
p_aerob <- ggplot(data=plotdat[!grepl("Anaerobically",plotdat[["Growth_conditions"]]),],aes(x = Bacteria,y=means,fill=treatment))#initializes plot
p_aerob <- p_aerob+geom_bar(stat='identity',position = position_dodge())+scale_fill_manual(values = phage_colors)#adds bars and colors them correctly
p_aerob <- p_aerob+geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=.2,position=position_dodge(0.9))#adds error bars
p_aerob <- p_aerob + custom_theme+theme(strip.text.y = element_blank(),plot.margin = margin(b=0,0,0,0,'cm'),strip.background.y=element_blank(),strip.background.x = element_blank(),strip.text.x = element_blank(),legend.title = element_blank(),legend.box.background = element_blank(),legend.text = element_text(size=text_size))#Changes theme settings
p_aerob <- p_aerob + scale_y_continuous(breaks=y_breaks,labels = y_break_names,limits=ylimits)+scale_x_discrete(labels=plot_labels)#changes the axis text
p_aerob <- p_aerob + ylab('ΔCFU/mL\n4h-0h')+xlab('Strain')#adds the y and y axis title
p_aerob <- p_aerob+facet_grid(Growth_conditions~bacteria_type,labeller = label_parsed,scales = 'free',space='free')+theme(strip.text=element_text(size = text_size))#facets the plot into e coli and non-ecoli


#we combine the two plots to show both the aerobic and anaerobic panel at the same time
comboplot <- ggpubr::ggarrange(p_anaerob,p_aerob,nrow=2,heights=c(50,50),align='v',common.legend = F,legend = 'none')+theme(legend.text = element_text(size=text_size))


width_transform <- 1
p_anaerob <- ggplot(data=plotdat_ecoli[grepl("Anaerobically",plotdat_ecoli[["Growth_conditions"]]),],aes(x = Bacteria,y=means,fill=treatment))#initializes plot
p_anaerob <- p_anaerob + geom_bar(stat='identity',position = position_dodge())+scale_fill_manual(values = phage_colors)#adds bars and colors them correctly
p_anaerob <- p_anaerob + geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=.2,position=position_dodge(0.9*width_transform))#adds the error bars
p_anaerob <- p_anaerob + custom_theme+theme(plot.margin = margin(t=0.5,0,0,0,'cm'),strip.background.x=element_rect(fill="#FFFFFF"),strip.background.y=element_blank(),strip.text.y = element_text(angle=0,hjust=0.5),legend.title = element_blank(),legend.box.background = element_blank(),legend.text = element_text(size=text_size),axis.title.x=element_blank())#Changes theme settings
p_anaerob <- p_anaerob + scale_y_continuous(breaks=y_breaks,labels = y_break_names,limits=ylimits)+scale_x_discrete(labels=plot_labels)#changes the axis text
p_anaerob <- p_anaerob + ylab('ΔCFU/mL\n4h-0h')+xlab('Strain')#adds the y and y axis title
p_anaerob <- p_anaerob + facet_grid(Growth_conditions~bacteria_type,labeller = label_parsed,scales = 'free',space='free')+theme(strip.text=element_text(size = text_size))#facets the plot into e coli and non-ecoli
p_anaerob <- p_anaerob + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

#we make two different facet grid plots with either the aerobic or the aerobic data
p_aerob <- ggplot(data=plotdat_ecoli[!grepl("Anaerobically",plotdat_ecoli[["Growth_conditions"]]),],aes(x = Bacteria,y=means,fill=treatment))#initializes plot
p_aerob <- p_aerob+geom_bar(stat='identity',position = position_dodge())+scale_fill_manual(values = phage_colors)#adds bars and colors them correctly
p_aerob <- p_aerob+geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=.2,position=position_dodge(0.9))#adds error bars
p_aerob <- p_aerob + custom_theme+theme(strip.text.y = element_text(angle=0,hjust=0.5),plot.margin = margin(b=0,0,0,0,'cm'),strip.background.y=element_blank(),strip.background.x = element_blank(),strip.text.x = element_blank(),legend.title = element_blank(),legend.box.background = element_blank(),legend.text = element_text(size=text_size))#Changes theme settings
p_aerob <- p_aerob + scale_y_continuous(breaks=y_breaks,labels = y_break_names,limits=ylimits)+scale_x_discrete(labels=plot_labels)#changes the axis text
p_aerob <- p_aerob + ylab('ΔCFU/mL\n4h-0h')+xlab('Strain')#adds the y and y axis title
p_aerob <- p_aerob+facet_grid(Growth_conditions~bacteria_type,labeller = label_parsed,scales = 'free',space='free')+theme(strip.text=element_text(size = text_size))#facets the plot into e coli and non-ecoli
p_aerob <- p_aerob + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())+xlab('')

comboplot_e_coli <- ggpubr::ggarrange(p_anaerob,p_aerob,nrow=2,heights=c(50,50),align='v',common.legend = T,legend = 'right')+theme(legend.text = element_text(size=text_size))



comboplot_comboplot <- ggpubr::ggarrange(comboplot,comboplot_e_coli,nrow=1,widths=c(0.45,0.215),align='h',common.legend = T,legend = 'right')+theme(legend.text = element_text(size=text_size))

ggsave(comboplot_comboplot,filename = paste0(fig_folder,'Supplementary figure 15.pdf'),height=5,width=10,device=cairo_pdf)
ggsave(comboplot_comboplot,filename = paste0(fig_folder,'Supplementary figure 15.png'),height=5,width=10)



#we output relevant summary stats
outmat <- rbind(fig4a_dat[,c("Bacteria","treatment","means","sds","Growth_conditions")],fig4b_dat[,c("Bacteria","treatment","means","sds","Growth_conditions")])
outmat[["Growth_conditions"]] <- gsub('\n',' ',outmat[["Growth_conditions"]])
outmat <- cbind(outmat,'replicates'=4)

openxlsx::write.xlsx(outmat,file = paste0(summary_stats_folder,'Supplementary figure 15.xlsx'))

#########################
#Supplementary figure 16#
#########################

#We want to make a plot that illustrates the distribution of samples from the JMI panel
#in both year of sampling and the continent in which the sample was obtained.
#we load the JMI table
jmi_resistance_dat <- read_excel(paste0(data_folder,"JMI_strain_characteristics.xlsx"))

#we distribution of the continents the samples were obtained at
continent_dat <- as.data.frame(table(jmi_resistance_dat[["Continent"]]))
continent_dat <- cbind(continent_dat,'type'=rep('Continent',nrow(continent_dat)))

#and the distribution of the years the samples were obtained at
year_dat <- as.data.frame(table(jmi_resistance_dat[["Study Year"]]))
year_dat <- cbind(year_dat,'type'=rep('Year of sampling',nrow(year_dat)))

#we want both geographic and temporal distribution in one faceted plot so we gather the two
plotdat <- rbind(year_dat,continent_dat)

#we format the order of the year/continents so the bars are always decreasing
continent_order <- as.character(continent_dat[["Var1"]][order(continent_dat[["Freq"]],decreasing = T)])
year_order <- as.character(year_dat[["Var1"]][order(year_dat[["Freq"]],decreasing = T)])
plotdat[["Var1"]] <- factor(as.character(plotdat[["Var1"]]),levels=c(continent_order,year_order))


#We plot the data as a facteted bar plot
p <- ggplot(data=plotdat,aes(x=Var1,y=Freq,fill=type))+#initializes the plot
  geom_bar(stat='identity')+#Adds the bars
  facet_wrap(~type,scales = 'free')+#facets and makes sure years arent plotted in continent and vice versa
  custom_theme+#sets custom theme
  theme(panel.grid.major.y=element_blank(),#removes the major panel line on y axis and
        axis.title.y=element_blank(),#removes y axis title
        axis.title.x = element_blank(),#removes x axis title
        legend.position = 'none',#removes legend
        plot.tag.position = 'bottomright',plot.tag = element_text(size=text_size,vjust=0,hjust=0,margin=margin(t = -12.5,l=4,0,0,'pt')))+#sets the y tag to indicate the x axis on the bottom right
  coord_flip()+#flips coordinates
  labs(tag = 'Number of\nstrains')+#sets the tag text
  scale_fill_manual(values = c(SNIPR_color1,SNIPR_color2))
#saves it
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 16.pdf'),width = 5,height=3.5)
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 16.png'),width = 5,height=3.5)


outmat <- plotdat[,c("Var1","Freq","type")]
outmat_continent <- outmat[outmat[,'type']=='Continent',]
outmat_year <- outmat[outmat[,'type']!='Continent',]

openxlsx::write.xlsx(outmat_continent,file = paste0(summary_stats_folder,'supplementary figure 16 continent.xlsx'))
openxlsx::write.xlsx(outmat_year,file = paste0(summary_stats_folder,'supplementary figure 16 year'))


#########################
#Supplementary figure 17#
#########################
#We read the JMI table
JMI_phylogrouping <- read.delim(paste0(data_folder,"JMI_phylogroup.tsv"), header=FALSE)
colnames(JMI_phylogrouping) <- c('strain','alleles','allele_pos_neg','sig_gene','phylogroup','originating_file')

#Loads the JMI MLST data
JMI_MLST_data <- read.delim(paste0(data_folder,"JMI_MLST.tsv"), header=T)
top_N_MLSTs <- 15
remove_names <- names(sort(table(JMI_MLST_data[,"MLST_type"]),decreasing = T))[top_N_MLSTs:length(unique(JMI_MLST_data[["MLST_type"]]))]
JMI_MLST_data[JMI_MLST_data[,"MLST_type"]%in%remove_names,"MLST_type"] <- 'Other'

#we get a table of the phylogroup
phylogroup_table=table(JMI_phylogrouping[["phylogroup"]])

#Now we find the frequencies of MLSTs, again using the table from the previous plot
MLST_table=table(JMI_MLST_data[["MLST_type"]])
#we do not include the MLSTs we disincluded as we only plot the most frequent MLSTs in this plot
MLST_table <- MLST_table[names(MLST_table)!='Other']

plotdat <- rbind(cbind(names(phylogroup_table),unname(phylogroup_table),rep('Phylogroup',length(phylogroup_table)))
                 ,cbind('names'=names(MLST_table),'freq'=unname(MLST_table),'kind'=rep('Top 14 MLSTs',length(MLST_table))))

#formatting
plotdat <- as.data.frame(plotdat)
plotdat[["kind"]] <- factor(plotdat[["kind"]],levels=as.character(unique(plotdat[["kind"]])))
plotdat[["names"]] <- gsub('or','\nor',gsub('clade','Clade ',plotdat[["names"]]))

plotdat[["names"]] <- factor(plotdat[["names"]],levels = plotdat[["names"]][rev(order(as.numeric(plotdat[["freq"]])))])
plotdat[["freq"]] <- as.numeric(plotdat[["freq"]])

#we wish to coordinate the color scheme with the one used in the tree above

#We indicate the colors that we use for MLSTs
MLST_colors <- c(
  "#087546",
  "#45f7a4",
  "#23BA2B",
  "#9DD494",
  "#daf745",
  "#B9A3FF",
  "#1F35FF",
  "#3576CC",
  "#61DAFF",
  "#ABFFF6",
  "#FFCC26",
  "#FF9600",
  "#CC6B3B",
  "#8C3F38"
)
names(MLST_colors) <- sort(as.character(plotdat[grepl('MLSTs',plotdat[["kind"]]),"names"]))


colors_to_use <- c(MLST_colors,phylogroup_colors)

#now we do da plotting
p <- ggplot(data=plotdat,aes(x=names,y=freq,fill=names))+geom_bar(stat='identity')+#starts the bars
  facet_wrap(~kind,scales = 'free')+#adds facets
  custom_theme+#sets presets to something nicer
  theme(legend.title=element_blank(),legend.position = 'none',axis.title.x=element_blank())+ylab('Number of strains')+#adjusts some beauty stuff 
  scale_fill_manual(values = colors_to_use)#sets snipr color scale :)
p <- p+theme(panel.grid.minor.y = element_blank(),panel.grid.major.y=element_line())

ggsave(p,filename = paste0(fig_folder,'Supplementary figure 17.pdf'),height =3,width=9)  
ggsave(p,filename = paste0(fig_folder,'Supplementary figure 17.png'),height =3,width=9)  


phylogroup_data <- plotdat[plotdat[,"kind"]=='Phylogroup',]
MLST_data <- plotdat[plotdat[,"kind"]!='Phylogroup',]

openxlsx::write.xlsx(phylogroup_data,file = paste0(summary_stats_folder,'supplementary figure 17 phylogroup.xlsx'))
openxlsx::write.xlsx(MLST_data,file = paste0(summary_stats_folder,'supplementary figure 17 MLST.xlsx'))


#########################
#Supplementary figure 18#UNFINISHED
#########################
#we wish to make a plot showing immune cell counts when SNIPR001 was administered
porcine_immune_cell_counts <- read_excel(paste0(data_folder,"pig_immune_cell_counts.xlsx"))

#we create the data to create dot points
plotdat <- reshape2::melt(as.data.frame(porcine_immune_cell_counts))

#the time variables are in a bit of weird format computationally,
#because the time is noted in multiple units - hours, days, and then "right before administration" as pre-administration
#we would like to have these chronoligically ordered
#first we get the unordered data
#we do this by converting the unordered data to something on a more unified scale and then we apply the order from that scale
#to the character vector
#first we get the character vector
time_vars_unordered <- sort(unique(plotdat[["time"]]))
#and we create a copy of it
time_vars_unordered_for_ordering <- time_vars_unordered
#then we set pre-administration as 0h, because that is what it is
time_vars_unordered_for_ordering[time_vars_unordered_for_ordering=='Pre-administration'] <- "0h"
#then we remove the day and hour indicators - it doesn't actually matter that much
#if it is -7 days or hours it should still be before -6 days/hours
unitless_time_variable <- as.numeric(gsub('Day ','',gsub('h','',time_vars_unordered_for_ordering)))
#then we order them and apply that ordering to the character vector
time_vars_ordered <- time_vars_unordered[order(unitless_time_variable)]

#then we sort the plot data according to this order
plotdat[["time"]] <- factor(plotdat[["time"]],levels = time_vars_ordered)

#we set the color variables for controll and SNIPR001
color_variable <- rep('Vehicle',nrow(plotdat))
color_variable[grepl('SNIPR001',plotdat[["variable"]])] <- 'SNIPR001'

#we add the colors to the plot data
plotdat <- cbind(plotdat,'color_variable'=color_variable)

#we calculate a set of averages to create a trend-line for the plot
mean_data <- plotdat %>% group_by(Cell_type,time,color_variable) %>% summarise('mean'=mean(value,na.rm=T))

#and fetch the colors we wish to use in the plot
local_colors <- c(phage_colors,'Vehicle'=phage_colors[['Control']])

#we initialize the plot
p <- ggplot()
#we add a trend-line that shows the average of the cell counts
p <- p + geom_path(data=mean_data,aes(x=time,y=mean,group=color_variable,color=color_variable))
#then we add the individual points as well
p <- p + geom_jitter(plotdat,mapping = aes(x=time,y=value,group=color_variable,color=color_variable),alpha=0.25,width = 0.15)
#and we facet wrap the whole deal according to cell type and allow the y axes to have different scales
p <- p + facet_wrap(vars(Cell_type),scales = 'free_y')
#we set the color scheme
p <- p + scale_color_manual('Treatment',values = local_colors)
#we change the axis titles
p <- p + ylab(expression('Cell count µL'^"-1"))+xlab('Time')
#apply our custom theme and angle the x axis labels 
p <- p+custom_theme+theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1))

#we save the plot
ggsave(p,filename = paste0(fig_folder,"Supplementary figure 18.pdf"),width=12,height=7)
ggsave(p,filename = paste0(fig_folder,"Supplementary figure 18.png"),width=12,height=7)

#we output summary stats
outdat <- plotdat %>% group_by(time,Cell_type) %>% summarise('mean'=mean(value,na.rm=T),'sd'=sd(value,na.rm=T))
write.xlsx(outdat,file = paste0(summary_stats_folder,'supplementary figure 18 summary stats.xlsx'))

########################
#Supplementary figure 19#
########################
#we load the data for serum CRP during dosage of SNIPR001 and vehicle
plotdat_unformatted <- as.data.frame(read_excel(paste0(data_folder,"CRP in pigs after oral dosing.xlsx")))

#We create a function for converting the day/hour format into something more consistent
convert_day_to_hour <- function(day_string){
  if(grepl('Day ',day_string)){
    return(paste0(as.character(as.numeric(gsub('Day ','',day_string))*24),'h'))
  }else{
    return(day_string)
  }
}

#converts all the time-points to hour format
hour_string <- c()
for(day_string in plotdat_unformatted[["Time relative to dosing"]]){
  hour_string <- c(hour_string,convert_day_to_hour(day_string))
}

#The pre-measurement was taken immediately before administration, so we set this value to 0
hour_string[hour_string=='PRE'] <- "0h"

#sets the row values to be the number of hours since administration 
rownames(plotdat_unformatted) <- gsub('h','',hour_string)
#we convert to a more ggplot friendly format while omitting the time-point data, since it is already included in the row
plotdat <- reshape2::melt(as.matrix(plotdat_unformatted)[,-1])
colnames(plotdat) <- c('time','dosed','measure')
plotdat[["dosed"]] <- as.character(plotdat[["dosed"]])

#we remove the numbering of replicates
for(name in c('SNIPR001','Vehicle')){
  plotdat[["dosed"]] <- gsub(paste0(paste0(name,' ',seq(1,99)),collapse='|'),name,plotdat[["dosed"]])
}
#and convert the serum CRP measures to numeric
plotdat[["measure"]] <- as.numeric(plotdat[["measure"]])

#we wish to change the way the x-labels are displayed by using two formats: 
#one hour based format for 24 and fewer hours, and a day format for more than 24 hours
#We wish for the last label in hour format to say x hours and the last time-point in
#day format to say n days
#gets the breaks
breaks <- unique(plotdat[["time"]])
break_names <- breaks
#changes divides the time-points that are higher than 24 by 24
day_format <- break_names>24
break_names[day_format] <- break_names[day_format]/24

#adds "hours" to the highest pre-day timepoint and "days" to the highest day format
break_names[match(T,day_format)-1] <- paste0(break_names[match(T,day_format)-1],' hours')
break_names[length(break_names)] <- paste0(break_names[length(break_names)],' days')

#calls the pre time-point "pre-administration"
break_names[break_names=='0'] <- 'Pre-   \nadministration    '
break_names[break_names=='2'] <- '  2'

#changes the y-axis labels
#if we only use 10^n timepoints here, we will only have two guiding lines
#which looks kind of awkward, so we will add 1.2, 1.4, 1.6, 1.8 times 10^n
y_breaks <- c(10^seq(0,7))
y_breaks <- c(y_breaks,y_breaks*2,y_breaks*4,y_breaks*6,y_breaks*8)
#we format the labels properly
y_break_labels <- parse(text=c(
  paste0('1%*%10^',seq(0,7)),
  paste0('2%*%10^',seq(0,7)),
  paste0('4%*%10^',seq(0,7)),
  paste0('6%*%10^',seq(0,7)),
  paste0('8%*%10^',seq(0,7))
))

#removes the NA values from the plot
plotdat <- plotdat[!is.na(plotdat[,"measure"]),]

#we wish for the x-axis to be equidistant, so we reformat the number scale to a factor scale
plotdat[["time"]] <- factor(plotdat[["time"]],levels = sort(unique(plotdat[["time"]])))

#starts plotting
p <- ggplot(data=plotdat,mapping = aes(time,y = measure,color=dosed))#boots the plot
#we wish to add the control and SNIPR001 points slightly offset, so that is what we do
p <- p+geom_boxplot(data = plotdat[plotdat[["dosed"]]=='SNIPR001',],mapping = aes(x=time,y = measure,color=dosed),position=position_nudge(x = 0.15),width=0.15)#adds the points for SNIPR001
p <- p+geom_boxplot(data = plotdat[plotdat[["dosed"]]!='SNIPR001',],mapping = aes(x=time,y = measure,color=dosed),position=position_nudge(x = -0.15),width=0.15)#adds the points for SNIPR001
p <- p+geom_point(data = plotdat[plotdat[["dosed"]]=='SNIPR001',],mapping = aes(x=time,y = measure,color=dosed),position=position_nudge(x = 0.15),size=1)#adds the points for SNIPR001
p <- p+geom_point(data = plotdat[plotdat[["dosed"]]!='SNIPR001',],mapping = aes(x=time,y = measure,color=dosed),position=position_nudge(x = -0.15),size=1)#adds the points for vehicle
p <- p + scale_x_discrete(breaks=breaks,labels =break_names)#changes the x axis labes to reflect our naming scheme
p <- p + scale_y_continuous(breaks=y_breaks,labels =y_break_labels,trans = 'log10')#and changes the y axis labels
p <- p+custom_theme+theme(panel.grid.minor = element_blank(),legend.position = 'none',strip.text = element_text(size=text_size,angle=0),panel.grid.major.y = element_line()) #set and removes the y axis title
p <- p+xlab('')+ylab('Serum CRP (ng/mL)')#sets labels instead of the y axis label
p <- p + scale_color_manual(values = c('SNIPR001'=SNIPR_green,'Vehicle'=SNIPR_grey))

#saves the plot
ggsave(plot = p,filename = paste0(fig_folder,'Supplementary figure 19.pdf'),width=10,height=3)
ggsave(plot = p,filename = paste0(fig_folder,'Supplementary figure 19.png'),width=10,height=3)



#we generate some summary statistics for the plot
unique_dat <- unique(plotdat[,c("time","dosed")])
outdat <- c()
for(row in rownames(unique_dat)){
  time <- as.character(unique_dat[row,"time"])
  dosed <- as.character(unique_dat[row,"dosed"])
  
  measures <- log(plotdat[plotdat[,"time"]==time & plotdat[,"dosed"]==dosed,"measure"],10)
  
  outdat <- rbind(outdat,
                  cbind("time"=time,
                        'dosed'=dosed,
                        'mean (log10)'=mean(measures),
                        'sd (log10)'=sd(measures),
                        'n'=length(measures)))
}

openxlsx::write.xlsx(outdat,file = paste0(summary_stats,'Supplementary figure 19.xlsx'))

