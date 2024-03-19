####--------Packages needed--------------####

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(cowplot)
library(MetBrewer)

Dataset <- read.csv("data/CTC_Dataset_complete.csv")
Dataset_cores <- read.csv("data/CTC_Dataset_cores.csv")

####--------PCA of blade and bladelet cores--------------####

#1 Create the dataset ####
PCA_Cores <- Dataset_cores %>%
  filter(Laminar_y_n == "TRUE",
         Core_classification_2 != "Shatter" & Core_classification_2 != "Initial",
         Layer != "ars"
         ) %>%
  select(Layer, Core_classification_2, Volume, Ratio_fsl_thick, Ratio_fsl_width, flak_surf_length, `mean_angle`) %>%
  rename(`Mean angle` = mean_angle, `FSL/T` = Ratio_fsl_thick, `FSL` = flak_surf_length, `FSL/W` = Ratio_fsl_width) %>%
  mutate(Core_classification_2 = recode(Core_classification_2, `Narrow-sided` = 'Narrow/Burin', `Burin core` = 'Narrow/Burin')) %>%
  na.omit()

#2 Correlation between the quantitative variables selected for analysis ####
cor_matrix_core <- cor(PCA_Cores[,3:7]) 
cor_matrix_core

#3 PCA in the FactoMineR package ####
PCA_core <- FactoMineR::PCA(PCA_Cores, quali.sup=1:2, scale.unit = T, graph = T)

PCA_core$eig # Eigenvalues

#4 Results of the PCA ####
dimdesc_PCA_cores <- dimdesc(PCA_core, axes=c(1:3))
print(dimdesc_PCA_cores)

#5 First visualization plot ####
plot.PCA(PCA_core)

#6 Importance of the diffeent components ####

PCA_cores_PC_imp <- fviz_eig(PCA_core, addlabels = T,
                             repel=T,
                             ggtheme=theme(text=element_text(size=14),
                                           plot.title = element_blank(),
                                           axis.title = element_text(size=14),
                                           axis.text = element_text(size=12))) + 
  ylim(0, 50) +
  theme_minimal_grid() +
  labs(x = "Principal Components", y = "% Explained Variance") +
  theme(plot.title = element_blank())

PCA_cores_PC_imp

# ggsave(filename="PCA_cores_PC_imp.png", plot=PCA_cores_PC_imp, device="png",
#        height=4, width=5, units="in", dpi=500)


#7 Contribution of the quantitative variables to the first two components ####

PCA_cores_PCs_expl <- fviz_pca_var(PCA_core,
                                   col.var = "contrib",
                                   axes =c(1,2),
                                   labelsize=5,repel = T, ggtheme=theme(text=element_text(size=14),
                                                                        axis.title = element_text(size=14),
                                                                        axis.text = element_text(size=12),
                                                                        legend.position = "bottom")) +
  labs(y = "PC2 (21.6%)", x = "PC1 (47.2%)", colour = "Contrib") +
  theme_minimal_grid() +
  theme(plot.title = element_blank()) +
  scale_color_gradientn(colors=met.brewer("Isfahan1"))


PCA_cores_PCs_expl

# ggsave(filename="PCA_cores_PCs_expl.png", plot=PCA_cores_PCs_expl, device="png",
#        height=4, width=5, units="in", dpi=500)

#8 Plotting PC1 to PC2 visualizing the core type ####

PCA1.2a <- factoextra::fviz_pca_ind(PCA_core,
             axes=c(1, 2), label="none",
             addEllipses=T, ellipse.type="confidence",
             habillage=c(2), pointsize=3, labelsize=10, repel=T,
             ggtheme = theme(text=element_text(size=16),
                             axis.title = element_text(size=16),
                             axis.text = element_text(size=14)),
             legend.title=element_text("Core type")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  scale_fill_manual(values=met.brewer("Lakota", 5)) +
  scale_color_manual(values=met.brewer("Lakota",5)) +
  # scale_color_met_d("Veronese") +
  # scale_fill_met_d("Veronese") +
  labs(x="PC1 (47.2%)", y="PC2 (21.6%)") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank()) +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))

#9 Plotting PC1 to PC2 visualizing the unit of provenience ####

PCA1.2b <- fviz_pca_ind(PCA_core,
             axes=c(1, 2), label="none",
             addEllipses=T, ellipse.type="confidence",
             habillage=c(1), pointsize=3, labelsize=10, repel=T,
             ggtheme = theme(text=element_text(size=16),
                             axis.title = element_text(size=16),
                             axis.text = element_text(size=14)),
             legend.title=element_text("Unit")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  scale_fill_manual(values=met.brewer("Navajo", 2)) +
  scale_color_manual(values=met.brewer("Navajo",2)) +
  labs(x="PC1 (47.2%)", y="PC2 (21.6%)") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))
  
#10 Merging and saving the figures for publication ####

ggsave(filename = "output/figures/Figure_5.tiff", width = 8, height = 10, units = "in", bg = "white", dpi=300, plot=(
  ggdraw() +
    draw_plot(PCA_cores_PC_imp, x = 0, y = 0.7, width = .5, height = .30) +
    draw_plot(PCA_cores_PCs_expl, x = .5, y = 0.7, width = .5, height = .30) +
    draw_plot(PCA1.2b, x = 0, y = 0.35, width = 0.84, height = .35)  +
    draw_plot(PCA1.2a, x=0, y=0, width=1, height= .35)+
    draw_plot_label(label = c("a", "b", "c", "d"),
                    x = c(0, .5, 0,0), y = c(1, 1, .7, .35), size=14)
))


#11 Contribution of the quantitative variables to the first and third components ####

PCA_cores_PCs1.3_expl <- fviz_pca_var(PCA_core,
                                   col.var = "contrib",
                                   axes =c(1,3),
                                   labelsize=5,repel = T, ggtheme=theme(text=element_text(size=14),
                                                                        axis.title = element_text(size=14),
                                                                        axis.text = element_text(size=12),
                                                                        legend.position = "bottom")) +
  labs(y = "PC3 (19.7%)", x = "PC1 (47.2%)", colour = "Contrib") +
  theme_minimal_grid() +
  theme(plot.title = element_blank()) +
  scale_color_gradientn(colors=met.brewer("Isfahan1")) #Change name according to the SI file


PCA_cores_PCs1.3_expl

# ggsave(filename="PCA_cores_PCs_expl.png", plot=PCA_cores_PCs_expl, device="png",
#        height=4, width=5, units="in", dpi=500)

#8 Plotting PC1 to PC2 visualizing the core type ####

PCA1.3a <- factoextra::fviz_pca_ind(PCA_core,
                                    axes=c(1, 3), label="none",
                                    addEllipses=T, ellipse.type="confidence",
                                    habillage=c(2), pointsize=3, labelsize=10, repel=T,
                                    ggtheme = theme(text=element_text(size=16),
                                                    axis.title = element_text(size=16),
                                                    axis.text = element_text(size=14)),
                                    legend.title=element_text("Core type")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  scale_fill_manual(values=met.brewer("Lakota", 5)) +
  scale_color_manual(values=met.brewer("Lakota",5)) +
  # scale_color_met_d("Veronese") +
  # scale_fill_met_d("Veronese") +
  labs(x="PC1 (47.2%)", y="PC3 (19.7%)") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank()) +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))

#9 Plotting PC1 to PC2 visualizing the unit of provenience ####

PCA1.3b <- fviz_pca_ind(PCA_core,
                        axes=c(1, 3), label="none",
                        addEllipses=T, ellipse.type="confidence",
                        habillage=c(1), pointsize=3, labelsize=10, repel=T,
                        ggtheme = theme(text=element_text(size=16),
                                        axis.title = element_text(size=16),
                                        axis.text = element_text(size=14)),
                        legend.title=element_text("Unit")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  scale_fill_manual(values=met.brewer("Navajo", 2)) +
  scale_color_manual(values=met.brewer("Navajo",2)) +
  labs(x="PC1 (47.2%)", y="PC3 (19.7%)") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))

#10 Merging and saving the figures for publication ####

ggsave(filename = "output/figures/Figure_S6.png", width = 8, height = 10, units = "in",dpi=300, plot=(
  ggdraw() +
    draw_plot(PCA_cores_PCs1.3_expl, x = 0, y = 0.7, width = 1, height = .30) +
    draw_plot(PCA1.3b, x = 0, y = 0.35, width = 0.84, height = .35)  +
    draw_plot(PCA1.3a, x=0, y=0, width=1, height= .35)+
    draw_plot_label(label = c("A", "B", "C"),
                    x = c(0.2, 0,0), y = c(1, .7, .35), size=14)
))
