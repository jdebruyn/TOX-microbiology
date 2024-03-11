#Analysis of PMI model results

library(readxl)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(tidyverse)

#first let's plot the environmental metadata for the samples included in the models

#load data
soil_meta = read_excel("C:/Users/Allison/OneDrive - University of Tennessee/DeBruyn Lab/NIJ_TOX/TOX_data_all.xlsx", sheet = "Compiled")
soil_meta_seq = soil_meta %>% filter(`DNA-EEA_sample` == "Y") #remove non-sequencing data
soil_meta_seq_env = soil_meta_seq %>% select(Sample, Donor, Treatment, ADH_10_actual, Study_day, Temperature,'Gravimetric Moisture',pH,EC,BG_avg,NAG_avg,PHOS_avg,LAP_avg) #select only the environmental data used as Random Forest predictors

colpalette2= c("khaki3","steelblue3","orange","chocolate","aquamarine4","plum3","black","mediumpurple4","violetred3","brown4","chartreuse3","lavenderblush4","lightpink","mediumpurple3","lightcyan1","palegreen4","red1","wheat4","papayawhip","tomato3")

temp=ggplot(soil_meta_seq_env, aes(x=Study_day, y = Temperature, group = Donor, color= Donor)) + geom_line(size=2) + scale_color_manual(values = colpalette2) +theme_bw()
temp %+% subset(soil_meta_seq_env, Treatment %in% c("control"))

#convert soil metadata from wide to long so all env variables can be plotted at the same time
soil_meta_seq_env_long = melt(soil_meta_seq_env, id.vars = c("Sample", "Donor","Treatment","ADH_10_actual", "Study_day"))
soil_meta_seq_env_long_donor = soil_meta_seq_env_long %>% filter(Treatment == "donor") %>% filter(value != "NA")

ggplot(soil_meta_seq_env_long_donor, aes(x=Study_day, y= value, group = Donor, color = Donor)) + geom_line(size = 1) + scale_color_manual(values = colpalette2) + facet_wrap(~variable, scales = "free_y") + theme_bw() #env data by study day

ggplot(soil_meta_seq_env_long_donor, aes(x=ADH_10_actual, y= value, group = Donor, color = Donor)) + geom_line(size = 1) + scale_color_manual(values = colpalette2) + facet_wrap(~variable, scales = "free_y") + theme_bw() #env data by ADH

soil_meta_seq_env_long_donor_5000 = soil_meta_seq_env_long_donor %>% filter(ADH_10_actual <= 5000.0)
ggplot(soil_meta_seq_env_long_donor_5000, aes(x=ADH_10_actual, y= value, group = Donor, color = Donor)) + geom_line(size = 1) + scale_color_manual(values = colpalette2) + facet_wrap(~variable, scales = "free_y") + theme_bw() #env data by ADH only samples <= 5000 ADH

soil_meta_seq_env_long_5000 = soil_meta_seq_env_long %>% filter(ADH_10_actual <= 5000.0)
ggplot(soil_meta_seq_env_long_5000, aes(x=ADH_10_actual, y= value, group = Donor, color = Donor)) + geom_line(size = 1) + scale_color_manual(values = colpalette2) + facet_wrap(Treatment~variable, scales = "free_y") 

soil_meta_seq_env_long_control = soil_meta_seq_env_long %>% filter(Treatment == "control")


#next let's look at the RFM results:
PMI_results = read_excel("C:/Users/Allison/OneDrive - University of Tennessee/DeBruyn Lab/NIJ_TOX/PMI est project/PMI_5000_compiled.xlsx", sheet = "Model Results") #load our data sheet reporting OOBMSE, RMSE, and MAE from 100 model runs

PMI_results$measure = as.factor(PMI_results$measure) #convert data variable measure to a factor

PMI_results_sub = PMI_results %>% select(Mod_code, measure, Avg, SE) #select only the pertentant variables and the average error values

meta = read_excel("C:/Users/Allison/OneDrive - University of Tennessee/DeBruyn Lab/NIJ_TOX/PMI est project/PMI_5000_compiled.xlsx", sheet = "meta") #load metadata describing the model parameters 
meta$Marker = as.factor(meta$Marker) #convert data variable marker to a factor
meta$Tax_level = factor(meta$Tax_level, ordered = TRUE, levels = c("Phylum", "Class", "Order", "OTU")) #convert data variable Tax_level to an ordered factor 
meta$Env_data = as.factor(meta$Env_data) #conver data variable Env_data to a factor

PMI_results_sub = PMI_results_sub %>% left_join(meta, by = "Mod_code")
PMI_results_sub_MAE = PMI_results_sub %>% filter(measure == "MAE")

#plot average MAE ~ biological marker independent of other variables (taxonomic level, env data)
my_comparisons <- list( c("16S", "16S-ITS"), c("16S", "ITS"), c("16S-ITS", "ITS") )

ggplot(PMI_results_sub_MAE, aes(x=Marker, y=Avg, fill = "indianred")) + geom_boxplot() +theme_bw()+ stat_compare_means(method = "anova", label.x = 1, label.y = 980) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif")

mae_marker = lm(Avg ~ Marker, data = PMI_results_sub_MAE)
anova(mae_marker)
#Analysis of Variance Table

#Response: Avg
#Df Sum Sq Mean Sq F value   Pr(>F)   
#Marker     2  23805 11902.4  9.4565 0.001179 **
#  Residuals 21  26432  1258.7                    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#plot MAE of all 100 runs - not using this becuase they are not 100 independent runs
ggplot(MAE_long, aes(x=Marker, y=value, fill = "indianred")) + geom_boxplot() +theme_bw()+ stat_compare_means(method = "anova", label.x = 1, label.y = 980) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test")

#plot average MAE ~ tax level and marker independent of env 
my_comparisons <- list( c("Phylum", "Class"), c("Phylum", "Order"), c("Phylum", "OTU"), c("Class","Order"),c("Class","OTU"), c("Order","OTU") )

ggplot(PMI_results_sub_MAE, aes(x=Tax_level, y=Avg, fill = Tax_level)) + geom_boxplot() +theme_bw()+ facet_wrap(~Marker)+ stat_compare_means(method = "anova", label.x = 1, label.y = 980) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test")

#plot average MAE ~ tax level independent of other variables (biological marker, env data)
ggplot(PMI_results_sub_MAE, aes(x=Tax_level, y=Avg, fill = Tax_level)) + geom_boxplot() +theme_bw()+ stat_compare_means(method = "anova", label.x = 1, label.y = 980) 


mod_tax = lm(Avg ~ Tax_level, data = PMI_results_sub_MAE)
anova(mod_tax)
#Analysis of Variance Table

#Response: Avg
#Df Sum Sq Mean Sq F value Pr(>F)
#Tax_level  3   9419  3139.6  1.5383 0.2355
#Residuals 20  40818  2040.9 

mod_tax_marker = lm(Avg ~ Tax_level*Marker, data = PMI_results_sub_MAE)
anova(mod_tax_marker)
#Analysis of Variance Table

#Response: Avg
#Df  Sum Sq Mean Sq F value   Pr(>F)   
#Tax_level         3  9418.8  3139.6  3.0090 0.072251 . 
#Marker            2 23804.9 11902.4 11.4074 0.001677 **
#  Tax_level:Marker  6  4492.2   748.7  0.7176 0.643259   
#Residuals        12 12520.8  1043.4                    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

mod_tax_env = lm(Avg ~ Tax_level*Env_data, data = PMI_results_sub_MAE)
anova(mod_tax_env)


#assess the effects of env predictors
#plot average MAE ~ tax level independent of other variables (biological marker, env data)
ggplot(PMI_results_sub_MAE, aes(x= Env_data, y=Avg)) + geom_boxplot() +theme_bw()+ facet_wrap(~Marker)+ stat_compare_means(method = "t.test", label.x = 1, label.y = 980) + geom_point(color = Tax_level, size = 3)

#boxplot with data points colored by Taxa level
ggplot(PMI_results_sub_MAE, aes(x=Env_data, y=Avg)) + geom_boxplot(show.legend = F) + geom_point(position = position_jitterdodge(jitter.width = 2, dodge.width = 0), pch=21, aes(fill=Tax_level, size = 3)) +facet_wrap(~Marker) + theme_bw() + stat_compare_means(method = "t.test", label.x = 1, label.y = 980)


#range of avg MAE for ITS mods:
PMI_results_sub_MAE %>% filter(Marker == "ITS") %>% summarise(range(Avg))
#mean of avg MAE for ITS mods:
PMI_results_sub_MAE %>% filter(Marker == "ITS") %>% summarise(mean(Avg))

#range of avg MAE for 16S mods:
PMI_results_sub_MAE %>% filter(Marker == "16S") %>% summarise(range(Avg))
#mean of avg MAE for 16S mods:
PMI_results_sub_MAE %>% filter(Marker == "16S") %>% summarise(mean(Avg))

#range of avg MAE for 16S-ITS mods:
PMI_results_sub_MAE %>% filter(Marker == "16S-ITS") %>% summarise(range(Avg))
#mean of avg MAE for 16S-ITS mods:
PMI_results_sub_MAE %>% filter(Marker == "16S-ITS") %>% summarise(mean(Avg))

#look at model specs:
PMI_results_MAE = PMI_results %>% filter(measure == "MAE") 


min(PMI_results_MAE$Avg) #best model
max(PMI_results_MAE$Avg) #worst model
#error range: 796 - 997

PMI_results_MAE %>% arrange(Avg) %>% select(Mod_code, Avg)
#top model: 16S_phylum_env 796
#top 16S-ITS: 16S-ITS_order 801
#top ITS: ITS_order_env 874

#top model without env: 16S-ITS_order 801
#top model with env: 16S_phylum_env 796

#top 16S without env: 16S_order_env 816
#top 16S with env: 16S_phylum_env 796

PMI_results_MAE %>% arrange(desc(Avg)) %>% select(Mod_code, Avg)
#worst performing model: ITS_Phylum_no_10_OTU 997


#format dataframe for stats/boxplots
v = PMI_results %>% select(-name, -Avg, -SE)

v_long = melt(v, id.vars = c("Mod_code", "measure"))
v_long = v_long %>%left_join(meta, by = "Mod_code")

MAE_long = v_long %>% filter(measure == "MAE")

#bar charts to show MAE
a = ggplot(PMI_results_sub, aes(x=Tax_level, y=Avg, group = Env_data, fill=Env_data)) + geom_bar(stat = "identity", color="black", position=position_dodge()) + geom_errorbar(aes(ymin=Avg-SE, ymax =Avg+SE), width = .2, position = position_dodge(.9)) + scale_y_continuous(limits = c(0, 1050) ,expand = expansion(mult = c(0,0))) + facet_wrap(~Marker) +theme_bw()
a %+% subset(PMI_results_sub, measure %in% c("MAE")) +
  scale_fill_manual(values=c('#999999','#E69F00')) 

a2 = ggplot(PMI_results_sub, aes(x=Tax_level, y=Avg, group = Env_data, fill=Env_data)) + geom_bar(stat = "identity", color="black", position=position_dodge()) + geom_errorbar(aes(ymin=Avg-SE, ymax =Avg+SE), width = .2, position = position_dodge(.9)) + scale_y_continuous(expand = expansion(mult = c(0,0))) + coord_cartesian(ylim=c(750,1000)) + facet_wrap(~Marker) +theme_bw()
a2 %+% subset(PMI_results_sub, measure %in% c("MAE")) +
  scale_fill_manual(values=c('#999999','#E69F00')) 

b = ggplot(PMI_results_sub, aes(x=Env_data, y=Avg, fill=Env_data)) + geom_bar(stat = "identity", color="black", position=position_dodge()) + geom_errorbar(aes(ymin=Avg-SE, ymax =Avg+SE), width = .2, position = position_dodge(.9)) + scale_y_continuous(limits = c(0, 1050) ,expand = expansion(mult = c(0,0))) + facet_grid(Marker~Tax_level) +theme_bw()

b %+% subset(PMI_results_sub, measure %in% c("MAE"))+
  scale_fill_manual(values=c('#999999','#E69F00')) + 
  stat_compare_means(method = "t.test") #doesn't quite work since I plotted the average; should try with boxplots

f = ggplot(MAE_long, aes(x=Marker, y=value, fill=Marker)) + geom_bar(stat = "summary", fun.y = "mean", color="black") + coord_cartesian(ylim=c(750,1000))  +  facet_grid(Tax_level~Env_data) + theme_bw()
f %+%  stat_compare_means(comparisons = my_comparisons)



#boxplots:
c = ggplot(v_long, aes(x=Tax_level, y=value, fill=Env_data)) + geom_boxplot(outlier.colour = "black", position = position_dodge(1)) + facet_grid(~Marker) + theme_bw()
c %+% subset(v_long, measure %in% c("MAE"))+
  scale_fill_manual(values=c('#999999','#E69F00')) 

d = ggplot(v_long, aes(x=Env_data, y=value, fill=Env_data)) + geom_boxplot(outlier.colour = "black", position = position_dodge(1)) + facet_grid(Marker~Tax_level) + theme_bw()
d %+% subset(v_long, measure %in% c("MAE"))+
  scale_fill_manual(values=c('#999999','#E69F00')) +
  stat_compare_means(method = "t.test", label.x = 1, label.y = 980)


#stats:

mod = lm(value ~ Marker*Tax_level*Env_data, data = MAE_long)
anova(mod)
#Analysis of Variance Table

#Response: value
#Df  Sum Sq Mean Sq F value    Pr(>F)    
#Marker                       2 2380487 1190243 5649.12 < 2.2e-16 ***
#  Tax_level                    3  941879  313960 1490.11 < 2.2e-16 ***
#  Env_data                     1  207612  207612  985.37 < 2.2e-16 ***
#  Marker:Tax_level             6  449223   74871  355.35 < 2.2e-16 ***
#  Marker:Env_data              2  234385  117193  556.22 < 2.2e-16 ***
#  Tax_level:Env_data           3  652173  217391 1031.78 < 2.2e-16 ***
#  Marker:Tax_level:Env_data    6  157907   26318  124.91 < 2.2e-16 ***
#  Residuals                 2376  500613     211                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

plot_model(mod, type= "pred")
ggplot(MAE_long, aes(x=Env_data, y=value, fill=Env_data)) + geom_boxplot() + theme_bw()

ggplot(MAE_long, aes(x=Tax_level, y=value, group=Marker, fill=Marker)) + geom_boxplot(outlier.colour = "black", position = position_dodge(0.8)) + theme_bw()

my_comparisons <- list( c("16S", "16S-ITS"), c("16S", "ITS"), c("16S-ITS", "ITS") )

comp_2 <- list(c("16S", "ITS") )

y = compare_means(value~Marker, data = MAE_long, method = "anova", group.by = c("Tax_level","Env_data"))
e = ggplot(v_long, aes(x=Marker, y=value, fill=Marker)) + geom_boxplot(outlier.colour = "black", position = position_dodge(1)) + facet_grid(Tax_level~Env_data) + theme_bw()
e %+% subset(v_long, measure %in% c("MAE")) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 980) +
  stat_compare_means(comparisons = my_comparisons)

e %+% subset(v_long, measure %in% c("MAE")) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 980) +
  stat_compare_means(comparisons = comp_2)


