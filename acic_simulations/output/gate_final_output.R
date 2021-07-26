# Plots the rmse, coverage, bias and interval length graphs of the GATE results
rm(list=ls())
library("gridExtra")
library("grid")
library("ggplot2")
library("xtable")

load("~/Documents/GitHub/CML_ATE_HTE/acic_simulations/output/gate_final_ouput.RData")
knobs <- read.csv("~/Documents/GitHub/CML_ATE_HTE/acic_simulations/knobs_all.csv", sep=";")
b_idx <- c(28,56,41,22,32,30,27) #Benchmark analysis
names_low <-  c("GML Lasso low" ,"GML Trees low " ,"GML Boost low","GML RF low","GML Nnet low","GML BART low", "Causal Forest low", "BART MChains low")
names_high <- c("GML Lasso high" ,"GML Trees high " ,"GML Boost high","GML RF high","GML Nnet high", "GML BART high", "Causal Forest high", "BART MChains high")

# Tables ------------------------------------------------------------------
low_matrix_rmse_m <- low_matrix_rmse
colnames(low_matrix_rmse_m) <- names_low
high_matrix_rmse_m <- high_matrix_rmse
colnames(high_matrix_rmse_m) <- c("GML Lasso high" ,"GML Trees high " ,"GML Boost high","GML RF high","GML Nnet high", "GML BART high", "Causal Forest high", "BART MChains high")
rmse_all <- cbind(low_matrix_rmse_m, high_matrix_rmse_m)
round(rmse_all[b_idx , sort(colnames(rmse_all))],2)
xtable(rmse_all[b_idx , sort(colnames(rmse_all))])


low_matrix_coverage_m <- low_matrix_coverage
colnames(low_matrix_coverage_m) <- names_low
high_matrix_coverage_m <- high_matrix_coverage
colnames(high_matrix_coverage_m) <- names_high
coverage_all <- cbind(low_matrix_coverage_m, high_matrix_coverage_m)
coverage_all[b_idx ,sort(colnames(coverage_all))]
xtable(coverage_all[b_idx ,sort(colnames(coverage_all))])


low_matrix_bias_m <- low_matrix_bias 
colnames(low_matrix_bias_m) <- names_low
high_matrix_bias_m <- high_matrix_bias 
colnames(high_matrix_bias_m) <- names_high
bias_all <- cbind(low_matrix_bias_m, high_matrix_bias_m)
xtable(bias_all[b_idx ,sort(colnames(bias_all))])

low_matrix_interval_m <- low_matrix_interval
colnames(low_matrix_interval_m) <- names_low
high_matrix_interval_m <- high_matrix_interval
colnames(high_matrix_interval_m) <- names_high
intervall_all <- cbind(low_matrix_interval_m, high_matrix_interval_m)
round(intervall_all[b_idx ,sort(colnames(intervall_all))],2)
xtable(intervall_all[b_idx ,sort(colnames(intervall_all))])


xtable(rbind(colMeans(bias_all[,sort(colnames(bias_all))], na.rm=T),colMeans(rmse_all[,sort(colnames(rmse_all))]),colMeans(coverage_all[,sort(colnames(coverage_all))]),colMeans(intervall_all[,sort(colnames(intervall_all))])))


# Plotting ----------------------------------------------------------------

plot <-function(data_low, data_high, metrics="metrics" , methods, title, y_name, min=0, lim=0.3, r=100, pos=0.3, text_pos="GML RF", legend_pos=c(0.15,0.8), shape=7, color_h="blue", color_l="green", size_shape=4, text_size=17, line=0){
  
  means_low <- data.frame(round(colMeans(data_low,na.rm = T),2))
  colnames(means_low) <- metrics
  means_low$methods <- as.factor(methods)
  means_low$methods <- factor(means_low$methods, levels = means_low$methods)
  means_low$methods <- methods
  means_low$level <- rep("least", nrow(means_low))
  
  means_high <- data.frame(round(colMeans(data_high,na.rm = T),3))
  colnames(means_high) <- metrics
  means_high$methods <- as.factor(methods)
  means_high$methods <- factor(means_high$methods, levels = means_high$methods)
  means_high$methods <- methods
  means_high$level  <- rep("most", nrow(means_high))
  
  means <- rbind(means_high, means_low)
  
  
  ggplot(means, aes(x=methods, y=metrics, color=level)) + geom_point(aes(shape=level), size=size_shape, shape=shape) + ggtitle(title) + theme_bw() + ylab(y_name) +ylim(min,lim)+ scale_colour_manual(values = c(color_h, color_l))+ scale_fill_manual(values = c("green", "blue")) +theme(axis.title.x=element_blank())+ theme(axis.text.y = element_text(size=text_size))+ annotate("text", label = paste("k = ", nrow(data_low),", r = ",r, sep=""), x = text_pos, y = pos, size=5)+theme(axis.text.x = element_text(angle = 90, size=text_size))+theme(plot.title = element_text(size = text_size, face = "bold")) + theme(axis.title = element_text(size = text_size, face = "bold")) + guides(size = FALSE)+ theme(legend.position = legend_pos,legend.box = "horizontal") + theme(legend.background=element_rect(fill = alpha("white", 0)))  + theme(legend.text=element_text(size=text_size))+theme(legend.title =element_blank())+geom_hline(yintercept=line)
  
}

#overview
rmse_overview <- plot(low_matrix_rmse, high_matrix_rmse, methods = colnames(high_matrix_rmse), title = "RMSE GATE Overview", y_name="rmse", min=0, lim=0.3, pos=0.3, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
coverage_overview <- plot(low_matrix_coverage, high_matrix_coverage, methods = colnames(high_matrix_coverage), title = "Coverage GATE Overview", y_name="coverage", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
bias_overview <- plot(low_matrix_bias, high_matrix_bias,methods=colnames(low_matrix_bias),title="Bias GATE Overview", y_name="bias", min=-0.25, lim=0.25, pos=0.25, shape=15, color_l = "yellow3", color_h = "orange3")
interval_overview <- plot(low_matrix_interval, high_matrix_interval,methods=colnames(low_matrix_interval),title="Interval GATE Overview", y_name="int length", min=0.09, lim=0.3, pos=0.255, shape=19, color_l = "steelblue4", color_h = "steelblue1", line=NULL)


#rmse
plot1_r <- plot(low_matrix_rmse[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear"),], high_matrix_rmse[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear") ,], y_name="rmse GATE", methods=colnames(low_matrix_rmse),title= "Both functions linear", shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot1b_r <- plot(low_matrix_rmse[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear") ,], high_matrix_rmse[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear"),], y_name="rmse GATE", methods=colnames(low_matrix_rmse),title= "At least one function linear", shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot2_r <- plot(low_matrix_rmse[which(knobs$Response.Model!="linear" & knobs$Treatment.Model!="linear"),], high_matrix_rmse[which(knobs$Response.Model!="linear" & knobs$Treatment.Model!="linear") ,], y_name="rmse GATE", methods=colnames(low_matrix_rmse),title= "No function linear", shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot3_r <- plot(low_matrix_rmse[which(knobs$Percent.treated=="low"),], high_matrix_rmse[which(knobs$Percent.treated=="low"),],methods=colnames(low_matrix_rmse),title="Percentage of treated low", y_name="rmse GATE", min=0, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot4_r <- plot(low_matrix_rmse[which(knobs$Percent.treated=="high"),], high_matrix_rmse[which(knobs$Percent.treated=="high"),],methods=colnames(low_matrix_rmse),title="Percentage of treated high", y_name="rmse GATE", min=0, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot5_r <- plot(low_matrix_rmse[which(knobs$Overlap=="full"),], high_matrix_rmse[which(knobs$Overlap=="full"),],methods=colnames(low_matrix_rmse),title="Full Overlap", y_name="rmse GATE", min=0, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot6_r <- plot(low_matrix_rmse[which(knobs$Overlap=="penalize"),], high_matrix_rmse[which(knobs$Overlap=="penalize"),],methods=colnames(low_matrix_rmse),title="Penalized Overlap", y_name="rmse GATE", min=0, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot7_r <- plot(low_matrix_rmse[which(knobs$Trt.Rsp.Alignment=="low"),], high_matrix_rmse[which(knobs$Trt.Rsp.Alignment=="low"),],methods=colnames(low_matrix_rmse),title="Low Alignment", y_name="rmse GATE", shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot8_r <- plot(low_matrix_rmse[which(knobs$Trt.Rsp.Alignment=="high"),], high_matrix_rmse[which(knobs$Trt.Rsp.Alignment=="high"),],methods=colnames(low_matrix_rmse),title="High Alignment", y_name="rmse GATE", shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot9_r <- plot(low_matrix_rmse[which(knobs$heterogneity=="low"),], high_matrix_rmse[which(knobs$heterogneity=="low"),],methods=colnames(low_matrix_rmse),title="Low Heterogeneity", y_name="rmse GATE", lim=0.31, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot10_r <- plot(low_matrix_rmse[which(knobs$heterogneity=="high"),], high_matrix_rmse[which(knobs$heterogneity=="high"),],methods=colnames(low_matrix_rmse),title="High Heterogeneity", y_name="rmse GATE", lim=0.31, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot11_r <- plot(low_matrix_rmse[which(knobs$Treatment.Model=="polynomial"),], high_matrix_rmse[which(knobs$Treatment.Model=="polynomial"),],methods=colnames(low_matrix_rmse),title="Treatment model polynomial", y_name="rmse GATE", lim=0.31, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot12_r <- plot(low_matrix_rmse[which(knobs$Treatment.Model=="step"),], high_matrix_rmse[which(knobs$Treatment.Model=="step"),],methods=colnames(low_matrix_rmse),title="Treatment model step", y_name="rmse GATE", lim=0.31, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot13_r <- plot(low_matrix_rmse[which(knobs$Response.Model=="step"),], high_matrix_rmse[which(knobs$Treatment.Model=="step"),],methods=colnames(low_matrix_rmse),title="Response model step", y_name="rmse GATE", lim=0.31, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)
plot14_r <- plot(low_matrix_rmse[which(knobs$Response.Model=="exponential"),], high_matrix_rmse[which(knobs$Response.Model=="exponential"),],methods=colnames(low_matrix_rmse),title="Response model exponential", y_name="rmse GATE", lim=0.31, shape=18, color_l = "turquoise2", color_h = "darkblue", size_shape = 4)


#coverage
plot1_c <- plot(low_matrix_coverage[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear"),], high_matrix_coverage[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear"),],methods=colnames(low_matrix_coverage), title= "Both functions linear", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot1b_c <- plot(low_matrix_coverage[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear"),], high_matrix_coverage[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear") ,],methods=colnames(low_matrix_coverage), title= "At least one function linear", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot2_c <- plot(low_matrix_coverage[which(knobs$Response.Model!="linear" & knobs$Treatment.Model!="linear"),], high_matrix_coverage[which(knobs$Response.Model!="linear" & knobs$Treatment.Model!="linear") ,], methods=colnames(low_matrix_coverage), title= "No function linear", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot3_c <- plot(low_matrix_coverage[which(knobs$Percent.treated=="low"),], high_matrix_coverage[which(knobs$Percent.treated=="low"),],methods=colnames(low_matrix_coverage),title="Percentage of treated low", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot4_c <- plot(low_matrix_coverage[which(knobs$Percent.treated=="high"),], high_matrix_coverage[which(knobs$Percent.treated=="high"),],methods=colnames(low_matrix_coverage),title="Percentage of treated high", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot5_c <- plot(low_matrix_coverage[which(knobs$Overlap=="full"),], high_matrix_coverage[which(knobs$Overlap=="full"),],methods=colnames(low_matrix_coverage),title="Full Overlap", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot6_c <- plot(low_matrix_coverage[which(knobs$Overlap=="penalize"),], high_matrix_coverage[which(knobs$Overlap=="penalize"),],methods=colnames(low_matrix_coverage),title="Penalized Overlap", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot7_c <- plot(low_matrix_coverage[which(knobs$Trt.Rsp.Alignment=="low"),], high_matrix_coverage[which(knobs$Trt.Rsp.Alignment=="low"),],methods=colnames(low_matrix_coverage),title="Low Alignment", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot8_c <- plot(low_matrix_coverage[which(knobs$Trt.Rsp.Alignment=="high"),], high_matrix_coverage[which(knobs$Trt.Rsp.Alignment=="high"),],methods=colnames(low_matrix_coverage),title="High Alignment", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot9_c <- plot(low_matrix_coverage[which(knobs$heterogneity=="low"),], high_matrix_coverage[which(knobs$heterogneity=="low"),],methods=colnames(low_matrix_coverage),title="Low Heterogeneity", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot10_c <- plot(low_matrix_coverage[which(knobs$heterogneity=="high"),], high_matrix_coverage[which(knobs$heterogneity=="high"),],methods=colnames(low_matrix_coverage),title="High Heterogeneity", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot11_c <- plot(low_matrix_coverage[which(knobs$Treatment.Model=="polynomial"),], high_matrix_coverage[which(knobs$Treatment.Model=="polynomial"),],methods=colnames(low_matrix_coverage),title="Treatment model polynomial", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot12_c <- plot(low_matrix_coverage[which(knobs$Treatment.Model=="step"),], high_matrix_coverage[which(knobs$Treatment.Model=="step"),],methods=colnames(low_matrix_coverage),title="Treatment model step", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot13_c <- plot(low_matrix_coverage[which(knobs$Response.Model=="step"),], high_matrix_coverage[which(knobs$Response.Model=="step"),],methods=colnames(low_matrix_coverage),title="Response model step", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)
plot14_c <- plot(low_matrix_coverage[which(knobs$Response.Model=="exponential"),], high_matrix_coverage[which(knobs$Response.Model=="exponential"),],methods=colnames(low_matrix_coverage),title="Response model exponential", y_name="coverage GATE", min=0.1, lim=1, pos=1, shape=17, color_l = "indianred2", color_h = "darkred", line=0.95)

#bias 
plot1_b <- plot(low_matrix_bias[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear"),], high_matrix_bias[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear"),],methods=colnames(low_matrix_bias), title= "Both functions linear", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot1b_b <- plot(low_matrix_bias[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear") ,], high_matrix_bias[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear") ,],methods=colnames(low_matrix_bias), title= "At least one function linear", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot2_b <- plot(low_matrix_bias[which(knobs$Response.Model=="linear" & knobs$Treatment.Mode!="linear") ,], high_matrix_bias[which(knobs$Response.Model!="linear" & knobs$Treatment.Model=="linear") ,], methods=colnames(low_matrix_bias), title= "No function linear", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot3_b <- plot(low_matrix_bias[which(knobs$Percent.treated=="low"),], high_matrix_bias[which(knobs$Percent.treated=="low"),],methods=colnames(low_matrix_bias),title="Percentage of treated low", y_name="bias GATE", min=-0.3,lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot4_b <- plot(low_matrix_bias[which(knobs$Percent.treated=="high"),], high_matrix_bias[which(knobs$Percent.treated=="high"),],methods=colnames(low_matrix_bias),title="Percentage of treated high", y_name="bias GATE", min=-0.3,lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot5_b <- plot(low_matrix_bias[which(knobs$Overlap=="full"),], high_matrix_bias[which(knobs$Overlap=="full"),],methods=colnames(low_matrix_bias),title="Full Overlap", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot6_b <- plot(low_matrix_bias[which(knobs$Overlap=="penalize"),], high_matrix_bias[which(knobs$Overlap=="penalize"),],methods=colnames(low_matrix_bias),title="Penalized Overlap", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot7_b <- plot(low_matrix_bias[which(knobs$Trt.Rsp.Alignment=="low"),], high_matrix_bias[which(knobs$Trt.Rsp.Alignment=="low"),],methods=colnames(low_matrix_bias),title="Low Alignment", y_name="bias GATE", min=-0.3,lim=0.25, pos=0.25, shape=15, color_l = "yellow3", color_h = "orange3")
plot8_b <- plot(low_matrix_bias[which(knobs$Trt.Rsp.Alignment=="high"),], high_matrix_bias[which(knobs$Trt.Rsp.Alignment=="high"),],methods=colnames(low_matrix_bias),title="High Alignment", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot9_b <- plot(low_matrix_bias[which(knobs$heterogneity=="low"),], high_matrix_bias[which(knobs$heterogneity=="low"),],methods=colnames(low_matrix_bias),title="Low Heterogeneity", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot10_b <- plot(low_matrix_bias[which(knobs$heterogneity=="high"),], high_matrix_bias[which(knobs$heterogneity=="high"),],methods=colnames(low_matrix_bias),title="High Heterogeneity", y_name="bias GATE", min=-0.3,lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot11_b <- plot(low_matrix_bias[which(knobs$Treatment.Model=="polynomial"),], high_matrix_bias[which(knobs$Treatment.Model=="polynomial"),],methods=colnames(low_matrix_bias),title="Treatment model polynomial", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot12_b <- plot(low_matrix_bias[which(knobs$Treatment.Model=="step"),], high_matrix_bias[which(knobs$Treatment.Model=="step"),],methods=colnames(low_matrix_bias),title="Treatment model step", y_name="bias GATE", min=-0.3,lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot13_b <- plot(low_matrix_bias[which(knobs$Response.Model=="step"),], high_matrix_bias[which(knobs$Response.Model=="step"),],methods=colnames(low_matrix_bias),title="Response model step", y_name="bias GATE", min=-0.3, lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")
plot14_b <- plot(low_matrix_bias[which(knobs$Response.Model=="exponential"),], high_matrix_bias[which(knobs$Response.Model=="exponential"),],methods=colnames(low_matrix_bias),title="Response model exponential", y_name="bias GATE", min=-0.3,lim=0.25, pos=0.25,  shape=15, color_l = "yellow3", color_h = "orange3")

#interval 
plot1_i <- plot(low_matrix_interval[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear") ,], high_matrix_interval[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear") ,],methods=colnames(low_matrix_interval), title= "Both functions linear", y_name="int length GATE", min=0.03, lim=0.35, pos=0.3, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot1b_i <- plot(low_matrix_interval[which(knobs$Response.Model=="linear" | knobs$Treatment.Model=="linear") ,], high_matrix_interval[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear"),],methods=colnames(low_matrix_interval), title= "At least one function linear", y_name="int length GATE", min=0.03, lim=0.35, pos=0.3, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot2_i <- plot(low_matrix_interval[which(knobs$Response.Model!="linear" & knobs$Treatment.Model!="linear") ,], high_matrix_interval[which(knobs$Response.Model=="linear" & knobs$Treatment.Model=="linear") ,], methods=colnames(low_matrix_interval), title= "No function linear", y_name="int length GATE", min=0.03, lim=0.35, pos=0.3, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot3_i <- plot(low_matrix_interval[which(knobs$Percent.treated=="low"),], high_matrix_interval[which(knobs$Percent.treated=="low"),],methods=colnames(low_matrix_interval),title="Percentage of treated low", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot4_i <- plot(low_matrix_interval[which(knobs$Percent.treated=="high"),], high_matrix_interval[which(knobs$Percent.treated=="high"),],methods=colnames(low_matrix_interval),title="Percentage of treated high", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot5_i <- plot(low_matrix_interval[which(knobs$Overlap=="full"),], high_matrix_interval[which(knobs$Overlap=="full"),],methods=colnames(low_matrix_interval),title="Full Overlap", y_name="int length GATE", min=0.08, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot6_i <- plot(low_matrix_interval[which(knobs$Overlap=="penalize"),], high_matrix_interval[which(knobs$Overlap=="penalize"),],methods=colnames(low_matrix_interval),title="Penalized Overlap", y_name="int length GATE", min=0.08, lim=0.25, pos=0.18, shape=19, color_l = "steelblue4", color_h = "steelblue1", line=NULL)
plot7_i <- plot(low_matrix_interval[which(knobs$Trt.Rsp.Alignment=="low"),], high_matrix_interval[which(knobs$Trt.Rsp.Alignment=="low"),],methods=colnames(low_matrix_interval),title="Low Alignment", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot8_i <- plot(low_matrix_interval[which(knobs$Trt.Rsp.Alignment=="high"),], high_matrix_interval[which(knobs$Trt.Rsp.Alignment=="high"),],methods=colnames(low_matrix_interval),title="High Alignment", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot9_i <- plot(low_matrix_interval[which(knobs$heterogneity=="low"),], high_matrix_interval[which(knobs$heterogneity=="low"),],methods=colnames(low_matrix_interval),title="Low Heterogeneity", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot10_i <- plot(low_matrix_interval[which(knobs$heterogneity=="high"),], high_matrix_interval[which(knobs$heterogneity=="high"),],methods=colnames(low_matrix_interval),title="High Heterogeneity", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot11_i <- plot(low_matrix_interval[which(knobs$Treatment.Model=="polynomial"),], high_matrix_interval[which(knobs$Treatment.Model=="polynomial"),],methods=colnames(low_matrix_interval),title="Treatment model polynomial", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot12_i <- plot(low_matrix_interval[which(knobs$Treatment.Model=="step"),], high_matrix_interval[which(knobs$Treatment.Model=="step"),],methods=colnames(low_matrix_interval),title="Treatment model step", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot13_i <- plot(low_matrix_interval[which(knobs$Response.Model=="step"),], high_matrix_interval[which(knobs$Response.Model=="step"),],methods=colnames(low_matrix_interval),title="Response model step", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
plot14_i <- plot(low_matrix_interval[which(knobs$Response.Model=="exponential"),], high_matrix_interval[which(knobs$Response.Model=="exponential"),],methods=colnames(low_matrix_interval),title="Response model exponential", y_name="int length GATE", min=0.09, lim=0.25, pos=0.25, shape=19, color_l = "steelblue4", color_h = "steelblue1",line=NULL)
