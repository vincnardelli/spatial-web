library(ggplot2)
library(patchwork)
library(dplyr)
library(purrr)
library(tidyr)

# Extract information from parallel object
import <- function(name){
  paralleltr <- transpose(readRDS(name))
  df <- data.frame(rho = sapply(paralleltr$model, function(s) s$lambda), 
                        genrho = unlist(paralleltr$genrho), 
                        # b0 = sapply(paralleltr$model, function(s) s$coefficients[1]), 
                        b1 = sapply(paralleltr$model, function(s) s$coefficients[1]),
                        #  b0se = sapply(paralleltr$model, function(s) s$rest.se[1]), 
                        b1se = sapply(paralleltr$model, function(s) s$rest.se[1]), 
                        #ols = unlist(paralleltr$ols),
                        zita = sapply(paralleltr$ps, function(s) s$zita),
                        n = sapply(transpose(paralleltr$ps)$nn, sum), 
                        N = sum(paralleltr$NN[[1]]),
                        loss = sapply(paralleltr$ps, function(s) s$loss),
                        repres = sapply(paralleltr$ps, function(s) s$representativeness),
                        AIC = sapply(paralleltr$model, function(s) AIC(s)), 
                        LL = sapply(paralleltr$model, function(s) s$LL))
  
  df <- df %>% 
    select(-rho) %>% 
    select('rho'=genrho,  everything()) %>% 
    group_by(rho, zita) %>% 
    summarise(bias = round((mean(b1)-true)^2, 1), 
              var=round(mean(b1se)^2, 3), 
              mse = bias+var)
  names(df) <- c("rho", "zita", "Bias", "Variance", "MSE")
  return(df)
  
}

simulation3d <- import('1_d_error.rds')
simulation3k <- import('1_k_error.rds')
simulation3i <- import('1_i_error.rds')

graph3d <-simulation3d %>% 
  filter(rho == 0.2) %>% 
  tidyr::pivot_longer(Bias:MSE) %>% 
  mutate(name = factor(name, c("MSE", "Bias", "Variance"))) %>% 
  ggplot() +
  geom_line(aes(zita, value, color=name, linetype=name)) +
  scale_colour_manual(name="", values = c("black", "gray20", "gray70")) +
  scale_linetype_manual(name="", values = c("solid", "dashed", "dashed")) + 
  theme_bw()+
  coord_cartesian(ylim=c(0, 0.4)) +
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  labs(x = expression(zeta), y="MSE / Bias / Variance") +
  scale_x_continuous(breaks=seq(0, 1, 0.2))  +
  ggtitle("Sim - TD") 

graph3k <-simulation3k %>% 
  filter(rho == 0.2) %>% 
  tidyr::pivot_longer(Bias:MSE) %>% 
  mutate(name = factor(name, c("MSE", "Bias", "Variance"))) %>% 
  ggplot() +
  geom_line(aes(zita, value, color=name, linetype=name)) +
  scale_colour_manual(name="", values = c("black", "gray20", "gray70")) +
  scale_linetype_manual(name="", values = c("solid", "dashed", "dashed")) + 
  theme_bw()+
  coord_cartesian(ylim=c(0, 0.4)) +
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  labs(x = expression(zeta), y="") +
  scale_x_continuous(breaks=seq(0, 1, 0.2))  +
  ggtitle("Sim - KNN") 

graph3i <-simulation3i %>% 
  filter(rho == 0.2) %>% 
  tidyr::pivot_longer(Bias:MSE) %>% 
  mutate(name = factor(name, c("MSE", "Bias", "Variance"))) %>% 
  ggplot() +
  geom_line(aes(zita, value, color=name, linetype=name)) +
  scale_colour_manual(name="", values = c("black", "gray20", "gray70")) +
  scale_linetype_manual(name="", values = c("solid", "dashed", "dashed")) + 
  theme_bw()+
  coord_cartesian(ylim=c(0, 0.4)) +
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
 labs(x = expression(zeta), y="") +
  scale_x_continuous(breaks=seq(0, 1, 0.2))  +
  ggtitle("Sim - ID") 


(graph3d | graph3k | graph3i) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("output/comparison_error.pdf", width = 8, height=4)

ggsave("output/comparison_error.png", width = 8, height=4)






