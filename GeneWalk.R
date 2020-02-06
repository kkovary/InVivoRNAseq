library(tidyverse)
library(ggrepel)

hits <- read_csv('~/Downloads/Cort_pellet vs Placebo_pellet.csv') %>%
  mutate(GeneName = toupper(GeneName))

# High
gw_high <- read_csv('~/Documents/GitHub/ShinyApps/InVivoRNAseq/up_regulated_genewalk_results.csv') %>%
  rename(GeneName = hgnc_symbol)

joined_high <- left_join(hits, gw_high, by = 'GeneName') %>%
  filter(hit == T, foldChange > 1, !is.na(go_id))

joined_high <- joined_high %>% group_by(GeneName) %>% mutate(fraction_sim_go = ncon_go / sum(ncon_go))
  
ggplot(joined_high, aes(x = -log10(pvalue), y = fraction_sim_go, size = ncon_gene)) + geom_point(alpha = 0.5) +
  geom_text_repel(data = filter(joined_high, -log10(pvalue) > 0, fraction_sim_go > 0.35), 
                  aes(label = GeneName), colour = 'black', size = 3)

ggplot(joined_high, aes(x = log2(foldChange), y = fraction_sim_go, size = ncon_gene)) + geom_point(alpha = 0.5) +
  geom_text_repel(data = filter(joined_high, log2(foldChange) > 1, fraction_sim_go > 0.35), 
                  aes(label = GeneName), colour = 'black', size = 3)

# Low
gw_low <- read_csv('~/Documents/GitHub/ShinyApps/InVivoRNAseq/down_regulated_genewalk_results.csv') %>%
  rename(GeneName = hgnc_symbol)

joined_low <- left_join(hits, gw_low, by = 'GeneName') %>%
  filter(hit == T, foldChange < 1, !is.na(go_id))

joined_low <- joined_low %>% group_by(GeneName) %>% mutate(fraction_sim_go = ncon_go / sum(ncon_go))

ggplot(joined_low, aes(x = -log10(pvalue), y = fraction_sim_go, size = ncon_gene)) + geom_point(alpha = 0.5) +
  geom_text_repel(data = filter(joined_low, -log10(pvalue) > 0, fraction_sim_go > 0.35), 
                  aes(label = GeneName), colour = 'black', size = 3)

ggplot(joined_low, aes(x = log2(foldChange), y = fraction_sim_go, size = ncon_gene)) + geom_point(alpha = 0.5) +
  geom_text_repel(data = filter(joined_low, log2(foldChange) < -1, fraction_sim_go > 0.35), 
                  aes(label = GeneName), colour = 'black', size = 3)
