library(data.table)
library(ggplot2)
library(arules)
library(ggmosaic)


filePath<-("D:/DataProject/venv/Quantum/")
data<- fread(paste0(filePath,"importQVI_data.csv"))

sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]


premium_colors <- c(
  "Premium" = "#44D5A3",
  "Mainstream" = "#44B6C5",
  "Budget" = "#3F69AE"
)

p <- ggplot(sales) +
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 4),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 4),
        plot.title = element_text(size = 12, margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"))  
p <- p + scale_fill_manual(values = premium_colors)

p <- p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = paste0(round(.wt / sum(.wt) * 100, 1), '%')))

ggsave("Sales.png", plot = p, width = 8, height = 6, units = "in")
#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE,PREMIUM_CUSTOMER)][order(CUSTOMERS)]
#### Create plot
p <- ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER,LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 4),
        axis.text.y = element_text(size = 6), 
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 4),
        plot.title = element_text(size = 12, margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"))
#### Plot with proportion of customers
p <- p + scale_fill_manual(values = premium_colors)
p <- p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y =(ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))
ggsave("customer.png", plot = p, width = 8, height = 6, units = "in")

# Calculate the average number of units per customer by segments
avg_units_customer_by_segments <- data[, .(avgunits_customer = sum(PROD_QTY)/length(unique(LYLTY_CARD_NBR))), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
# Visualization
p <-ggplot(avg_units_customer_by_segments, aes(x =LIFESTAGE , y = avgunits_customer, fill = PREMIUM_CUSTOMER)) +
  
  geom_bar(stat = "identity", position = position_dodge2(width = c(0.2, 0.6)), color = "black", linewidth = 0.2) +
  labs(title = "Average Units per Customer by Premium and Lifestage",
       x = "segments", y = "Average Units per Customer") +
  scale_fill_manual(values = premium_colors) +
  theme(panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 4),
        plot.title = element_text(size = 12, margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"))
p <- p + geom_text(aes(label = round(avgunits_customer, 1), y = avgunits_customer + 0.1), position = position_dodge(width = 0.9), vjust = 0, size = 3)
# Create custom legend
ggsave("AvgUnit.png", plot = p, width = 10, height = 6, units = "in")
# Calculate the average price per unit chips by segments
avgprice_units_by_segment <- data[, .(avgprice_units = sum(TOT_SALES) / sum(PROD_QTY)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Visualization
p <-ggplot(avgprice_units_by_segment, aes(x =LIFESTAGE , y = avgprice_units, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge2(width = c(0.2, 0.6)), color = "black", linewidth = 0.2) +  
  labs(title = "Average price per unit chips by Premium and Lifestage",
       x = "segments", y = "Average price per unit") +
  scale_fill_manual(values = premium_colors) +
  theme(panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 4),
        plot.title = element_text(size = 12, margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"))
p <- p + geom_text(aes(label = round(avgprice_units, 1), y = avgprice_units + 0.1), position = position_dodge(width = 0.9), vjust = 0, size = 3)
ggsave("AvgPriceUnit.png", plot = p, width = 10, height = 6, units = "in")

#### Perform an independent t‐test between mainstream vs premium and budget midage and young singles and couples
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") &
         PREMIUM_CUSTOMER == "Mainstream", price],
       data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") &
         PREMIUM_CUSTOMER != "Mainstream", price],
       alternative = "greater")

#### Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER =="Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER =="Mainstream"),]
#### Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment =sum(PROD_QTY)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by= BRAND]
brand_proportions <- merge(quantity_segment1_by_brand,quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]

p <-ggplot(brand_proportions, aes(x = reorder(BRAND, -affinityToBrand), y = affinityToBrand)) +
  geom_bar(stat = "identity", fill = "#6183BA") +
  geom_text(aes(label = round(affinityToBrand, 2), vjust = 0, hjust = -0.5), size = 3) +
  coord_flip() +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent")) + 
  labs(title = "Brand Affinity Bar Chart", x = "Brand", y = "Affinity Score")


ggsave("afintyAnalysis.png", plot = p, width = 12, height = 5, units = "in")
#### Preferred pack size compared to the rest of the population
quantity_segment1_by_pack <- segment1[, .(targetSegment =sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by =PACK_SIZE]
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[,affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]
print(length(unique(data$STORE_NBR)))
p <-ggplot(pack_proportions, aes(x = reorder(PACK_SIZE, -affinityToPack), y = affinityToPack)) +
  geom_bar(stat = "identity", fill = "#6183BA") +
  geom_text(aes(label = round(affinityToPack, 2), vjust = 0, hjust = -0.5), size = 3) +
  coord_flip() +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent")) +  
  labs(title = "PACK SIZE Affinity Bar Chart", x = "PACK_SIZE", y = "Affinity Score")


ggsave("afintyAnalysisPacksize.png", plot = p, width = 12, height = 5, units = "in")