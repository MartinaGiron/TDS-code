library(tidyverse)

sales <- data.frame(item = c("mouse", "keyboard", "headphones", "usb hub", "charger",
                             "screen protector", "laptop sleeve", "monitor", "webcam",
                             "microphone", "sound card", "external fan"),
                    profit = c(20035, 23948, 945, 3396, 923,
                               249, 1020, 1011, 320, 
                               2830, 100, 81))
# Basic bar chart 
ggplot(sales, aes(x = item, y = profit)) +
  geom_col()

# Re-order categories
ggplot(sales, aes(x = fct_reorder(item, desc(profit)), y = profit)) +
  geom_col()

# Add Axis Labels
ggplot(sales, aes(x = fct_reorder(item, desc(profit)), y = profit)) +
  geom_col() +
  labs(title = "Most Profitable Items Sold",
       x = "Product",
       y = "Profit (USD)")
                  
#  Reposition Tick Labels
ggplot(sales, aes(x = fct_reorder(item, desc(profit)), y = profit)) +
  geom_col() +
  labs(title = "Most Profitable Items Sold",
       x = "Product",
       y = "Profit (USD)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7))

# Flip coordinate axes
ggplot(sales, aes(x = fct_reorder(item, profit), y = profit)) +
  geom_col() +
  labs(title = "Most Profitable Items Sold",
       x = "Product",
       y = "Profit (USD)") +
  coord_flip()

# Reclassify low-emphasis categories as others
sales %>%
  mutate(item_binned = ifelse(profit < 3000, "others", item)) %>%
  ggplot(., aes(x = fct_reorder(item_binned, profit), y = profit)) +
  geom_col() +
  labs(title = "Most Profitable Items",
       x = "Product",
       y = "Profit (USD)") +
  coord_flip()

# Customize color palette
sales %>%
  ggplot(., aes(x = fct_reorder(item, profit), y = profit, fill = profit)) +
  geom_col() +
  labs(title = "Most Profitable Items",
       x = "Product",
       y = "Profit (USD)") +
  coord_flip() +
  viridis::scale_fill_viridis(direction = -1) +
  theme_minimal()

# Customize color palette for your branding
sales %>%
  ggplot(., aes(x = fct_reorder(item, profit), y = profit, fill = profit)) +
  geom_col() +
  labs(title = "Most Profitable Items",
       x = "Product",
       y = "Profit (USD)") +
  coord_flip() +
  scale_fill_gradient(high = "#B00B69", low = "#0e0e63") + #add custom color hexes to low and high values
  theme_minimal()
