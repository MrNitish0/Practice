library(tidyverse)
library(stringr)
library(plotly)
# files used------
order_details <- read.csv("~/Pizza/order_details.csv")
View(order_details)

orders <- read_csv("Pizza/orders.csv")
View(orders)

pizza_types <- read_csv("Pizza/pizza_types.csv")
View(pizza_types)

pizzas <- read_csv("Pizza/pizzas.csv")
View(pizzas)
#----------------

# Joining Data as One.
Pizza_Data <- pizza_types %>% 
  full_join(pizzas, by = "pizza_type_id") %>% 
  full_join(order_details, by = "pizza_id") %>% 
  full_join(orders, by = "order_id")
view(Pizza_Data)


#1 Total Pizza Count by Size-------------------#

Count_by_Size <- Pizza_Data %>% 
  group_by(size) %>%
  count(quantity) %>%
  mutate(pizza_Qty=quantity*n) %>% 
  drop_na(pizza_Qty) %>% 
  summarize(Pizza_count=sum(pizza_Qty)) %>% 
  mutate(Count_Pcent=round(Pizza_count/sum(Pizza_count)*100,1))

view(Count_by_Size)


#---------------------------------------------1#

#2-Cost Contribution by SIze------------------------#

Cost_by_size <- Pizza_Data %>% 
  drop_na() %>% 
  group_by(size,price) %>% 
  summarise(QNTY=sum(quantity)) %>%
  mutate(T_Cost=QNTY*price) %>%
  summarise(Group_Cost=sum(T_Cost)) %>% 
  mutate(Pcnt_Cost=round({Group_Cost/sum(Group_Cost)}*100,2)) %>% 
  left_join(Count_by_Size, by="size")

view(Cost_by_size)


#Count vs Revenue plot--------------------------------------------

Cost_by_size_df <- as.data.frame(Cost_by_size)
  
Cost_by_size_df %>% 
  ggplot(aes(size,Pizza_count,colour = Pcnt_Cost))+
  geom_point(aes(size = Count_Pcent))+
  labs(title = "Pizza Count vs Pizza Revenue",
        x="Pizza Sizes",
        y="Units Sold",
       colour="Revenue",
       size=paste("Count","\n","Percentage")
       
       )


#-------------------------------------------------2#

view(Pizza_Data)



#Popular Pizza category Bar_plot by Name------------------------------#

Pizza_Data %>% 
  ggplot(aes(fct_infreq(name)))+
  geom_bar(colour = "black", fill ="pink" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Pizza Popularity",
       x="Pizzas",
       y= "Consumption")

#----------------------------------#


#Most sold Pizza Bar_plot with size by pizza_Id------------------------------#

Pizza_Data %>% 
  ggplot(aes(fct_infreq(pizza_id)))+
  geom_bar(colour = "black", fill ="pink" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Pizza Popularity",
       x="Pizza Id",
       y= "Consumption")

#----------------------------------#

view(Pizza_Data)



# changing the date column from character to Date format.

Pizza_Data$date = as.Date(Pizza_Data$date,format("%d-%m-%y"))
sapply(Pizza_Data, class)

##---------------------------------##
# storing months

monthX <-na.omit(unique(format(Pizza_Data$date, "%Y-%m")))
print(monthX)

M_store <-list()
for (Y_M in monthX) {
  dsa <-na.omit(Pizza_Data[format(Pizza_Data$date, "%Y-%m")==Y_M,])
  M_store[[Y_M]] <- dsa
  }

view(M_store[[Y_M]])

view(M_store[["2020-01"]])

## ------------------------------- ##
#?
M_store[["2020-10"]] %>% 
  group_by(name) %>%
  summarise(mtotal=sum(quantity)) %>%
  ggplot(aes(mtotal))+
  geom_density()

### --------------------------###
# Graph for indivisual pizza all year

pizza_names <- unique(Pizza_Data$name)
view(pizza_names)
 
naam_1 <- list()
view(Pizza_Data)

for (naam in pizza_names) {
  naa<- Pizza_Data[(Pizza_Data$name==naam),]
  naam_1[[naam]] <- naa
  }
view(naam_1[["The Hawaiian Pizza"]])
naam_1[["The Hawaiian Pizza"]]

# preparing Graph

piz <- function(pi){
  naam_1[[pi]] %>%
    drop_na(date) %>%
    arrange(date)%>%
    group_by(name,size,date= format( date, "%Y-%m")) %>%
    summarise(total=sum(quantity)) %>%
    ggplot(aes(date ,total,fill = size))+
    geom_bar(stat = "identity", position = "dodge")+
    facet_wrap(~size,3)+
    theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))+
  labs(title = naam_1[[pi]]$name,
       x = "Months",
       y ="Unit of Pizzas",
       fill = "Size")
}
piz(30)  #with piz() function we can enter 1-32 no. to check the pizza graph

## -------------------------------------- ##
# PIZZA TYPES
# ingredients
view(pizza_types) 

ingre <- unlist(strsplit(pizza_types$ingredients, split = ", ",useBytes = TRUE))
ingrd <- unique(ingre)
view(ingrd)

splt2 <- strsplit(pizza_types$ingredients, split = ", ", useBytes = TRUE)
splt2


### ---------------------


view(ingrd)
view(pizza_types)
sap <- list()

for (an_item in ingrd) {
  M_item <- grep(an_item,pizza_types$ingredients, perl = FALSE ,ignore.case = FALSE, value = FALSE, fixed = TRUE, useBytes = TRUE)
  M_item
# this is to handle the empty, so that the process would not get disrupt.
  if(length(M_item)>0){
    sap[[an_item]] <- pizza_types$name[M_item]
  }
  else{
  sap[[an_item]] <- NA}
}

sap

#turning into a tabular form
sap_df <- stack(sap)
colnames(sap_df) <- c("Product_Names","Ingredients_used")
sap_df
true_sap_df <- sap_df %>% 
  select(Ingredients_used,Product_Names) %>%
  arrange(Ingredients_used)

view(true_sap_df)
