# Task 1: Understanding the Data in Hand

# Superstoresdb have multiple files containing the below informations:

# market_fact ia a fact table containing all the fact variables which is connected to all the other dimension tables (cust_dimen, prod_id, shipping_id, orders_id) through foreign keys. 
# The market_fact provides all the information about the profit, sales, product base margin, discount, order quantity, shipping cost based on various customers, products, orders, shipping modes.

# a. cust_dimen - Information about the customers as mentioned below:-
 
# Customer_Name - Represents the customer name,  data type - varchar(22)
# Province - Unit of a country where the customer is located, data type - varchar(21)
# Region - An area inside the unit of a country where customer is located, data type - varchar(21)
# Customer_Segment - Dividing customers into groups based on common characteristics, data type - varchar(14)
# Cust_id - Unique ID for each customer, data type - varchar(9)

# b. order_dimen - Details of orders as mentioned below:-

# Order_ID - Represents the order id for shipments,  data type - varchar(5)
# Order_date - Date on which the order was placed, data type - char(10), it cannot have a datatype of date as the format is not according to the date.
# Order_Priority - Places a specific shipment ahead of other like "Critical", "High", "Medium", "Low", "Not Specified", data type - varchar(13)
# Ord_id - Unique ID for the order of products, data type - varchar(8)

# c. Prod_dimen - Details about all the products available in the store:-

# Product_Category - A way to organize products in store by the type of prodcuts store sells,  data type - varchar(15)
# Product_Sub_Category - A sub class of product category, data type - varchar(30), Unique
# Prod_id - Unique ID for the product sub category, data type - varchar(7)

# d. shipping_dimen - Details about the shippings:-

# Order_ID - Represents the order id for shipments,  data type - varchar(5)
# Ship_Mode - Term used to distinguish different ways to perform like "Express Air", "Regualr Air", "Delivery Truck", data type - varchar(14)
# Ship_Date - Date on which the order was shipped, data type - char(10)
# Ship_id- Unique ID for each shippment, data type - varchar(8)

# e. market_fact - Details about market facts for all the prodcuts, customers, order and shipments:-

# Ord_id - ID for the order of products,  data type - varchar(8)
# Prod_id - ID for the product sub category, data type - varchar(7)
# Ship_id - ID for each shippment, data type - varchar(8)
# cust_id- ID for each customer, data type - varchar(9)
# Sales - sales value for considering all the prodcuts, customers, order and shipments, data type - float
# Discout - Discounts received on sales, data type - float
# Order_Quantity - Number of quantity sold by the store, purchased by the customer, shipped by store, by product sub category, data type - Int(2)
# Profit - Profit received or loss incurred, data type - Float
# Shipping_Cost - Cost of shipping the products - datatype - Float
# Product_Base_Margin - States about how much the prodct sells for the actual cost of the product itself, data type - text


# 2. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.)

# Primary key is a field in a table which uniquely idedtifies each record in a table. It must contain unique values and cannot have NULL values.
# The below mention primary key in each table are unique and do not have any NULL values. Hence are considered as unique values.

# Foreign Key is a column/s that references a column of other table. The purposed of the foreign key is to ensure referential integrity of the data.
# The below mentioned foreign key are referencing the column of other table. Hence are considered as foreign keys.  

# cust_dimen - Primary Key (Cust_id), No Foreign Keys 
# marked_fact - No Primary Key, Foreign Keys (Ord_id referencing Ord_id of order_dimension table, Prod_id referencing Prod_id of prod_dimen table, Ship_id referencing Ship_id of ship_dimen table, Cust_id referencing Cust_id of cust_dimen table)
# order_dimen - Primary Key (Ord_id), No Foreign Keys
# prod_dimen - Primary Key (Prod_id), No Foreign Keys
# shipping_dimen - Primary Key (Ship_id), No Foreign Keys

# Task 2: Basic Analysis
 
# A. Find the total and the average sales (display total_sales and avg_sales)

select sum(Sales) as total_sales, avg(Sales) as avg_sales
from  market_fact; 

# B. Display the number of customers in each region in decreasing order of no_of_customers. The result should contain columns Region, no_of_customers

select Region, count(Cust_id) as no_of_customers
from cust_dimen
group by Region
order by no_of_customers Desc;

# C. Find the region having maximum customers (display the region name and max(no_of_customers)

select Region as region_name, count(Cust_id) as no_of_customers
from cust_dimen
group by Region
having count(Cust_id) = (select max(no_of_customers) from (select count(Cust_id) as no_of_customers from cust_dimen group by Region) as t);

# D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)

select Prod_id as product_id, sum(Order_Quantity) as no_of_products_sold
from market_fact
group by Prod_id
order by no_of_products_sold desc;

# E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)

select cd.Customer_Name as cutomer_name, sum(mf.Order_Quantity) as no_of_tables_purchased
from market_fact mf inner join cust_dimen cd on mf.Cust_id=cd.Cust_id
					inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
where cd.Region = "ATLANTIC" and pd.Product_Sub_Category = "TABLES"
group by mf.Cust_id;

# Task 3: Advanced Analysis

# A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?

select pd.Product_category as product_category, sum(mf.Profit) as profits
from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
group by pd.Product_Category
order by profits desc;

# B. Display the product category, product sub-category and the profit within each sub-category in three columns.

select pd.Product_Category as product_category, pd.Product_Sub_Category as product_sub_category, sum(mf.Profit) as profits
from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
group by pd.Product_Sub_Category;

# C. Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)

# i. Where is the least profitable product subcategory shipped the most?

select cd.Region as region, count(mf.Ship_id) as no_of_shipments
from market_fact mf inner join cust_dimen cd on mf.Cust_id=cd.Cust_id
					inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
where pd.Product_Sub_Category = 
(select pd.Product_Sub_Category
from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
group by pd.Product_Sub_Category
having sum(mf.profit) = (select min(profit) from (select sum(mf.profit) as profit from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id group by pd.Product_Sub_Category) as T))
group by cd.Region
having no_of_shipments = 
(select max(no_of_shipments) from 
(select count(mf.Ship_id) as no_of_shipments 
from market_fact mf inner join cust_dimen cd on mf.Cust_id=cd.Cust_id 
					inner join prod_dimen pd on mf.Prod_id=pd.Prod_id 
where pd.Product_Sub_Category = 
(select pd.Product_Sub_Category
from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
group by pd.Product_Sub_Category
having sum(mf.profit) = (select min(profit) from (select sum(mf.profit) as profit from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id group by pd.Product_Sub_Category) as T))
group by cd.Region) as y);

# ii. For the least profitable product sub-category, display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)

select cd.Region as region, count(mf.Ship_id) as no_of_shipments, sum(mf.Profit) as profit_in_each_region
from market_fact mf inner join cust_dimen cd on mf.Cust_id=cd.Cust_id
					inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
where pd.Product_Sub_Category = (select pd.Product_Sub_Category
from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id
group by pd.Product_Sub_Category
having sum(mf.profit) = (select min(profit) from (select sum(mf.profit) as profit from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id group by pd.Product_Sub_Category) as T))
group by cd.Region
order by profit_in_each_region desc;