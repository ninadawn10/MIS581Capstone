select (product_sales.*)::text, count(*) 
from product_sales
	group by product_sales.*
	having count(*) > 1
	;
