alter table product_sales add transactionID serial;

alter table product_sales
	add constraint product_sales_pk
	primary key (transactionID)
