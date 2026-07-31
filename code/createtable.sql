drop table if exists product_sales;

create table product_sales (
	transactiondate date,
	region varchar(10),
	product varchar(15),
	quantity int,
	unitprice numeric(10, 2),
	storelocation varchar(10),
	customertype varchar (20),
	discount numeric(10, 2),
	salesperson varchar(20),
	totalprice numeric(10, 2),
	paymentmethod varchar (20),
	promotion varchar(15),
	returned int,
	orderid varchar(15),
	customername varchar(15),
	shippingcost numeric(10, 2),
	orderdate date,
	deliverydate date,
	regionmanager varchar(15)
);

alter table product_sales
	add transactionID serial;

alter table product_sales
	add constraint product_sales_pk
	primary key (transactionID)
