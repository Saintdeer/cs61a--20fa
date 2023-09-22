.read data.sql


CREATE TABLE average_prices AS
  SELECT category, avg(MSRP) AS average_price
  FROM products
  GROUP BY category;


CREATE TABLE lowest_prices AS
  SELECT store, item, min(price)
  FROM inventory
  GROUP BY item;


CREATE TABLE best_deal AS
  SELECT name, min(MSRP/rating)
  FROM products
  GROUP BY category;

CREATE TABLE shopping_list AS
  SELECT name, store
  FROM best_deal, lowest_prices
  WHERE name=item;


CREATE TABLE total_bandwidth AS
  SELECT sum(Mbs)
  FROM shopping_list AS a, stores AS b
  WHERE a.store=b.store;

