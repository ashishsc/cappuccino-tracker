CREATE SCHEMA IF NOT EXISTS cap;

CREATE TABLE IF NOT EXISTS cap.purchases (
	id SERIAL PRIMARY KEY,
	description text,
    cents  integer NOT NULL
);

CREATE TABLE IF NOT EXISTS cap.caps (
    id SERIAL PRIMARY KEY,
    cents integer NOT NULL,
    shop text
)