CREATE SCHEMA IF NOT EXISTS cap;
CREATE TABLE IF NOT EXISTS cap.purchases (
	id SERIAL PRIMARY KEY,
	description text,
    -- in cents
    cents  integer NOT NULL
)
