--
-- testdb schema
--

CREATE TABLE category (
    id bigserial,
    title text,
    PRIMARY KEY (id)
);

CREATE TABLE item (
    id bigserial,
    category_id bigint,
    title text,
    num int,
    PRIMARY KEY (id),
    FOREIGN KEY (category_id) REFERENCES category (id) ON DELETE SET NULL
);
