# --- !Ups

CREATE SEQUENCE users_id_seq;

CREATE TABLE users (
    id          INTEGER NOT NULL DEFAULT nextval('users_id_seq') PRIMARY KEY,
    email       VARCHAR(255) NOT NULL,
    name        VARCHAR(255) NOT NULL
);

CREATE SEQUENCE openid_associations_id_seq;

CREATE TABLE openid_associations (
    id          INTEGER NOT NULL DEFAULT nextval('openid_associations_id_seq') PRIMARY KEY,
    server_url  TEXT NOT NULL,
    handle      VARCHAR(255) NOT NULL,
    secret      TEXT NOT NULL,
    issued      INTEGER NOT NULL,
    lifetime    INTEGER NOT NULL,
    assoc_type  INTEGER NOT NULL
);

CREATE SEQUENCE openid_user_associations_id_seq;

CREATE TABLE openid_user_associations (
    id                  INTEGER NOT NULL DEFAULT nextval('openid_user_associations_id_seq') PRIMARY KEY,
    user_id             INTEGER NOT NULL,
    openid              INTEGER NOT NULL,
    created             TIMESTAMP WITH TIMEZONE NOT NULL DEFAULT current_timestamp()
); 

# --- !Downs

DROP TABLE IF EXISTS users;
DROP SEQUENCE IF EXISTS users_id_seq;
DROP TABLE IF EXISTS openid_associations;
DROP SEQUENCE IF EXISTS openid_associations_id_seq;
DROP TABLE IF EXISTS openid_user_associations;
DROP SEQUENCE IF EXISTS openid_user_associations_id_seq;

