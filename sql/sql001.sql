create table usrs (
       id serial not null,
       name varchar(64) not null unique,
       longname varchar(64) not null,
       email varchar(64) not null unique,
       password varchar(32) not null,
       password_seed varchar(32) not null,
       icon_url varchar(256) not null default '',
       description text not null default '',
       created_at timestamp,
       lat varchar(64),
       lng varchar(64),
       Constraint "usrs_pkey" Primary Key("id"));


create table teams (
       id serial not null,
       name varchar(64) not null unique,
       owner_id integer,
       icon_url varchar(256) not null default '',
       description text not null default '',
       created_at timestamp,
       Constraint "teams_pkey" Primary Key("id"));

create table usrs_teams (
       usr_id integer not null,
       team_id integer not null,
       created_at timestamp,
       Constraint "usr_id_team_id_pkey" Primary Key("usr_id", "team_id"));