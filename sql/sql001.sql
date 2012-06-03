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
       status smallint default 0,
       created_at timestamp,
       Constraint "teams_pkey" Primary Key("id"));

alter table teams add column status_description text;

create index teams_owner_id_id_index on teams (id, owner_id);
create index teams_status_id_index on teams (status, id);


create table usrs_teams (
       usr_id integer not null,
       team_id integer not null,
       created_at timestamp,
       Constraint "usr_id_team_id_pkey" Primary Key("usr_id", "team_id"));

create index usrs_teams_index on usrs_teams (usr_id, team_id, created_at);

create table messages (
       id bigserial not null,
       usr_id integer not null,
       team_id integer not null,
       text varchar(420),
       created_at timestamp,
       lat varchar(64),
       lng varchar(64),
       Constraint "messages_pkey" Primary Key("id"));

create index messages_usr_id_id_index on messages (usr_id, id);
create index messages_team_id_id_index on messages (team_id, id);

alter table messages add column in_article_id bigint;
alter table messages add column in_reply_to_id bigint;

create table articles (
       id serial not null,
       usr_id integer not null,
       team_id integer not null,
       title varchar(256),
       text text,
       type smallint default 0 not null,
       created_at timestamp,
       lat varchar(64),
       lng varchar(64),
       Constraint "articles_pkey" Primary Key("id"));

alter table articles add column status smallint;
alter table articles add column progress smallint;
       
create index articles_usr_id_id_index on articles (usr_id, id);
create index articles_team_id_id_index on articles (team_id, id);
