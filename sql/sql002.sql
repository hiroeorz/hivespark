create table teamfiles (
       id serial not null,
       team_id integer not null,
       name varchar(64) not null,
       owner_id integer,
       description text not null default '',
       created_at timestamp,
       updated_at timestamp,
       Constraint "teamfiles_pkey" Primary Key("id"));
