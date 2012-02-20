
-define(DB_SRV, {global, dbsrv}).
-define(DB, pool1).

-record(usr, {id               ::integer(),
              name             ::string(),
              longname = ""    ::string(),
              email            ::string(),
              password         ::binary(),
              password_seed    ::binary(),
              icon_url = ""    ::string(),
              lat = ""         ::string(),
              lng = ""         ::string(),
              description = "" ::string(),
              created_at       ::tuple()}).

-record(team, {id               ::integer(),
               name             ::string(),
               icon_url = ""    ::string(),
               description = "" ::string(),
               created_at       ::tuple()}).
