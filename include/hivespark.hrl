
-define(DB_SRV, {global, dbsrv}).

-record(usr, {id               ::integer(),
              name             ::string(),
              longname = ""    ::string(),
              email            ::string(),
              password         ::binary(),
              icon_url = ""    ::string(),
              lat = ""         ::string(),
              lng = ""         ::string(),
              description = "" ::string(),
              created_at       ::tuple()}).

-record(project, {id               ::integer(),
                  name             ::string(),
                  icon_url = ""    ::string(),
                  lat = ""         ::string(),
                  lng = ""         ::string(),
                  description = "" ::string(),
                  created_at       ::tuple()}).
