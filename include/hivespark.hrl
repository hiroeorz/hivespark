
-define(DB_SRV, {global, dbsrv}).
-define(DB, pool1).

-record(http_state, {session_key :: binary()}).

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
               owner_id         ::integer(),
               icon_url = ""    ::string(),
               description = "" ::string(),
               created_at       ::tuple()}).

-record(usr_team, {usr_id       ::integer(),
                   team_id      ::integer(),
                   created_at   ::tuple()}).

-record(message, {id            ::binary(),
                  usr_id        ::integer(),
                  text          ::binary(),
                  created_at    ::tuple(),
                  lat           ::string(),
                  lng           ::string()}).

-define(ROUTE,
        C == <<"auth">>; A == <<"login">>,
        C == <<"usr_public">>; A == <<"create">>,
        C == <<"team_public">>; A == <<"create">>).

-define(AUTHENTICATED_ROUTE, 
        C == <<"usr">>; A == <<"show">>,
        C == <<"team">>; A == <<"all">>,
        C == <<"team">>; A == <<"list">>,
        C == <<"team">>; A == <<"add_usr">>,
        C == <<"team">>; A == <<"checkin">>,
        C == <<"team">>; A == <<"show_checkin">>,
        C == <<"team">>; A == <<"send_message">>,
        C == <<"team">>; A == <<"get_messages">>,
        C == <<"message">>; A == <<"send">>
       ).
