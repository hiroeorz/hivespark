-define(APP, hivespark).
-define(DB_SRV, {global, dbsrv}).
-define(DB, pool1).
-define(PGSQL_DEFAULT_PARAMETERS, [{"integer_datetimes", "on"}]).

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
              created_at       ::non_neg_integer()}).

-record(team, {id                       ::integer(),
               name                     ::string(),
               owner_id                 ::integer(),
               icon_url = ""            ::string(),
               description = ""         ::string(),
               status = 0               ::integer(),
               status_description = ""  ::string(),
               created_at               ::non_neg_integer()}).

-record(usr_team, {usr_id       ::integer(),
                   team_id      ::integer(),
                   created_at   ::non_neg_integer()}).

-record(message, {id            ::integer(),
                  usr_id        ::integer(),
                  team_id       ::integer(),
                  text          ::binary(),
                  created_at    ::non_neg_integer(),
                  lat           ::string(),
                  lng           ::string()}).

-record(article, {id            ::binary(),
                  usr_id        ::integer(),
                  team_id       ::integer(),
                  title         ::binary(),
                  text          ::binary(),
                  type          ::integer(),
                  created_at    ::non_neg_integer(),
                  status        ::integer(),
                  progress      ::integer(),
                  lat           ::string(),
                  lng           ::string()}).

-define(ROUTE,
        C == <<"auth">>, A == <<"login">>;
        C == <<"auth">>, A == <<"index">>;
        C == <<"usr_public">>, A == <<"new">>;
        C == <<"usr_public">>, A == <<"create">>).

-define(AUTHENTICATED_ROUTE, 
        C == <<"usr">>, A == <<"show">>;
        C == <<"usr">>, A == <<"edit">>;
        C == <<"usr">>, A == <<"upload_icon">>;
        C == <<"usr">>, A == <<"show_myself">>;
        C == <<"usr">>, A == <<"update">>;
        C == <<"usr">>, A == <<"image">>;
        C == <<"usr">>, A == <<"save_image">>;
        C == <<"usr">>, A == <<"logout">>;
        C == <<"team">>, A == <<"index">>;
        C == <<"team">>, A == <<"new">>;
        C == <<"team">>, A == <<"show">>;
        C == <<"team">>, A == <<"edit">>;
        C == <<"team">>, A == <<"upload_icon">>;
        C == <<"team">>, A == <<"info">>;
        C == <<"team">>, A == <<"create">>;
        C == <<"team">>, A == <<"update">>;
        C == <<"team">>, A == <<"all">>;
        C == <<"team">>, A == <<"statuses_list">>;
        C == <<"team">>, A == <<"delete">>;
        C == <<"team">>, A == <<"list">>;
        C == <<"team">>, A == <<"add_usr">>;
        C == <<"team">>, A == <<"delete_usr">>;
        C == <<"team">>, A == <<"checkin">>;
        C == <<"team">>, A == <<"show_checkin">>;
        C == <<"team">>, A == <<"send_message">>;
        C == <<"team">>, A == <<"get_messages">>;
        C == <<"team">>, A == <<"save_image">>;
        C == <<"team">>, A == <<"image">>;
        C == <<"article">>, A == <<"create">>;
        C == <<"article">>, A == <<"update">>;
        C == <<"article">>, A == <<"teams_list">>;
        C == <<"article">>, A == <<"status_increment">>;
        C == <<"article">>, A == <<"status_decrement">>;
        C == <<"message">>, A == <<"send">>
       ).
