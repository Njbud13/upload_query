-module(persist).

%% Setup
-export([ init_db/0
        ]).

%% API
-export([ add_content/5
        , file_exists/3
        , get_id/2
        , get_is_payable/1
        , mark_file_written/1
        , store_file/4
        , read_file/3
        , set_is_paid/1
        ]).

%% Utils
-export([ get_receiver_ids/1
        , try_read_id/1
        , all_keys/0
        ]).

%% Types
-type content_id() :: integer().
-type sender_id() :: integer().
-type file_type() :: string().
-type file_name() :: binary().
-type receiver_id() :: integer().
-type is_payable() :: boolean().
-type file_data() :: binary().

-export_type([ content_id/0
             , sender_id/0
             , receiver_id/0
             , file_type/0
             , file_name/0
             , file_data/0
             , is_payable/0
             ]).

-define(POOL, pgdb).

-spec init_db() -> ok.
init_db() ->
    [ {ok, [], []}
    , {ok, [], []}
    , {ok, [], []}
    , {ok, [], []}
    ] =
    pgapp:squery(?POOL, "
        DROP TABLE IF EXISTS content;
        CREATE TABLE content (
            id serial NOT NULL PRIMARY KEY,
            sender_id bigint NOT NULL,
            file_type varchar(256) NOT NULL,
            file_name varchar(256) NOT NULL,
            receiver_id bigint NOT NULL,
            is_payable boolean NOT NULL DEFAULT TRUE,
            file_written boolean NOT NULL DEFAULT FALSE,
            created_at timestamp DEFAULT current_timestamp,
            updated_at timestamp DEFAULT NULL,
            paid_at timestamp DEFAULT NULL
        );
       CREATE INDEX sender_receiver_index ON content(sender_id, receiver_id);
       CREATE INDEX sender_file_name_index ON content(sender_id, file_name);
       "),
    ok.

-spec add_content(sender_id(), file_type(), file_name(), receiver_id(), is_payable()) ->
  {ok, content_id()} | error.
add_content(SenderId, FileType, FileName, ReceiverId, IsPayable) ->
    case pgapp:equery(?POOL,
                      "INSERT INTO content(sender_id, file_type, file_name, receiver_id, is_payable)
                       VALUES ($1, $2, $3, $4, $5)
                       RETURNING id;
                       ",
                      [SenderId, FileType, FileName, ReceiverId, IsPayable]) of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        _ -> error % TODO: alarm? return reason?
    end.

-spec mark_file_written(content_id()) -> ok | error.
mark_file_written(Id) ->
    case pgapp:equery(?POOL, "UPDATE content SET file_written=TRUE WHERE id=$1", [Id]) of
        {ok, Count} when Count =:= 1 -> ok;
        _ -> error % TODO: Handle gracefully? Alarm?
    end.

-spec file_exists(sender_id(), receiver_id(), file_name()) -> boolean().
file_exists(SenderId, ReceiverId, FileName) ->
    SQL = "SELECT id FROM content "
          "WHERE sender_id=$1 AND receiver_id=$2 AND file_name=$3 AND file_written=TRUE",
    Params = [SenderId, ReceiverId, FileName],
    case pgapp:equery(?POOL, SQL, Params) of
        {ok, _, []} -> false;
        {ok, _, List} when is_list(List) -> true;
        _ -> false
    end.

all_keys() ->
  SQL = "SELECT id FROM content",
  Params = [],
  case pgapp:equery(?POOL, SQL, Params) of
    {ok, _, List} when is_list(List) -> List;
    _ -> []
  end.

-spec try_read_id(content_id()) -> {ok, #{id := content_id()
                                        , sender_id := sender_id()
                                        , file_type := file_type()
                                        , file_name := file_name()
                                        , receiver_id := receiver_id()
                                        , is_payable := is_payable()}} |
                                   none.
try_read_id(Id) ->
    SQL = "SELECT id, sender_id, file_type, file_name, receiver_id, is_payable "
          "FROM content "
          "WHERE id=$1",
    Params = [Id],
    case pgapp:equery(?POOL, SQL, Params) of
        {ok, _, [{Id, SenderId, FileType, FileName, ReceiverId, IsPayable}]} ->
            {ok, #{id => Id
                 , sender_id => SenderId
                 , file_type => FileType
                 , file_name => FileName
                 , receiver_id => ReceiverId
                 , is_payable => IsPayable}};
        _ ->
            none
    end.

%% TODO: Assumption on unique Sender<->Receiver sets here
-spec get_id(sender_id(), receiver_id()) -> {ok, content_id()} | {error, atom()}.
get_id(SenderId, ReceiverId) ->
  case pgapp:equery(?POOL, "SELECT id FROM content WHERE sender_id=$1 AND receiver_id=$2",
                    [SenderId, ReceiverId]) of
    {ok, _, []} -> {error, none};
    {ok, _, [{Id}]} -> {ok, Id};
    {ok, _, List} when length(List) > 1 -> {error, multiple_ids};
    _ -> {error, other}
  end.

-spec get_is_payable(content_id()) -> {ok, boolean()} | {error, atom()}.
get_is_payable(ContentId) ->
  case pgapp:equery(?POOL, "SELECT is_payable FROM content WHERE id=$1", [ContentId]) of
    {ok, _, []} -> {error, none};
    {ok, _, [{Bool}]} -> {ok, Bool};
    {ok, _, List} when length(List) > 1 -> {error, multiple_ids};
    _ -> {error, other}
  end.

-spec set_is_paid(content_id()) -> ok | error.
set_is_paid(Id) ->
  case pgapp:equery(?POOL, "UPDATE content SET is_payable=FALSE WHERE id=$1", [Id]) of
    {ok, Count} when Count =:= 1 -> ok;
    _ -> error % TODO: Handle gracefully? Alarm?
  end.

-spec get_receiver_ids(sender_id()) -> [{content_id(), receiver_id()}] | none.
get_receiver_ids(SenderId) ->
    case pgapp:equery(?POOL, "SELECT id, receiver_id FROM content WHERE sender_id=$1", [SenderId]) of
        {ok, _, []} -> none;
        {ok, _, List} -> List;
        _ -> none
    end.

%% TODO: decide on storage solution, for now save to disk
-spec store_file(sender_id(), receiver_id(), file_name(), file_data()) -> ok.
store_file(SenderId, ReceiverId, FileName, FileData) ->
  {ok, App} = application:get_application(),
  FilePath = filename:join([ application:get_env(App, file_store_base_path, "/tmp")
                           , integer_to_list(SenderId)
                           , integer_to_list(ReceiverId)
                           , FileName
                           ]),
  filelib:ensure_dir(FilePath),
  ok = file:write_file(FilePath, FileData).

-spec read_file(sender_id(), receiver_id(), file_name()) -> {ok, file_data()} | {error, atom()}.
read_file(SenderId, ReceiverId, FileName) ->
  {ok, App} = application:get_application(),
  FilePath = filename:join([ application:get_env(App, file_store_base_path, "/tmp")
                           , integer_to_list(SenderId)
                           , integer_to_list(ReceiverId)
                           , FileName
                           ]),
  file:read_file(FilePath).
