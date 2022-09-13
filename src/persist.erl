-module(persist).

%% Setup
-export([ init_db/0
        ]).

%% API
-export([ add_content/5
        , file_exists/2
        , mark_file_written/1
        , store_file/4
        ]).

%% Utils
-export([ get_receiver_ids/1
        , try_read_id/1
        , all_keys/0
        ]).

-type seq_id() :: integer().
-define(POOL, pgdb).

-export_type([ seq_id/0
             ]).

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

-spec add_content(upload_handler:sender_id(),
                  upload_handler:file_type(),
                  upload_handler:file_name(),
                  upload_handler:receiver_id(),
                  upload_handler:is_payable()) -> {ok, seq_id()} | error.
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

-spec mark_file_written(seq_id()) -> ok | error.
mark_file_written(Id) ->
    case pgapp:equery(?POOL, "UPDATE content SET file_written=TRUE WHERE id=$1", [Id]) of
        {ok, Count} when Count =:= 1 -> ok;
        _ -> error % TODO: Handle gracefully? Alarm?
    end.

-spec file_exists(upload_handler:sender_id(), upload_handler:file_name()) -> boolean().
file_exists(SenderId, FileName) ->
    SQL = "SELECT id FROM content WHERE sender_id=$1 AND file_name=$2 AND file_written=TRUE",
    Params = [SenderId, list_to_binary(FileName)],
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

get_receiver_ids(SenderId) ->
    case pgapp:equery(?POOL, "SELECT id, receiver_id FROM content WHERE sender_id=$1", [SenderId]) of
        {ok, _, []} -> none;
        {ok, _, List} -> List;
        _ -> none
    end.

%% TODO: decide on storage solution, for now save to disk and let cronjob transfer
store_file(SenderId, ReceiverId, FileName, FileData) ->
  {ok, App} = application:get_application(),
  FilePath = filename:join([ application:get_env(App, file_store_base_path, "/tmp")
                           , integer_to_list(SenderId)
                           , integer_to_list(ReceiverId)
                           , FileName
                           ]),
  filelib:ensure_dir(FilePath),
  ok = file:write_file(FilePath, FileData).
