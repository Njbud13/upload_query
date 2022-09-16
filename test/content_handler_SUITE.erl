-module(content_handler_SUITE).

-behaviour(ct_suite).

%% Suite api
-export([ all/0
        , end_per_suite/1
        , init_per_suite/1
        ]).

%% Test cases
-export([ query_content_id/1
        , query_sender_id/1
        , query_sender_receiver_id/1
        , failed_query/1
        , set_is_paid/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%_* Suite API  ===============================================================

all() -> [ query_content_id
         , query_sender_id
         , query_sender_receiver_id
         , failed_query
         , set_is_paid
         ].

init_per_suite(Config) ->
  ok = upload_query:start(),
  ok = persist:init_db(),
  Config.

end_per_suite(_Config) ->
  ok = persist:init_db(),
  ok.

%%%_* Test cases ===============================================================

%% Given a query on content_id
%% When the content exists
%% Then the file shall be downloaded by the client
query_content_id(Config) when is_list(Config) ->
  TestFile = filename:join([?config(data_dir, Config), "download.me"]),
  ReceiverId = rand:uniform(100), % any integer works
  FileType = "not_important",
  FileName = "download.me",
  SenderId = ensure_unique_sender_id(),
  IsPayable = true,
  {ok, ContentId} = persist:add_content(SenderId, FileType, FileName, ReceiverId, IsPayable),
  ok = persist:mark_file_written(ContentId),
  meck:expect(persist, read_file, 3, file:read_file(TestFile)),
  Path = "http://localhost:8080/content?content_id=" ++ integer_to_list(ContentId), % Configurable?
  {ok, {Status, Headers, Body}} = httpc:request(Path),
  meck:unload(persist),
  ?assertEqual(200, element(2, Status)),
  ?assertEqual("application/octet-stream", proplists:get_value("content-type", Headers)),
  ?assert(length(Body) > 0), % Assert something is there, not the actual content
  ok.

%% Given a query on content_id
%% When the content exists
%% Then the file shall be downloaded by the client
query_sender_id(Config) when is_list(Config) ->
  ReceiverId = rand:uniform(100), % any integer works
  FileType = "not_important",
  FileName = "download.me",
  SenderId = ensure_unique_sender_id(),
  IsPayable = true,
  {ok, ContentId} = persist:add_content(SenderId, FileType, FileName, ReceiverId, IsPayable),
  ok = persist:mark_file_written(ContentId),
  Path = "http://localhost:8080/content?sender_id=" ++ integer_to_list(SenderId), % Configurable?
  {ok, {Status, Headers, Body}} = httpc:request(Path),
  ?assertEqual(200, element(2, Status)),
  ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
  BodyMap = jsone:decode(list_to_binary(Body)),
  ContentMap = hd(maps:get(<<"array">>, BodyMap)),
  ?assertEqual(ContentId, maps:get(<<"content_id">>, ContentMap)),
  ?assertEqual(ReceiverId, maps:get(<<"receiver_id">>, ContentMap)),
  ok.

%% Given a query on content_id
%% When the content exists
%% Then the file shall be downloaded by the client
query_sender_receiver_id(Config) when is_list(Config) ->
  ReceiverId = rand:uniform(100), % any integer works
  FileType = "not_important",
  FileName = "download.me",
  SenderId = ensure_unique_sender_id(),
  IsPayable = true,
  {ok, ContentId} = persist:add_content(SenderId, FileType, FileName, ReceiverId, IsPayable),
  ok = persist:mark_file_written(ContentId),
  Path = "http://localhost:8080/content?sender_id=" ++ integer_to_list(SenderId) % Configurable?
          ++ "&receiver_id=" ++ integer_to_list(ReceiverId),
  {ok, {Status, Headers, Body}} = httpc:request(Path),
  ?assertEqual(200, element(2, Status)),
  ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
  BodyMap = jsone:decode(list_to_binary(Body)),
  ?assertEqual(true, binary_to_atom(maps:get(integer_to_binary(ContentId), BodyMap))),
  ok.

%% Given a file is uploaded
%% When all input-parameters are NOT correct
%% Then the file will NOT be stored in the db
failed_query(Config) when is_list(Config) ->
  ReceiverId = rand:uniform(100), % any integer works
  FileType = "not_important",
  FileName = "download.me",
  SenderId = ensure_unique_sender_id(),
  IsPayable = true,
  {ok, ContentId} = persist:add_content(SenderId, FileType, FileName, ReceiverId, IsPayable),
  ContentIdPath = "http://localhost:8080/content?content_id=" ++ integer_to_list(ContentId),
  {ok, {ContentId404, Headers, Body}} = httpc:request(ContentIdPath),
  ok = persist:mark_file_written(ContentId),
  WrongSenderId = ensure_unique_sender_id(),
  SenderIdPath = "http://localhost:8080/content?sender_id=" ++ integer_to_list(WrongSenderId),
  SenderReceiverPath = "http://localhost:8080/content?sender_id=" ++ integer_to_list(WrongSenderId)
                                               ++ "&receiver_id=" ++ integer_to_list(ReceiverId),
  {ok, {SenderId404, Headers, Body}} = httpc:request(SenderIdPath),
  {ok, {SenderReceiver404, Headers, Body}} = httpc:request(SenderReceiverPath),
  ?assertEqual(404, element(2, ContentId404)),
  ?assertEqual(404, element(2, SenderId404)),
  ?assertEqual(404, element(2, SenderReceiver404)),
  ok.

set_is_paid(Config) when is_list(Config) ->
  ReceiverId = rand:uniform(100), % any integer works
  FileType = "not_important",
  FileName = "download.me",
  SenderId = ensure_unique_sender_id(),
  IsPayable = true,
  {ok, ContentId} = persist:add_content(SenderId, FileType, FileName, ReceiverId, IsPayable),
  ok = persist:mark_file_written(ContentId),
  {ok, PreUpdate} = persist:try_read_id(ContentId),
  ?assert(maps:get(is_payable, PreUpdate)),
  {ok, {Status, _Headers, _Body}} =
    httpc:request(post,
                  { "http://localhost:8080/content"
                  , []
                  , "application/json"
                  , "{\"content_id\":\"" ++ integer_to_list(ContentId) ++ "\"}"},
                  [],
                  []),
  {ok, PostUpdate} = persist:try_read_id(ContentId),
  ?assertEqual(204, element(2, Status)),
  ?assertNot(maps:get(is_payable, PostUpdate)),
  ok.

ensure_unique_sender_id() -> ensure_unique_sender_id(1).

ensure_unique_sender_id(SenderId) ->
  case persist:get_receiver_ids(SenderId) of
    none -> SenderId;
    _ -> ensure_unique_sender_id(SenderId + 1)
  end.
  
%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
