-module(upload_handler_SUITE).

-behaviour(ct_suite).

%% Suite api
-export([ all/0
        , end_per_suite/1
        , init_per_suite/1
        ]).

%% Test cases
-export([ successful_upload/1
        , failed_upload/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%_* Suite API  ===============================================================

all() -> [ successful_upload
         , failed_upload
         ].

init_per_suite(Config) ->
  ok = upload_query:start(),
  ok = persist:init_db(),
  Config.

end_per_suite(_Config) ->
  ok = persist:init_db(),
  ok.

%%%_* Test cases ===============================================================

%% Given a file is uploaded
%% When all input-parameters are correct
%% Then the file will be stored in the db
successful_upload(Config) when is_list(Config) ->
  SenderId = integer_to_list(rand:uniform(100)), % any integer works
  ReceiverId = integer_to_list(rand:uniform(100)), % any integer works
  TestFile = filename:join([?config(data_dir, Config), "upload.me"]),
  Path = "http://localhost:8080/upload", %% TODO: make this configurable/automated
  OsCmd = io_lib:format("curl -s "
                        "-F 'sender_id=~p' "
                        "-F 'receiver_id=~p' "
                        "-F 'is_payable=~p' "
                        "-F 'file_type=~p' "
                        "-F 'file=\@~p' " ++
                        Path,
                       [SenderId, ReceiverId, "true", "test_file", TestFile]),
  meck:expect(persist, store_file, 4, ok),
  IdStr = os:cmd(OsCmd),
  Id = list_to_integer(IdStr),
  meck:unload(persist),
  {ok, Map} = persist:try_read_id(Id),
  ?assertEqual(Id, maps:get(id, Map)),
  ok.

%% Given a file is uploaded
%% When all input-parameters are NOT correct
%% Then the file will NOT be stored in the db
failed_upload(Config) when is_list(Config) ->
  AllKeys = persist:all_keys(),
  Error400 = "400",
  Error405 = "405",
  Error500 = "500",
  SenderId = integer_to_list(rand:uniform(100)), % any integer works
  ReceiverId = integer_to_list(rand:uniform(100)), % any integer works
  TestFile = filename:join([?config(data_dir, Config), "upload.me"]),
  Path = "http://localhost:8080/upload", %% TODO: make this configurable/automated
  OsStr = "curl -s -o /dev/null -i -w \"%{http_code}\" "
          "-F 'sender_id=~p' "
          "-F 'receiver_id=~p' "
          "-F 'is_payable=~p' "
          "-F 'file_type=~p' "
          "-F 'file=\@~p' " ++ Path,
  OsCmd1 = io_lib:format(OsStr, ["not_an_integer", ReceiverId, "true", "test_file", TestFile]),
  OsCmd2 = io_lib:format(OsStr, [SenderId, "not_an_integer", "true", "test_file", TestFile]),
  OsCmd3 = io_lib:format(OsStr, [SenderId, ReceiverId, "not_a_boolean", "test_file", TestFile]),
  OsCmd4 = io_lib:format("curl -X POST -s -o /dev/null -i -w \"%{http_code}\" "
                         "-F 'is_payable=true' " ++ Path, []),
  OsCmd5 = io_lib:format("curl -X GET -s -o /dev/null -i -w \"%{http_code}\" " ++ Path, []),
  ?assertEqual(Error500, os:cmd(OsCmd1)),
  ?assertEqual(Error500, os:cmd(OsCmd2)),
  ?assertEqual(Error400, os:cmd(OsCmd3)),
  ?assertEqual(Error400, os:cmd(OsCmd4)),
  ?assertEqual(Error405, os:cmd(OsCmd5)),
  ?assertEqual(length(AllKeys), length(persist:all_keys())),
  ok.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
