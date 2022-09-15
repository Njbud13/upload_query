-module(upload_handler).
-behaviour(cowboy_handler).

%% Callbacks
-export([ init/2
        ]).

%% Types

-type init_map() :: #{sender_id => persist:sender_id()
                    , file_type => persist:file_type()
                    , file_name => persist:file_name()
                    , receiver_id => persist:receiver_id()
                    , is_payable => persist:is_payable()
                    , file_data => persist:file_data()}.

-type result_map() :: #{sender_id := persist:sender_id()
                      , file_type := persist:file_type()
                      , file_name := persist:file_name()
                      , receiver_id := persist:receiver_id()
                      , is_payable := persist:is_payable()
                      , file_data := persist:file_data()}.

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req0 = #{method := <<"GET">>}, State) ->
  Req = cowboy_req:reply(405, #{<<"allow">> => <<"POST">>}, Req0),
  {ok, Req, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    %% TODO: Add audit logging
    %% TODO: Add authentication/authorization?
    {Req1, Result} = acc_multipart(Req0, maps:new()),
    case valid_call(Result) of
        true ->
            Id = store_result(Result),
            Req = cowboy_req:reply(200, #{}, integer_to_binary(Id), Req1),
            {ok, Req, State};
        false ->
            Req = cowboy_req:reply(400, #{}, <<"invalid_call">>, Req1),
            {ok, Req, State}
    end.

%% -------------------------------------------------------------------------------------------------
%% Multipart parser

-spec acc_multipart(cowboy_req:req(), init_map()) -> {cowboy_req:req(), result_map()}.
acc_multipart(Req, AccMap) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            [Req4, Map] =
            case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    handle_duplicate_key(FieldName, AccMap),
                    {ok, MyBody, Req3} = cowboy_req:read_part_body(Req2),
                    [Req3, convert_input(FieldName, MyBody)];
                {file, _FieldName, Filename, _CType} ->
                    handle_multiple_files(AccMap),
                    {Req5, [FileData]} = stream_file(Req2, []),
                    [Req5, #{file_name => binary_to_list(Filename), file_data => FileData}]
            end,
            acc_multipart(Req4, maps:merge(AccMap, Map));
        {done, Req2} ->
            {Req2, AccMap}
    end.

stream_file(Req, Acc0) ->
    case cowboy_req:read_part_body(Req) of
        {ok, Body, Req2} ->
            {Req2, lists:append(Acc0, [Body])};
        {more, Body, Req2} ->
            Acc = lists:append(Acc0, [Body]),
            stream_file(Req2, Acc)
    end.

%% -------------------------------------------------------------------------------------------------
%% Storage

-spec store_result(result_map()) -> persist:content_id().
store_result(#{sender_id := SenderId,
               file_type := FileType,
               file_name := FileName,
               receiver_id := ReceiverId,
               is_payable := IsPayable,
               file_data := FileData}) ->
    {ok, Id} = persist:add_content(SenderId, FileType, FileName, ReceiverId, IsPayable),
    ok = persist:store_file(SenderId, ReceiverId, FileName, FileData),
    %% TODO: add alarm for old non-written files
    ok = persist:mark_file_written(Id),
    Id.

%% -------------------------------------------------------------------------------------------------
%% Validation

-spec valid_call(result_map()) -> boolean().
%% TODO: rewrite to return 'ok | {error, Reason}'
valid_call(Result) ->
    case maps:find(sender_id, Result) of
        {ok, SenderId} when is_integer(SenderId) -> true;
        _ -> false
    end andalso
    case maps:find(receiver_id, Result) of
        {ok, ReceiverId} when is_integer(ReceiverId) -> true;
        _ -> false
    end andalso
    case maps:find(is_payable, Result) of
        {ok, Value} when is_boolean(Value) -> Value;
        _ -> false
    end andalso
    maps:is_key(file_name, Result) andalso
    %% TODO: Is any file ok to store?
    maps:is_key(file_data, Result) andalso
    %% TODO: Should this be allowed? Overwrite or store duplicate files? Based on receiver_id?
    case persist:file_exists(maps:get(sender_id, Result), maps:get(file_name, Result)) of
        false -> true;
        true -> false
    end.

%% -------------------------------------------------------------------------------------------------
%% Helpers

handle_duplicate_key(Key, Map) ->
    case maps:is_key(Key, Map) of
        true ->
            %% TODO: Duplicate kay, how to handle? Log for now
            logger:warning(#{upload_handler => handle_duplicate_key,
                             duplicate_key => Key});
        false ->
            ok
    end.

handle_multiple_files(AccMap) ->
    case maps:is_key(file_name, AccMap) of
        true ->
            %% TODO: Multiple files uploaded, how to handle? Log for now
            logger:warning(#{upload_handler => handle_multiple_files,
                             duplicate_file_name => maps:get(file_name, AccMap)});
        false ->
            ok
    end.

convert_input(<<"sender_id">>, Binary) when is_binary(Binary) ->
    #{sender_id => binary_to_integer(Binary)};
convert_input(<<"receiver_id">>, Binary) when is_binary(Binary) ->
    #{receiver_id => binary_to_integer(Binary)};
convert_input(<<"is_payable">>, Binary) when is_binary(Binary) ->
    #{is_payable => binary_to_atom(Binary)};
%% TODO: Consider changing this to automatic detection
convert_input(<<"file_type">>, Binary) when is_binary(Binary) ->
    #{file_type => binary_to_list(Binary)}.
