-module(content_handler).
-behaviour(cowboy_handler).

%% Callbacks
-export([ init/2
        ]).

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req0 = #{method := <<"GET">>}, State) ->
    Parsed = cowboy_req:parse_qs(Req0),
    Req = case decide_query_response(Parsed) of
            content_id -> query_content_id(Req0);
            receiver_id -> query_sender_receiver_id(Req0);
            sender_id -> query_sender_id(Req0)
          end,
    {ok, Req, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    Req = case cowboy_req:header(<<"content-type">>, Req0) of
            <<"application/json">> -> handle_is_paid(Req0);
            _ -> cowboy_req:reply(415, Req0)
          end,
    {ok, Req, State}.

handle_is_paid(Req0) ->
    {ok, PostVals, Req1} = cowboy_req:read_body(Req0),
    PostMap = jsone:decode(PostVals),
    case valid_post_call(PostMap) of
      true ->
        ContentId = binary_to_integer(maps:get(<<"content_id">>, PostMap)),
        %% TODO: Implement payment-api here
        ok = persist:set_is_paid(ContentId),
        Req1;
      error ->
        cowboy_req:reply(404, Req1)
    end.

valid_post_call(PostMap) ->
  %% TODO: Do we validate if it is already set to is_payable=false, if so where?
  case maps:find(<<"content_id">>, PostMap) of
    {ok, ContentId} -> is_integer(binary_to_integer(ContentId));
    error -> false
  end.

decide_query_response(Parsed) ->
  case lists:keymember(<<"content_id">>, 1, Parsed) of
    true ->
      content_id;
    false ->
      case lists:keymember(<<"sender_id">>, 1, Parsed) of
        true ->
          case lists:keymember(<<"receiver_id">>, 1, Parsed) of
            true -> receiver_id;
            false -> sender_id
          end;
        false -> invalid
      end
  end.

query_content_id(Req) ->
    #{content_id := Id} = cowboy_req:match_qs([{content_id, int}], Req),
    read_file_and_reply(Id, Req).

query_sender_receiver_id(Req) ->
  #{sender_id := SenderId, receiver_id := ReceiverId} =
    cowboy_req:match_qs([{sender_id, int}, {receiver_id, int}], Req),
  %% TODO: This causes 2 round-trips to PG, could be optimized
  case persist:get_id(SenderId, ReceiverId) of
    {ok, Id} ->
      case persist:get_is_payable(Id) of
        {ok, IsPayable} ->
          Json = jsone:encode(#{integer_to_binary(Id) => atom_to_binary(IsPayable)}),
          cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req);
        {error, _} ->
          %% TODO: Can this happen? Handle better than just 500
          logger:alert("is_payable not available for id=~p", [Id]),
          cowboy_req:reply(500, Req)
      end;
    {error, _} ->
      cowboy_req:reply(404, Req)
  end.

query_sender_id(Req) ->
  #{sender_id := SenderId} = cowboy_req:match_qs([{sender_id, int}], Req),
  case persist:get_receiver_ids(SenderId) of
    none ->
      cowboy_req:reply(404, Req);
    List0 ->
      List = [[{<<"content_id">>, ContentId}, {<<"receiver_id">>, ReceiverId}]
              || {ContentId, ReceiverId} <- List0],
      Json = jsone:encode(#{array => List}),
      cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req)
  end.

read_file_and_reply(Id, Req0) ->
  case persist:try_read_id(Id) of
    {ok, #{file_name := FileName, sender_id := SenderId, receiver_id := ReceiverId}} ->
      case persist:read_file(SenderId, ReceiverId, FileName) of
        {ok, Bin} ->
          CDBin = <<<<"attachment;filename=">>/binary, FileName/binary>>,
          cowboy_req:reply(200, #{<<"Content-Type">> => <<"application/octet-stream">>,
                                  <<"Content-Disposition">> => CDBin},
                           Bin, Req0);
        {error, _} -> cowboy_req:reply(404, Req0)
      end;
    none ->
      cowboy_req:reply(404, Req0)
  end.
