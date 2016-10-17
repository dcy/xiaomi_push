-module(xiaomi_push).

%%API
-export([regid_message/1, regid_message/2,
         regid_notification/3, regid_notification/5, %%通知栏
         regid_pass_through/2, regid_pass_through/4, %%透传

         regid_messages/1, regid_messages/2,

         all_message/1, all_message/2,
         all_notificaiton/2, all_notificaiton/4,
         all_pass_through/1, all_pass_through/3,

         topic_message/1, topic_message/2,
         topic_notification/3, topic_notification/5,
         topic_pass_through/2,  topic_pass_through/4,

         gen_authorization/1, gen_headers/1
        ]).

-export([send/3, send_for_headers/3, urlencode_messages/1]).

-include_lib("eutil/include/eutil.hrl").

gen_authorization(AppSecret) ->
    <<"key=", (list_to_binary(AppSecret))/binary>>.

gen_headers(AppSecret) ->
    Auth = gen_authorization(AppSecret),
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>},
     {<<"Authorization">>, Auth}].

%% 向某个regid或一组regid列表推送某条消息
regid_message(MsgMaps) ->
    AppSecret = get_conf_app_secret(),
    regid_message(AppSecret, MsgMaps).

regid_message(AppSecret, MsgMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
    send(AppSecret, URL, MsgMaps).


regid_notification(RegIds, Title, Desc) ->
    PkgName = get_conf_pkg_name(),
    AppSecret = get_conf_app_secret(),
    regid_notification(AppSecret, PkgName, RegIds, Title, Desc).

regid_notification(AppSecret, PkgName, RegIds, Title, Desc) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"pass_through">> => 0,
                <<"title">> => unicode:characters_to_binary(Title),
                <<"description">> => unicode:characters_to_binary(Desc),
                <<"notify_type">> => -1,
                <<"extra.notify_effect">> => 1,
                <<"registration_id">> => list_to_binary(RegIds)
               },
    regid_message(AppSecret, MsgMaps).


regid_pass_through(RegIds, Payload) ->
    PkgName = get_conf_pkg_name(),
    AppSecret = get_conf_app_secret(),
    regid_pass_through(AppSecret, PkgName, RegIds, Payload).

regid_pass_through(AppSecret, PkgName, RegIds, Payload) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"pass_through">> => 1,
                <<"payload">> => Payload,
                <<"extra.notify_effect">> => 1,
                <<"registration_id">> => list_to_binary(RegIds)
               },
    regid_message(AppSecret, MsgMaps).

%% 向不同的regid发送不同的消息
regid_messages(Messages) ->
    AppSecret = get_conf_app_secret(),
    regid_messages(AppSecret, Messages).

regid_messages(AppSecret, Messages) ->
    URL = <<"https://api.xmpush.xiaomi.com/v2/multi_messages/regids">>,
    MsgMaps = #{<<"messages">> => jiffy:encode(Messages)},
    send(AppSecret, URL, MsgMaps).


%%向所有设备推送某条消息
all_message(MsgMaps) ->
    AppSecret = get_conf_app_secret(),
    all_message(AppSecret, MsgMaps).

all_message(AppSecret, MsgMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/all">>,
    send(AppSecret, URL, MsgMaps).

all_notificaiton(Title, Desc) ->
    AppSecret = get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    all_notificaiton(AppSecret, PkgName, Title, Desc).

all_notificaiton(AppSecret, PkgName, Title, Desc) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"pass_through">> => 0,
                <<"title">> => unicode:characters_to_binary(Title),
                <<"description">> => unicode:characters_to_binary(Desc),
                <<"notify_type">> => -1,
                <<"extra.notify_effect">> => 1},
    all_message(AppSecret, MsgMaps).

all_pass_through(Payload) ->
    AppSecret = get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    all_pass_through(AppSecret, PkgName, Payload).

all_pass_through(AppSecret, PkgName, Payload) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"pass_through">> => 1,
                <<"payload">> => Payload,
                <<"notify_type">> => -1,
                <<"extra.notify_effect">> => 1},
    all_message(AppSecret, MsgMaps).


%%向某个topic推送某条消息
topic_message(MsgMaps) ->
    AppSecret = get_conf_app_secret(),
    topic_message(AppSecret, MsgMaps).

topic_message(AppSecret, MsgMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/topic">>,
    send(AppSecret, URL, MsgMaps).

topic_notification(Topic, Title, Desc) ->
    AppSecret = get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    topic_notification(AppSecret, PkgName, Topic, Title, Desc).

topic_notification(AppSecret, PkgName, Topic, Title, Desc) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"pass_through">> => 0,
                <<"title">> => unicode:characters_to_binary(Title),
                <<"description">> => unicode:characters_to_binary(Desc),
                <<"notify_type">> => -1,
                <<"extra.notify_effect">> => 1,
                <<"topic">> => list_to_binary(Topic)
               },
    topic_message(AppSecret, MsgMaps).

topic_pass_through(Topic, Payload) ->
    AppSecret = get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    topic_pass_through(AppSecret, PkgName, Topic, Payload).

topic_pass_through(AppSecret, PkgName, Topic, Payload) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"pass_through">> => 1,
                <<"payload">> => Payload,
                <<"notify_type">> => -1,
                <<"extra.notify_effect">> => 1,
                <<"topic">> => list_to_binary(Topic)
               },
    topic_message(AppSecret, MsgMaps).
    





    


send(AppSecret, URL, MsgMaps) ->
    ?TRACE_VAR(URL),
    ?TRACE_VAR(MsgMaps),
    Headers = gen_headers(AppSecret),
    send_for_headers(Headers, URL, MsgMaps).

send_for_headers(Headers, URL, MsgMaps) ->
    Method = post,
    Payload = eutil:urlencode(MsgMaps),
    Options = [{pool, xiaomi}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    #{<<"code">>:=Code} = Result,
    case Code of
        0 ->
            {ok, Code};
        _ ->
            ?ERROR_MSG("epush xiaomi error, URL: ~p, MsgMaps: ~p, Result: ~p", [URL, MsgMaps, Result]),
            {error, Code}
    end.

get_conf_pkg_name() ->
    {ok, PkgName} = application:get_env(xiaomi_push, pkg_name),
    PkgName.

get_conf_app_secret() ->
    {ok, AppSecret} = application:get_env(xiaomi_push, app_secret),
    AppSecret.

urlencode_messages(Messages) ->
    Fun = fun(Message) ->
                  eutil:urlencode(Message)
          end,
    Messages1 = lists:map(Fun, Messages),
    ?TRACE_VAR(Messages1),
    lists:flatten(io_lib:format("~p", [Messages])).
