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

         regid_subscribe/2, regid_subscribe/4,
         regid_unsubscribe/2, regid_unsubscribe/4,

         message_status/2, message_status/4,

         fetch_invalid_regids/0, fetch_invalid_regids/1,

         trace_status/1, trace_status/3,

         all_topic/1, all_topic/2,

         general_notification/5, general_app_msg/4,

         gen_authorization/1, gen_headers/1
        ]).

-export([send/3, send_for_headers/3]).

-include_lib("eutil/include/eutil.hrl").

-define(SUCCESS, 0).

gen_authorization(AppSecret) ->
    <<"key=", (eutil:to_binary(AppSecret))/binary>>.

gen_headers(AppSecret) ->
    Auth = gen_authorization(AppSecret),
    [?URLENCEDED_HEAD, {<<"Authorization">>, Auth}].

gen_get_headers(AppSecret) ->
    Auth = gen_authorization(AppSecret),
    [{<<"Authorization">>, Auth}].


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
    MsgMaps = #{<<"restricted_package_name">> => eutil:to_binary(PkgName),
                <<"pass_through">> => 0,
                <<"title">> => unicode:characters_to_binary(Title),
                <<"description">> => unicode:characters_to_binary(Desc),
                <<"notify_type">> => -1,
                <<"notify_id">> => erlang:system_time(second),
                <<"extra.notify_effect">> => 1,
                <<"registration_id">> => eutil:to_binary(RegIds)
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


%%订阅RegId的标签
regid_subscribe(RegId, Topic) ->
    AppSecret =  get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    regid_subscribe(AppSecret, PkgName, RegId, Topic).

regid_subscribe(AppSecret, PkgName, RegId, Topic) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"registration_id">> => list_to_binary(RegId),
                <<"topic">> => list_to_binary(Topic)},
	URL = <<"https://api.xmpush.xiaomi.com/v2/topic/subscribe">>,
    send(AppSecret, URL, MsgMaps).

%%取消订阅RegId的标签
regid_unsubscribe(RegId, Topic) ->
    AppSecret =  get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    regid_unsubscribe(AppSecret, PkgName, RegId, Topic).

regid_unsubscribe(AppSecret, PkgName, RegId, Topic) ->
    MsgMaps = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                <<"registration_id">> => list_to_binary(RegId),
                <<"topic">> => list_to_binary(Topic)},
    URL = <<"https://api.xmpush.xiaomi.com/v2/topic/unsubscribe">>,
    send(AppSecret, URL, MsgMaps).

%%获取消息的统计数据
message_status(StartDate, EndDate) ->
    AppSecret =  get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    message_status(AppSecret, PkgName, StartDate, EndDate).

message_status(AppSecret, PkgName, StartDate, EndDate) ->
    Query = #{<<"restricted_package_name">> => list_to_binary(PkgName),
              <<"start_date">> => list_to_binary(StartDate),
              <<"end_date">> => list_to_binary(EndDate)},
    URL = <<"https://api.xmpush.xiaomi.com/v1/stats/message/counters">>,
    Headers = gen_get_headers(AppSecret),
    Result = eutil:http_get(URL, Headers, Query, [{pool, xiaomi}]),
    case maps:get(<<"code">>, Result) of
        ?SUCCESS ->
            {ok, maps:get(<<"data">>, maps:get(<<"data">>, Result))};
        ErrCode ->
            {error, ErrCode}
    end.

%%追踪消息状态
trace_status(MsgId) ->
    AppSecret =  get_conf_app_secret(),
    PkgName = get_conf_pkg_name(),
    trace_status(AppSecret, PkgName, MsgId).

trace_status(AppSecret, PkgName, MsgId) ->
    Query = #{<<"restricted_package_name">> => list_to_binary(PkgName),
              <<"msg_id">> => eutil:to_binary(MsgId)},
    URL = <<"https://api.xmpush.xiaomi.com/v1/trace/message/status">>,
    Headers = gen_get_headers(AppSecret),
    Result = eutil:http_get(URL, Headers, Query, [{pool, xiaomi}]),
    case maps:get(<<"code">>, Result) of
        ?SUCCESS ->
            {ok, maps:get(<<"data">>, maps:get(<<"data">>, Result))};
        ErrCode ->
            {error, ErrCode}
    end.

%%获取失效的regId列表
fetch_invalid_regids() ->
    AppSecret = get_conf_app_secret(),
    fetch_invalid_regids(AppSecret).

fetch_invalid_regids(AppSecret) ->
    URL = <<"https://feedback.xmpush.xiaomi.com/v1/feedback/fetch_invalid_regids">>,
    Headers = gen_get_headers(AppSecret),
    Result = eutil:http_get(URL, Headers, [], [{pool, xiaomi}]),
    case maps:get(<<"code">>, Result) of
        ?SUCCESS ->
            {ok, maps:get(<<"list">>, maps:get(<<"data">>, Result))};
        ErrCode ->
            {error, ErrCode}
    end.

%%获取一个应用的某个用户目前订阅的所有Topic
all_topic(RegId) ->
    AppSecret = get_conf_app_secret(),
    all_topic(AppSecret, RegId).

all_topic(AppSecret, RegId) when is_list(RegId) or is_binary(RegId) ->
    all_topic(AppSecret, #{<<"registration_id">> => eutil:to_binary(RegId)});
all_topic(AppSecret, Query) when is_map(Query) ->
    URL = <<"https://api.xmpush.xiaomi.com/v1/topic/all">>,
    Headers = gen_get_headers(AppSecret),
    Result = eutil:http_get(URL, Headers, Query, [{pool, xiaomi}]),
    case maps:get(<<"code">>, Result) of
        ?SUCCESS ->
            {ok, maps:get(<<"list">>, maps:get(<<"data">>, Result))};
        ErrCode ->
            {error, ErrCode}
    end.


send(AppSecret, URL, MsgMaps) ->
    Headers = gen_headers(AppSecret),
    send_for_headers(Headers, URL, MsgMaps).

send_for_headers(Headers, URL, MsgMaps) ->
    Result = eutil:http_post(URL, Headers, MsgMaps, [{pool, xiaomi}]),
    #{<<"code">>:=Code} = Result,
    case Code of
        ?SUCCESS ->
            case maps:get(<<"data">>, Result, undefined) of
                undefined -> ok;
                Data -> {ok, maps:get(<<"id">>, Data)}
            end;
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


general_notification(AppSecret, PkgName, RegIds, Title, Desc) ->
    regid_notification(AppSecret, PkgName, RegIds, Title, Desc).

general_app_msg(AppSecret, PkgName, RegIds, Msg) ->
    regid_pass_through(AppSecret, PkgName, RegIds, Msg).
