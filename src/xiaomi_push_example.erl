-module(xiaomi_push_example).
-compile(export_all).

-define(REGID, "go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=").
-define(TITLE, "Title标题").
-define(DESC, "DESC内容").
-define(PAYLOAD, <<"Payload">>).

regid_message() ->
    MsgMaps = #{<<"description">> => <<68,69,83,67,229,134,133,229,174,185>>,
                <<"extra.notify_effect">> => 1,
                <<"notify_type">> => -1,
                <<"pass_through">> => 0,
                <<"registration_id">> => <<"go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=">>,
                <<"restricted_package_name">> => <<"com.ligo.hisir20">>,
                <<"title">> => <<84,105,116,108,101,230,160,135,233,162,152>>},
    xiaomi_push:regid_message(MsgMaps).

regid_notification() ->
    xiaomi_push:regid_notification(?REGID, ?TITLE, ?DESC).

regid_pass_through() ->
    Content = jiffy:encode(#{<<"body">> => <<"Body">>}),
    Payload = jiffy:encode(#{<<"message_type">> => <<"Common">>, <<"Type">>  => 0,
                             <<"from">> => 51, <<"content">> => Content
                            }),
    xiaomi_push:regid_pass_through(?REGID, Payload).

all_message() ->
    MsgMaps =#{<<"description">> => <<68,69,83,67,229,134,133,229,174,185>>,
               <<"extra.notify_effect">> => 1,
               <<"notify_type">> => -1,
               <<"pass_through">> => 0,
               <<"restricted_package_name">> => <<"com.ligo.hisir20">>,
               <<"title">> => <<84,105,116,108,101,230,160,135,233,162,152>>},
    xiaomi_push:all_message(MsgMaps).

all_notificaiton() ->
    xiaomi_push:all_notificaiton(?TITLE, ?DESC).

all_pass_through() ->
    xiaomi_push:all_pass_through(<<"Hello World">>).

topic_message() ->
    MsgMaps = #{<<"description">> => <<68,69,83,67,229,134,133,229,174,185>>,
                <<"extra.notify_effect">> => 1,
                <<"notify_type">> => -1,
                <<"pass_through">> => 0,
                <<"restricted_package_name">> => <<"com.ligo.hisir20">>,
                <<"title">> => <<84,105,116,108,101,230,160,135,233,162,152>>,
                <<"topic">> => <<"topic1">>},
    xiaomi_push:topic_message(MsgMaps).

topic_notification() ->
    xiaomi_push:topic_notification("topic1", ?TITLE, ?DESC).

topic_pass_through() ->
    xiaomi_push:topic_pass_through("topic1", ?PAYLOAD).

regid_messages() ->
    {ok, PkgName} = application:get_env(xiaomi_push, pkg_name),
    Message1 = #{<<"restricted_package_name">> => list_to_binary(PkgName),
                 <<"pass_through">> => 0,
                 <<"notify_type">> => -1,
                 <<"title">> => unicode:characters_to_binary("title"),
                 <<"description">> => unicode:characters_to_binary("description")
                },
    Messages = [#{<<"target">> => list_to_binary(?REGID), <<"message">> => Message1}],
    xiaomi_push:regid_messages(Messages).

regid_subscribe() ->
    xiaomi_push:regid_subscribe(?REGID, "topic1").

regid_unsubscribe() ->
    xiaomi_push:regid_unsubscribe(?REGID, "topic1").

message_status() ->
    xiaomi_push:message_status("20161016", "20161018").

trace_status() ->
	xiaomi_push:trace_status("slm43b99476776547639jH").

fetch_invalid_regids() ->
    xiaomi_push:fetch_invalid_regids().

all_topic() ->
    xiaomi_push:all_topic(?REGID).

all_topic2() ->
    {ok, AppSecret} = application:get_env(xiaomi_push, app_secret),
    {ok, PkgName} = application:get_env(xiaomi_push, pkg_name),
    Query = #{<<"restricted_package_name">> => list_to_binary(PkgName),
              <<"registration_id">> => list_to_binary(?REGID)
             },
    xiaomi_push:all_topic(AppSecret, Query).
