# xiaomi_push
> 小米xiaomi推送push server sdk for Erlang    
> 集成版本：https://github.com/dcy/epush    
> 使用例子：[/src/xiaomi_push_example.erl](/src/xiaomi_push_example.erl)

## Authorization
* gen_authorization(AppSecret)
* gen_headers(AppSecret)

## regid_message 向某个regid或一组regid列表推送某条消息
* regid_message(MsgMaps) ->
* regid_message(AppSecret, MsgMaps) ->
```erlang
MsgMaps = #{<<"description">> => <<68,69,83,67,229,134,133,229,174,185>>,
            <<"extra.notify_effect">> => 1,
            <<"notify_type">> => -1,
            <<"pass_through">> => 0,
            <<"registration_id">> => <<"go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=">>,
            <<"restricted_package_name">> => <<"xiaomi_pkg_name">>,
            <<"title">> => <<84,105,116,108,101,230,160,135,233,162,152>>},
xiaomi_push:regid_message(MsgMaps).
```

## regid_notification 向某个regid或一组regid发送通知栏消息
* regid_notification(RegIds, Title, Desc) ->
* regid_notification(AppSecret, PkgName, RegIds, Title, Desc) ->
```erlang
xiaomi_push:regid_notification(?REGID, ?TITLE, ?DESC).
```

## regid_pass_through 向某个regid或一组regid发送透传消息
* regid_pass_through(RegIds, Payload) ->
* regid_pass_through(AppSecret, PkgName, RegIds, Payload) ->
```erlang
Payload = jiffy:encode(#{<<"body">> => <<"Body">>}),
xiaomi_push:regid_pass_through(?REGID, Payload).
```

## regid_messages 向不同的regid发送不同的消息
* regid_messages(Messages) ->
* regid_messages(AppSecret, Messages) ->
```erlang
{ok, PkgName} = application:get_env(xiaomi_push, pkg_name),
Message1 = #{<<"restricted_package_name">> => list_to_binary(PkgName),
             <<"pass_through">> => 0,
             <<"notify_type">> => -1,
             <<"title">> => unicode:characters_to_binary("title"),
             <<"description">> => unicode:characters_to_binary("description")
            },
Messages = [#{<<"target">> => list_to_binary(?REGID), <<"message">> => Message1}],
xiaomi_push:regid_messages(Messages).
```

## all_message 向所有设备推送某条消息
* all_message(MsgMaps) ->
* all_message(AppSecret, MsgMaps) ->
```erlang
MsgMaps =#{<<"description">> => <<68,69,83,67,229,134,133,229,174,185>>,
           <<"extra.notify_effect">> => 1,
           <<"notify_type">> => -1,
           <<"pass_through">> => 0,
           <<"restricted_package_name">> => <<"xiaomi_pkg_name">>,
           <<"title">> => <<84,105,116,108,101,230,160,135,233,162,152>>},
xiaomi_push:all_message(MsgMaps).
```

## all_notificaiton 向所有设备发送通知栏消息
* all_notificaiton(Title, Desc) ->
* all_notificaiton(AppSecret, PkgName, Title, Desc) ->
```erlang
xiaomi_push:all_notificaiton(?TITLE, ?DESC).
```

## all_pass_through 向所有设备发送透传消息
* all_pass_through(Payload) ->
* all_pass_through(AppSecret, PkgName, Payload) ->
```erlang
xiaomi_push:all_pass_through(<<"Hello World">>).
```

## topic_message 向某个topic推送某条消息
* topic_message(MsgMaps) ->
* topic_message(AppSecret, MsgMaps) ->
```erlang
MsgMaps = #{<<"description">> => <<68,69,83,67,229,134,133,229,174,185>>,
            <<"extra.notify_effect">> => 1,
            <<"notify_type">> => -1,
            <<"pass_through">> => 0,
            <<"restricted_package_name">> => <<"xiaomi_pkg_name">>,
            <<"title">> => <<84,105,116,108,101,230,160,135,233,162,152>>,
            <<"topic">> => <<"topic1">>},
xiaomi_push:topic_message(MsgMaps).
```

## topic_notification 向某个topic推送通知栏消息
* topic_notification(Topic, Title, Desc) ->
* topic_notification(AppSecret, PkgName, Topic, Title, Desc) ->
```erlang
xiaomi_push:topic_notification("topic1", ?TITLE, ?DESC).
```
## topic_pass_through 向某个topic推送透传消息
* topic_pass_through(Topic, Payload) ->
* topic_pass_through(AppSecret, PkgName, Topic, Payload) ->
```erlang
xiaomi_push:topic_pass_through("topic1", ?PAYLOAD).
```

## regid_subscribe  订阅RegId的标签
* regid_subscribe(RegId, Topic) ->
* regid_subscribe(AppSecret, PkgName, RegId, Topic) ->
```erlang
xiaomi_push:regid_subscribe(?REGID, "topic1").
```
## regid_unsubscribe 取消订阅RegId的标签
* regid_unsubscribe(RegId, Topic) ->
* regid_unsubscribe(AppSecret, PkgName, RegId, Topic) ->
```erlang
xiaomi_push:regid_unsubscribe(?REGID, "topic1").
```

## message_status 获取消息的统计数据
* message_status(StartDate, EndDate) ->
* message_status(AppSecret, PkgName, StartDate, EndDate) ->
```erlang
xiaomi_push:message_status("20161016", "20161018").
```

## trace_status 追踪消息状态
* trace_status(MsgId) ->
* trace_status(AppSecret, PkgName, MsgId) ->
```erlang
xiaomi_push:trace_status("slm43b99476776547639jH").
```

## fetch_invalid_regids 获取失效的RegId列表
* fetch_invalid_regids() ->
* fetch_invalid_regids(AppSecret) ->
```erlang
xiaomi_push:fetch_invalid_regids().
```

## all_topic 获取一个应用的某个用户目前订阅的所有Topic
* all_topic(RegId) ->
* all_topic(AppSecret, RegId) when is_list(RegId) or is_binary(RegId) ->
```erlang
xiaomi_push:all_topic(?REGID).
```
* all_topic(AppSecret, Query) when is_map(Query) ->
```erlang
{ok, AppSecret} = application:get_env(xiaomi_push, app_secret),
{ok, PkgName} = application:get_env(xiaomi_push, pkg_name),
Query = #{<<"restricted_package_name">> => list_to_binary(PkgName),
          <<"registration_id">> => list_to_binary(?REGID)
         },
xiaomi_push:all_topic(AppSecret, Query).
```
