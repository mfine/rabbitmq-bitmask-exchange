{application, rabbit_bitmask_exchange,
 [{description, "RabbitMQ bitmask exchange"},
  {vsn, "0.0.0"},
  {modules, [
    rabbit_exchange_type_direct_bitmask,
    rabbit_exchange_type_topic_bitmask
  ]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib, rabbit]}]}.
