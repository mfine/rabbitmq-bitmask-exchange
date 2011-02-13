# RabbitMQ Bitmask Exchanges

These are enhanced message routing exchange plugins for RabbitMQ: they
alter normal direct and topic exchange routing by treating the routing
key as a bit set and the binding key as a bit mask. Only destinations
that mask the routing key's bits will receive messages:

    RoutingKey & BindingKey == RoutingKey

The general idea is that a binding key represents supported bits and a
routing key represents required bits - the masking check above
determines whether a binding key can support a routing key.

## Direct Bitmask Exchange

Similar to an AMQP direct exchange, except that destinations are
matched by the above masking checks and then randomly chosen: all
destinations with binding keys that support a message's routing key
are determined; then one destination is randomly chosen and delivered
to.

To use, install the plugin and choose 'direct-bitmask' for exchange
type.

## Topic Bitmask Exchange

Similar to an AMQP topic exchange, except that destinations are
matched by the above masking checks: all destinations with binding
keys that support a message's routing key will be delivered to.

To use, install the plugin and choose 'topic-bitmask' for exchange
type.


