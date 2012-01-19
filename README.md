Couch Events
============

An application for distributing event notifications in a pub/sub manner. This
is just a quick sketch of an idea for refactoring some of the CouchDB
internals to lessen the amount of coupling in certain features.

Basic Design
------------

Code that wants to subscribe to an event channel would just do:

    couch_events_server:subscribe(ChannelName)

The channel name can be any arbitrary term but should match one of the event
sources if the subscriber ever wants to receive notifications. The channel
name list/discovery is out-of-band from normal operation (ie, documentation).

Code that wants to generate an event notice simple does:

    -include_lib("couch_events/include/couch_events.hrl").

    ...

    ?SEND_EVENT(ChannelName, Event)

Where ChannelName and Event are both arbitrary terms but should be documented
somewhere for client code. Event will most likely end up being a JSON term
but that's only by convention for now.

Performance Concerns
--------------------

If we're going to use this sort of event system for lots and lots of events as
well as expect it to handle a significant number of clients then we need to
make sure that the core pieces are as efficient as possible. In general this
means a couple things.

1. Don't route all event messages through a central distributor
2. Minimize the message passing when there aren't any subscribers so that
   we can have lots of event types for debugging and the like that
   don't place undo load on the system when no one is listening
3. Channels should be runtime changeable and not a compile time flag

To this end, the basic internal architecture is made of three basic parts.
The first part is couch\_events\_server which manages instances of
couch\_events\_channel which in turn are repsonsible for distributing
event messages to clients.

At initialization, couch\_events\_server has no channels. When a client
requests to subscribe to a channel that doesn't exist, a message is routed
through couch\_events\_server that ends up creating the channel.

couch\_events\_channel keeps track of its own subscribers and when it finds
that it has no more it exits cleanly which couch\_events\_server notices
so that it removes the channel from operation.

Subscribers can either unsubscribe directly or just exit to remove their
subscription status. Subscribers can subscribe to multiple channels. Each
subscription is identified by a Ref so it can figure out where each event
message originated. Event messagse are of the form '{event, Ref, Event}'
where Ref was returned as '{ok, Ref}' from couch\_events\_server:subscribe.

Code that wants to notify channels of events can use the '?SEND\_EVENT' macro
defined in couch\_events.hrl. This macro checks first if the channel is alive
and only sends a message when it is. In the case when no channel exists this
is a single functionc all. In the case where a channel does exist its a
function call and a message pass.

The secret sauce of all this is in couch\_events\_kv.erl which is a dynamic
K/V module based on Bob Ippolitto's mochiglobal. Basically it dynamically
recompiles a module everytime a channel is created or destroyed (which is
expected to be rather seldom). The module is a single function that relies
on pattern matching to look up the channel's pid before sending the message.
This is much simpler than it sounds.

