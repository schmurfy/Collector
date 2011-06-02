
# The Story

Collector is my first erlang project and in the same time an experiment at replacing
collectd in my monitoring architecture by something which does less and works better
with other applications to do the work it does not do.

# Objectives

- keep the application simple by having a nice set of api for external applications
    to communicate with so they can do anything they want with the data and how they want.
- add support to parse incoming data from other sources.
- maybe add a theshold handler to generate alerts if a value goes out of range.



# Short term Goals

These may change but currently they are:

- [Done] being able to receive packets from a collectd deamon
- [Done] all the event handlers can be loaded or unloaded as wanted via the config file
- send events to other applications to handle them.
- write and event handler which will write the values in rrd files.
- zeromq as transport module (I tried erlzmq2 but it just feels like it will be a major
    pain in the ass to deploy on production system with its recompilation of zeromq pulled
    from github).

Handlers:

- presence module: when values are received for an host the "active" timer is reset,
    if the timer reach 0 the machine is considered missing and an event is fired. [Done]
  
- tcp module: allow clients to connect and forward the events to all connected, maybe add
    a simple subscription system too.

- rrd module: save values in rrd files, that is borderline with my goals but I don't like
    the idea of requiring an external application connected to collector just to save
    the values in rrd files so I suppose it fits in the core.


# How it works

I split the tasks in different parts:

- receivers : parse an outside protocol and convert their content to an internal format
- event handlers : they are the main part, I use a set of gen_event modules to do the
    real work, they receives the messages from the receivers and can emit some on their
    own (like notifications).
- output : currently I am starting with tcp + bert to build a simple protocol so that
    external applications can register to receive events and maybe send new ones.



# Want to help ?

Since I am new to erlang there are some area where I have troubles, like how to properly
build a release, how to handle dependencies nicely. It may be I did not found the right
documentations but it to me like deployment and dev tools are nearly non existent at least
that is my feeling as a ruby developer.

Aside of that any contribution is welcome !

To get working you need to have a working erlang vm and rebar in your path,
hopefully you should be able to compile the project with:

  ``` bash
  make
  ```

It should download the dependencies and build them as well as the core.
For now you can run it with:

  ``` bash
  cp collector.config.base collector.config
  make run
  ```

You should be able to build a release with:

  ``` bash
  rebar generate
  ```

But for now it compiles fine but crash on start :(
