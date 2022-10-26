Playing files is the most common way to build an audio stream.
In liquidsoap, files are accessed through [requests](requests.html),
which combine the retrieval of a possibly remote file, and its
decoding.

Liquidsoap provides several operators for playing requests:
`single`, `playlist` and `playlist.safe`,
`request.dynamic.list`, `request.queue` and `request.equeue`.
In a few cases (`single` with a local file,
or `playlist.safe`) a request operator will know
that it can always get a ready request instantaneously.
It will then be [infallible](sources.html).
Otherwise, it will have a queue of requests ready
to be played (local files with a valid content), and will
feed this queue in the background.
This process is described here.

Common parameters
-----------------
Queued request sources maintain an *estimated remaining time*,
and trigger a new request resolution when this remaining time
goes below their `length` parameter.

The estimation is based on the duration of files prepared in the queue,
and the estimated remaining time in the currently playing file.
Precise file durations being expensive to compute, they are not
forced: if a duration is provided in the metadata it shall be used,
otherwise the `default_length` is assumed.

For example, with the default 10 seconds of wanted queue length,
the operator will only prepare a new file 10 seconds before
the end of the current one.

Up to liquidsoap 0.9.1, the estimated remaining time
in the current track was not taken into account.
With this behavior, each request-based source would keep at least
one song in queue, which was sometimes inconvenient.
This behavior can be restored by passing `conservative=true`,
which is useful in some cases:
it helps to ensure that a song will be ready in case of skip;
generally, it prepares things more in advance, which is good when
resolution is long (*e.g.*, heavily loaded server, remote files).

Request.dynamic
---------------
This source takes a custom function for creating its new requests.
This function, of type `()->request`,
can for example call an external program.

To create the request, the function will have
to use the `request.create` function which has type
`(string,?indicators:[string])`.
The first string is the initial URI of the request,
which is resolved to get an audio file.
The second argument can be used to directly specify the first row of URIs
(see the page about [requests](requests.html) for more details),
in which case the initial URI is just here for naming,
and the resolving process will try your list of indicators one by one
until a valid audio file is obtained.

An example that takes the output of an external script as an URI
to create a new request can be:
```liquidsoap
def my_request_function() =
  # Get the first line of my external process
  result =
    list.hd(default="", process.read.lines("my_script my_params"))
  # Create and return a request using this result
  [request.create(result)]
end

# Create the source
s = request.dynamic.list(my_request_function)
```

Queues
------
Liquidsoap features two sources which provide request queues that
can be directly manipulated by the user, via the server interface:
`request.queue` and `request.equeue`.
The former is a queued source where you can only push new requests,
while the later can be edited.

Both operators actually deal with two queues: *primary* and *secondary* queues.
The secondary queue is user-controlled.
The primary queue is the one that all queued request sources have,
its behavior is the same as described above, and it cannot be changed
in any way by the user.
Requests added to the secondary queue sit there until
the feeding process gets them and attempts to prepare them
and put them in the primary queue.
You can set how many requests will be in that primary queue
by tweaking the common parameters of all queued request sources.

The two sources are controlled via the [command server](advanced.html).
They both feature commands for looking up the queues,
queuing new requests, and the `equeue` operator also allows
removal and exchange of requests in the secondary queue.
