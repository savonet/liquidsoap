# Part B: notes on the OCaml implementation

Not normative. How the behaviour of Part A is built in OCaml today.

## Mapping

| Spec term               | Construct                                                                                                                                                                                                                                                        |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| operator = role + kind  | An object class inheriting a virtual kind class (`output_base`, `input_base`) and a virtual role class (`caller`, `listener`); four concrete classes. Roles reach the kind through virtual methods (`apply_on_connect`, `should_stop`, `log`, the lock methods). |
| shared operator state   | Virtual class `base`: stop flag, instance number, the three callback lists.                                                                                                                                                                                      |
| role contract           | Virtual classes `networking_agent`, `input_networking_agent`, `output_networking_agent`: `connect`, `disconnect`, `is_connected`, `get_socket`/`get_sockets`, `client_error`.                                                                                    |
| readiness loop          | Module `Poll`: a `Srt.Poll.t` and a `Hashtbl`, both created at module initialisation, and a `Scheduler.Async` task.                                                                                                                                              |
| connect task            | A `Scheduler.Async` task. A task function returns the delay before its next run: `>= 0.` runs again after that many seconds, `< 0.` sleeps until `wake_up`. `stop` does not interrupt a running call.                                                            |
| source lock             | `Mutex_utils` state with `atomic_lock` (short) and `mutable_lock` (mutating), exposed as source methods.                                                                                                                                                         |
| one-time initialisation | `Lazy.Mutexed`, forced in `base`'s class initialiser.                                                                                                                                                                                                            |
| shutdown registry       | A `Weak.Make` hash set keyed by the instance number.                                                                                                                                                                                                             |
| callbacks               | `Callbacks.t`: an id-tagged queue; `register` returns the release function.                                                                                                                                                                                      |
| byte queue              | `Strings.Mutable.t`.                                                                                                                                                                                                                                             |
| script socket value     | `Value.MkCustom` over `Srt.socket`, with methods attached by `Lang.meth`.                                                                                                                                                                                        |

## Exceptions as control flow

`Done` aborts a read when stopping and an accept when stopping. `Not_connected`
is raised by the caller's socket getter. `End_of_file` ends a decoder.
`builtins_srt.ml` declares its own `Done` and `Not_connected`, unused.

## Binding

The libsrt binding is generated with ctypes. Its calls run with the runtime
lock released. The admission predicate is a dynamic function pointer that
registers libsrt's thread with the runtime and takes the runtime lock for the
call. Log messages reach the handler on an OCaml thread the binding starts,
which waits in a C stub for the next message.

## Effects of the structure

- Multiple inheritance spreads one operator's behaviour over the kind class,
  the role class and the concrete class, which overrides `client_error`
  and `get_sockets`. The input caller's `get_sockets` catches `Not_found`
  where its getter raises `Not_connected`.
- Initialisers run in inheritance order; the kind's `on_connect` and
  `on_disconnect` handlers are registered there, which is what places them
  before script callbacks.
