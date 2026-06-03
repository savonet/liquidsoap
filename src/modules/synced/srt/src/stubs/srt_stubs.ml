open Ctypes
open Posix_socket

type socket = int

let const_char_ptr = typedef (ptr char) "const char*"
let const_string = typedef string "const char*"
let const_sockaddr = typedef (ptr sockaddr_t) "const struct sockaddr*"

module ListenCallback =
  (val Foreign.dynamic_funptr ~thread_registration:true ~runtime_lock:true
         (ptr void @-> int @-> int @-> const_sockaddr @-> const_char_ptr
        @-> returning int))

module Def (F : Cstubs.FOREIGN) = struct
  include Srt_types
  include Srt_types.Def (Srt_generated_types)
  open F

  let startup = foreign "srt_startup" (void @-> returning void)
  let cleanup = foreign "srt_cleanup" (void @-> returning void)
  let getlasterror = foreign "srt_getlasterror" (ptr int @-> returning errno)

  let getlasterror_str =
    foreign "srt_getlasterror_str" (void @-> returning const_string)

  let clearlasterror = foreign "srt_clearlasterror" (void @-> returning void)
  let create_socket = foreign "srt_create_socket" (void @-> returning int)

  let bind =
    foreign "srt_bind" (int @-> ptr sockaddr_t @-> int @-> returning int)

  let strlen = foreign "strlen" (ptr char @-> returning int)
  let listen = foreign "srt_listen" (int @-> int @-> returning int)

  let listen_callback =
    foreign "srt_listen_callback"
      (int @-> ListenCallback.t @-> ptr void @-> returning int)

  let accept =
    foreign "srt_accept" (int @-> ptr sockaddr_t @-> ptr int @-> returning int)

  let connect =
    foreign "srt_connect" (int @-> ptr sockaddr_t @-> int @-> returning int)

  let rendez_vous =
    foreign "srt_rendezvous"
      (int @-> ptr sockaddr_t @-> int @-> ptr sockaddr_t @-> int
     @-> returning int)

  let send = foreign "srt_send" (int @-> ptr char @-> int @-> returning int)
  let recv = foreign "srt_recv" (int @-> ptr char @-> int @-> returning int)

  let recvmsg =
    foreign "srt_recvmsg" (int @-> ptr char @-> int @-> returning int)

  let sendmsg =
    foreign "srt_sendmsg"
      (int @-> ptr char @-> int @-> int @-> bool @-> returning int)

  let setsockflag =
    foreign "srt_setsockflag"
      (int @-> socket_opt @-> ptr void @-> int @-> returning int)

  let getsockflag =
    foreign "srt_getsockflag"
      (int @-> socket_opt @-> ptr void @-> ptr int @-> returning int)

  let getsockstate = foreign "srt_getsockstate" (int @-> returning socket_status)
  let setloglevel = foreign "srt_setloglevel" (int @-> returning void)
  let close = foreign "srt_close" (int @-> returning int)
  let epoll_create = foreign "srt_epoll_create" (void @-> returning int)

  let epoll_add_usock =
    foreign "srt_epoll_add_usock" (int @-> int @-> ptr int @-> returning int)

  let epoll_remove_usock =
    foreign "srt_epoll_remove_usock" (int @-> int @-> returning int)

  let epoll_update_usock =
    foreign "srt_epoll_update_usock" (int @-> int @-> ptr int @-> returning int)

  let epoll_uwait =
    foreign "srt_epoll_uwait"
      (int @-> ptr PollEvent.t @-> int @-> int64_t @-> returning int)

  let epoll_wait =
    foreign "srt_epoll_wait"
      (int @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> int64_t
     @-> ptr void @-> ptr void @-> ptr void @-> ptr void @-> returning int)

  let epoll_release = foreign "srt_epoll_release" (int @-> returning int)

  let bstats =
    foreign "srt_bstats" (int @-> ptr CBytePerfMon.t @-> int @-> returning int)

  let bistats =
    foreign "srt_bistats"
      (int @-> ptr CBytePerfMon.t @-> int @-> int @-> returning int)
end
