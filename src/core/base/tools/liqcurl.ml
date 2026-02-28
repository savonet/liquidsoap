(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

module Http = Liq_http

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let string_of_curl_code = function
  | Curl.CURLE_OK -> "Ok"
  | Curl.CURLE_UNSUPPORTED_PROTOCOL -> "Unsupported_protocol"
  | Curl.CURLE_FAILED_INIT -> "Failed_init"
  | Curl.CURLE_URL_MALFORMAT -> "Url_malformat"
  | Curl.CURLE_URL_MALFORMAT_USER -> "Url_malformat_user"
  | Curl.CURLE_COULDNT_RESOLVE_PROXY -> "Couldnt_resolve_proxy"
  | Curl.CURLE_COULDNT_RESOLVE_HOST -> "Couldnt_resolve_host"
  | Curl.CURLE_COULDNT_CONNECT -> "Couldnt_connect"
  | Curl.CURLE_FTP_WEIRD_SERVER_REPLY -> "Ftp_weird_server_reply"
  | Curl.CURLE_FTP_ACCESS_DENIED -> "Ftp_access_denied"
  | Curl.CURLE_FTP_USER_PASSWORD_INCORRECT -> "Ftp_user_password_incorrect"
  | Curl.CURLE_FTP_WEIRD_PASS_REPLY -> "Ftp_weird_pass_reply"
  | Curl.CURLE_FTP_WEIRD_USER_REPLY -> "Ftp_weird_user_reply"
  | Curl.CURLE_FTP_WEIRD_PASV_REPLY -> "Ftp_weird_pasv_reply"
  | Curl.CURLE_FTP_WEIRD_227_FORMAT -> "Ftp_weird_227_format"
  | Curl.CURLE_FTP_CANT_GET_HOST -> "Ftp_cant_get_host"
  | Curl.CURLE_FTP_CANT_RECONNECT -> "Ftp_cant_reconnect"
  | Curl.CURLE_FTP_COULDNT_SET_BINARY -> "Ftp_couldnt_set_binary"
  | Curl.CURLE_PARTIAL_FILE -> "Partial_file"
  | Curl.CURLE_FTP_COULDNT_RETR_FILE -> "Ftp_couldnt_retr_file"
  | Curl.CURLE_FTP_WRITE_ERROR -> "Ftp_write_error"
  | Curl.CURLE_FTP_QUOTE_ERROR -> "Ftp_quote_error"
  | Curl.CURLE_HTTP_NOT_FOUND -> "Http_not_found"
  | Curl.CURLE_WRITE_ERROR -> "Write_error"
  | Curl.CURLE_MALFORMAT_USER -> "Malformat_user"
  | Curl.CURLE_FTP_COULDNT_STOR_FILE -> "Ftp_couldnt_stor_file"
  | Curl.CURLE_READ_ERROR -> "Read_error"
  | Curl.CURLE_OUT_OF_MEMORY -> "Out_of_memory"
  | Curl.CURLE_OPERATION_TIMEOUTED -> "Operation_timeouted"
  | Curl.CURLE_FTP_COULDNT_SET_ASCII -> "Ftp_couldnt_set_ascii"
  | Curl.CURLE_FTP_PORT_FAILED -> "Ftp_port_failed"
  | Curl.CURLE_FTP_COULDNT_USE_REST -> "Ftp_couldnt_use_rest"
  | Curl.CURLE_FTP_COULDNT_GET_SIZE -> "Ftp_couldnt_get_size"
  | Curl.CURLE_HTTP_RANGE_ERROR -> "Http_range_error"
  | Curl.CURLE_HTTP_POST_ERROR -> "Http_post_error"
  | Curl.CURLE_SSL_CONNECT_ERROR -> "Ssl_connect_error"
  | Curl.CURLE_FTP_BAD_DOWNLOAD_RESUME -> "Ftp_bad_download_resume"
  | Curl.CURLE_FILE_COULDNT_READ_FILE -> "File_couldnt_read_file"
  | Curl.CURLE_LDAP_CANNOT_BIND -> "Ldap_cannot_bind"
  | Curl.CURLE_LDAP_SEARCH_FAILED -> "Ldap_search_failed"
  | Curl.CURLE_LIBRARY_NOT_FOUND -> "Library_not_found"
  | Curl.CURLE_FUNCTION_NOT_FOUND -> "Function_not_found"
  | Curl.CURLE_ABORTED_BY_CALLBACK -> "Aborted_by_callback"
  | Curl.CURLE_BAD_FUNCTION_ARGUMENT -> "Bad_function_argument"
  | Curl.CURLE_BAD_CALLING_ORDER -> "Bad_calling_order"
  | Curl.CURLE_HTTP_PORT_FAILED -> "Http_port_failed"
  | Curl.CURLE_BAD_PASSWORD_ENTERED -> "Bad_password_entered"
  | Curl.CURLE_TOO_MANY_REDIRECTS -> "Too_many_redirects"
  | Curl.CURLE_UNKNOWN_TELNET_OPTION -> "Unknown_telnet_option"
  | Curl.CURLE_TELNET_OPTION_SYNTAX -> "Telnet_option_syntax"
  | Curl.CURLE_OBSOLETE -> "Obsolete"
  | Curl.CURLE_SSL_PEER_CERTIFICATE -> "Ssl_peer_certificate"
  | Curl.CURLE_GOT_NOTHING -> "Got_nothing"
  | Curl.CURLE_SSL_ENGINE_NOTFOUND -> "Ssl_engine_notfound"
  | Curl.CURLE_SSL_ENGINE_SETFAILED -> "Ssl_engine_setfailed"
  | Curl.CURLE_SEND_ERROR -> "Send_error"
  | Curl.CURLE_RECV_ERROR -> "Recv_error"
  | Curl.CURLE_SHARE_IN_USE -> "Share_in_use"
  | Curl.CURLE_SSL_CERTPROBLEM -> "Ssl_certproblem"
  | Curl.CURLE_SSL_CIPHER -> "Ssl_cipher"
  | Curl.CURLE_SSL_CACERT -> "Ssl_cacert"
  | Curl.CURLE_BAD_CONTENT_ENCODING -> "Bad_content_encoding"
  | Curl.CURLE_LDAP_INVALID_URL -> "Ldap_invalid_url"
  | Curl.CURLE_FILESIZE_EXCEEDED -> "Filesize_exceeded"
  | Curl.CURLE_USE_SSL_FAILED -> "Use_ssl_failed"
  | Curl.CURLE_SEND_FAIL_REWIND -> "Send_fail_rewind"
  | Curl.CURLE_SSL_ENGINE_INITFAILED -> "Ssl_engine_initfailed"
  | Curl.CURLE_LOGIN_DENIED -> "Login_denied"
  | Curl.CURLE_TFTP_NOTFOUND -> "Tftp_notfound"
  | Curl.CURLE_TFTP_PERM -> "Tftp_perm"
  | Curl.CURLE_REMOTE_DISK_FULL -> "Remote_disk_full"
  | Curl.CURLE_TFTP_ILLEGAL -> "Tftp_illegal"
  | Curl.CURLE_TFTP_UNKNOWNID -> "Tftp_unknownid"
  | Curl.CURLE_REMOTE_FILE_EXISTS -> "Remote_file_exists"
  | Curl.CURLE_TFTP_NOSUCHUSER -> "Tftp_nosuchuser"
  | Curl.CURLE_CONV_FAILED -> "Conv_failed"
  | Curl.CURLE_CONV_REQD -> "Conv_reqd"
  | Curl.CURLE_SSL_CACERT_BADFILE -> "Ssl_cacert_badfile"
  | Curl.CURLE_REMOTE_FILE_NOT_FOUND -> "Remote_file_not_found"
  | Curl.CURLE_SSH -> "Ssh"
  | Curl.CURLE_SSL_SHUTDOWN_FAILED -> "Ssl_shutdown_failed"
  | Curl.CURLE_AGAIN -> "Again"

let () =
  Printexc.register_printer (function
    | Curl.CurlException (curl_code, code, msg) ->
        Some
          (Printf.sprintf "CurlException(%s, %i, %s)"
             (string_of_curl_code curl_code)
             code msg)
    | _ -> None)

let fail ~pos message = Lang.raise_error ~pos ~message "http"
let interrupt = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"libcurl shutdown" (fun () ->
      Atomic.set interrupt true)

let mk_read fn len =
  if Atomic.get interrupt then Curl.Abort
  else (try Proceed (fn len) with _ -> Curl.Abort)

let parse_http_answer ~pos s =
  let f v c s = (v, c, s) in
  try Scanf.sscanf s "HTTP/%s %i %[^\r^\n]" f with
    | Scanf.Scan_failure s -> fail ~pos s
    | _ -> fail ~pos "Unknown error"

let should_stop _ _ _ _ = Atomic.get interrupt

let rec http_request ?headers ?http_version ~follow_redirect ~timeout ~url
    ~request ~on_body_data ~pos () =
  let connection = new Curl.handle in
  try
    connection#set_url url;
    connection#set_useragent Http.user_agent;
    connection#set_httpversion
      (match http_version with
        | None -> Curl.HTTP_VERSION_NONE
        | Some "1.0" -> Curl.HTTP_VERSION_1_0
        | Some "1.1" -> Curl.HTTP_VERSION_1_1
        | Some "2.0" -> Curl.HTTP_VERSION_2
        | Some v -> fail ~pos (Printf.sprintf "Unsupported http version %s" v));
    Option.iter connection#set_timeoutms timeout;
    (match request with
      | `Get -> connection#set_httpget true
      | `Post (len, get_data) ->
          connection#set_post true;
          (match len with
            | Some len -> connection#set_postfieldsizelarge len
            | None -> connection#set_httpheader ["Transfer-Encoding: chunked"]);
          connection#set_readfunction2 (mk_read get_data)
      | `Put (len, get_data) ->
          connection#set_put true;
          (match len with
            | Some len -> connection#set_postfieldsizelarge len
            | None -> connection#set_httpheader ["Transfer-Encoding: chunked"]);
          connection#set_readfunction2 (mk_read get_data)
      | `Head -> connection#set_nobody true
      | `Delete -> connection#set_customrequest "DELETE");
    ignore
      (Option.map
         (fun headers ->
           connection#set_httpheader
             (List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) headers))
         headers);
    let accepted = Atomic.make false in
    let response_headers = Buffer.create Utils.buflen in
    connection#set_headerfunction (fun s ->
        if not (Atomic.get accepted) then (
          let code = connection#get_httpcode in
          Atomic.set accepted (code < 300 || 400 <= code));
        Buffer.add_string response_headers s;
        String.length s);
    connection#set_xferinfofunction should_stop;
    connection#set_writefunction (fun s ->
        if Atomic.get accepted then on_body_data s;
        String.length s);
    connection#set_noprogress false;
    connection#perform;
    match connection#get_redirecturl with
      | url when url <> "" && follow_redirect ->
          connection#cleanup;
          http_request ?headers ?http_version ~follow_redirect ~timeout ~url
            ~request ~on_body_data ~pos ()
      | _ ->
          let response_headers =
            Re.Pcre.split ~rex:(Re.Pcre.regexp "[\r]?\n")
              (Buffer.contents response_headers)
          in
          let http_version, status_code, status_message =
            parse_http_answer ~pos (List.hd response_headers)
          in
          let response_headers =
            List.fold_left
              (fun ret header ->
                if header <> "" then (
                  try
                    let res =
                      Re.Pcre.exec
                        ~rex:(Re.Pcre.regexp "([^:]*):\\s*(.*)")
                        header
                    in
                    ( String.lowercase_ascii (Re.Pcre.get_substring res 1),
                      Re.Pcre.get_substring res 2 )
                    :: ret
                  with Not_found -> ret)
                else ret)
              [] (List.tl response_headers)
          in
          connection#cleanup;
          (http_version, status_code, status_message, response_headers)
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    connection#cleanup;
    Printexc.raise_with_backtrace exn bt
