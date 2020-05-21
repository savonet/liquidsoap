type socket = { ctx : SecureTransport.t; sock : Unix.file_descr }

include Http.Http_t with type connection = socket
