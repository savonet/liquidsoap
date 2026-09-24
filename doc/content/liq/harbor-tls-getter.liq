certificate = ref("/path/to/cert.pem")
transport = http.transport.ssl(certificate={certificate()})
