# BEGIN
# Our custom authentication. Note: the ICY protocol does not have any username
# and for icecast, it is "source" most of the time thus, we discard it.
def harbor_auth(port, login) =
  # Alice connects on port 9000 between 20h and 21h with password "rabbit".
  (port == 9000 and 20h-21h and login.password == "rabbit")
or

    # Bob connection on port 7000 between 18h and 20h with password "foo".
    (port == 7000 and 18h-20h and login.password == "foo")

end

# END
# BEGIN2
# Authentication for live 1:
auth1 = fun (login) -> harbor_auth(9000, login)
live1 = input.harbor(port=9000, auth=auth1, "/")

# Authentication for live 2:
auth2 = fun (login) -> harbor_auth(7000, login)
live2 = input.harbor(port=7000, auth=auth2, "/")

# END2
output.dummy(fallible=true, live1)
output.dummy(fallible=true, live2)
