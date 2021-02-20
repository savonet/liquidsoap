# Start an interface for the "telnet" server over http.
# @category Liquidsoap
# @flag extra
# @param ~port Port of the server.
# @param ~uri URI of the server.
def server.harbor(~port=8080, ~uri="/telnet")
  def webpage(~protocol, ~data, ~headers, uri)
    data="
<html>
  <head>
    <meta charset='utf-8'/>
    <title>Liquidsoap telnet server</title>
    <style>
      body {
          font-family: sans-serif;
          /* color: lightgreen; */
          /* background-color: black; */
      }
      h1 {
          text-align: center;
      }
      textarea {
          display: block;
          margin: 0 auto;
          color: lightgreen;
          background-color: black;
          padding: 1ex;
      }
    </style>
    <script>
      window.onload = function () {
        c = document.getElementById('console');

        function send() {
          var lines = c.value.substr(0, c.selectionStart).split('\\n');
          var line = lines[lines.length-1];
          var data = line;
          console.log('send ' + line);
          var xmlHttp = new XMLHttpRequest();
          xmlHttp.open('POST', '#{uri}');
          xmlHttp.onreadystatechange = function () {
            if(xmlHttp.readyState === XMLHttpRequest.DONE) {
              var status = xmlHttp.status;
              if (status === 0 || (status >= 200 && status < 400)) {
                c.value += xmlHttp.responseText + 'END\\n';
                c.scrollTop = c.scrollHeight;
              } else {
                console.log('Failed to send values.')
              }
            }
          }
          xmlHttp.send(data);
        }

        c.addEventListener('keypress', function(e) {if (e.which == 13) {send()}})
      }
    </script>
  </head>
  <body>
    <h1>Liquidsoap telnet server</h1>
    <textarea id='console' cols='80' rows='25'></textarea>
    <p style='text-align: center'>Type <code>help</code> if you are lost.</p>
  </body>
</html>
"
    http.response(data=data)
  end

  harbor.http.register(port=port, method="GET", uri, webpage)

  def setter(~protocol, ~data, ~headers, uri)
    log.info("Executing command: #{data}")
    answer = server.execute(data)
    answer = string.concat(separator="\n", answer) ^ "\n"
    http.response(data=answer)
  end

  harbor.http.register(port=port, method="POST", uri, setter)
  log.important(label="server.harbor", "Website should be ready at <http://localhost:#{port}#{uri}>.")
end