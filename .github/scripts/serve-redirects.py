"""Serves a built site the way Cloudflare Pages does, for the verification pass.

Only what _redirects uses here: a literal path, or one ending in `*` whose match
fills `:splat`. The real engine is what the smoke test against the deployed site
exercises; this only has to be faithful enough to install through.

    serve-redirects.py <site> <port>
"""

import functools
import http.server
import re
import sys


def load(path):
    rules = []
    with open(path, encoding="utf-8") as handle:
        for line in handle:
            source, target, _status = line.split()
            pattern = re.compile("^" + re.escape(source).replace(r"\*", "(.*)") + "$")
            rules.append((pattern, target))
    return rules


class Handler(http.server.SimpleHTTPRequestHandler):
    rules = ()

    def do_GET(self):
        for pattern, target in self.rules:
            match = pattern.match(self.path)
            if match:
                self.send_response(302)
                self.send_header(
                    "Location",
                    target.replace(":splat", match.group(1)) if match.groups() else target,
                )
                self.end_headers()
                return
        super().do_GET()

    do_HEAD = do_GET


def main():
    site, port = sys.argv[1], int(sys.argv[2])
    Handler.rules = load(site + "/_redirects")
    server = http.server.HTTPServer(
        ("127.0.0.1", port), functools.partial(Handler, directory=site)
    )
    server.serve_forever()


if __name__ == "__main__":
    main()
