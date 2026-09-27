# Using in production

Our Debian, Ubuntu and Alpine packages create a `liquidsoap` system user and
group, a log directory `/var/log/liquidsoap` and a cache directory
`/var/cache/liquidsoap`. The package also fills that cache for the standard
library, which speeds up startup.

In production, run liquidsoap in the foreground under a service manager, such as
`systemd` on Linux or `launchd` on macOS. The service manager starts your
script at boot, restarts it if it crashes, runs it as the `liquidsoap` user and
captures its output. Here is a minimal `systemd` unit, for instance in
`/etc/systemd/system/radio.service`:

```
[Unit]
Description=My radio
After=network-online.target
Wants=network-online.target

[Service]
User=liquidsoap
Group=liquidsoap
ExecStart=/usr/bin/liquidsoap /etc/liquidsoap/radio.liq
Restart=always

[Install]
WantedBy=multi-user.target
```

Enable and start it with `systemctl enable --now radio`. Your scripts do not
need the `#!` line when started this way.

By default, liquidsoap logs to its standard output, which `systemd` sends to
the journal. To log to a file, set `settings.log.file := true`. The file goes
to `settings.log.file.path`, which defaults to
`<syslogdir>/<script>.log`, that is `/var/log/liquidsoap/<script>.log` with our
packages. Liquidsoap reopens its log file when it receives `SIGUSR1`, so a
`logrotate` configuration can rotate it with:

```
postrotate
  systemctl kill --signal=USR1 radio
endscript
```

Errors in a script are easier to catch before a restart. Check your script after
each modification with `liquidsoap --check /etc/liquidsoap/radio.liq`, then
restart the service.
