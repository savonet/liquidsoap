# Strings encoding

Liquidsoap operates internally using the UTF-8 string encoding. Most strings inside the application are converted
to UTF-8 whenever possible. Conversion is done using [camomile](https://github.com/ocaml-community/camomile) automatic
string encoding detection. If the conversion fails, the string is kept as-is. The list of encodings used for automatic
detection is set via [settings.charset.encodings](settings.html#list-of-encodings-to-try-for-automatic-encoding-detection.)

There are some exceptions, however. For instance, filenames and paths are not converted: if your system expects paths
to be in a different encoding than UTF-8 then we do need to keep strings representing files and paths in this encoding
to prevent errors.

In general, you are advised to set the string encoding to UTF-8 on all systems running liquidsoap scripts for consistency
and clarity.

However, if for some reasons you need to tweak string encoding, these settings can be of use:

- `settings.log.recode` and `settings.log.recode.encoding`: set the first one to `true` and the second one to the string encoding you would like log entries to be converted into.
- `settings.metadata.recode`: set to `false` to prevent metadata from being converted to UTF-8.
