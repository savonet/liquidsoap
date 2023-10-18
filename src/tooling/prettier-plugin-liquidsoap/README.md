A [Prettier](https://prettier.io/) plugin and binary for Liquidsoap code

This package provides a [Prettier](https://prettier.io/) plugin for liquidsoap code
as well as a `liquidsoap-prettier` binary for formatting liquidsoap scripts.

## Installation

### `liquidsoap-prettier`

The `liquidsoap-prettier` command-line utility should be installed with this
package and should be available following the usual node package binary
conventions.

It works as follows:

```shell
$ liquidsoap-prettier [-w|--write] /path/to/file.liq
```

### Local prettier plugin

In general, `prettier` works better when installed locally but this requires a `package.json` at the
root of your project.

First, install `prettier-plugin-liquidsoap` as a dev-dependency:

```sh
npm install -D prettier prettier-plugin-liquidsoap
```

Then add the plugin to your Prettier config:

```json
// .prettierrc
{
  "plugins": ["prettier-plugin-liquidsoap"]
}
```

### Global prettier plugin

Installing the plugin globally works but currently requires a couple of workarounds. You need to install the plugin
and the `prettier` module globally:

```shell
% npm install -g prettier prettier-plugin-liquidsoap
```

Next, you need to create a prettier configuration file in your home directory. This file must reference the absolute path
to the plugin entrypoint:

```json
// $HOME/.prettierrc
{
  "plugins": ["/path/to/prettier-plugin-liquidsoap/dist/node.js"],
  "overrides": [
    {
      "files": "*.liq",
      "options": {
        "parser": "liquidsoap"
      }
    }
  ]
}
```

To find out the absolute path to the plugin entrypoint you can do:

```shell
% which prettier
<prefix>/bin/prettier
```

The plugin should then be located at: `<prefix>/lib/node_modules/prettier-plugin-liquidsoap/dist/node.js`
