A [Prettier v3+](https://prettier.io/) plugin for Liquidsoap code

## Installation

### Locally

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

### Globally

Installing globally works but currently requires a couple of workarounds. You need to install the plugin
and the `prettier` module globally:

```shell
% npm install -g prettier prettier-plugin-liquidsoap
```

Next, you need to create a prettier configuration file in your home directory. This file must reference the absolute path
to the plugin entrypoint:

```json
// $HOME/.prettierc
{
  "plugins": ["/path/to/prettier-plugin-liquidsoap/src/index.js"],
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

The plugin should then be located at: `<prefix>lib/node_modules/prettier-plugin-liquidsoap/src/index.js`
