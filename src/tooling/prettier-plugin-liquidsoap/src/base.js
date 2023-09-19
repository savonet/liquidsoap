const {
  builders: {
    concat,
    group,
    trim,
    indent,
    dedent,
    join,
    literalline,
    hardlineWithoutBreakParent,
    hardline,
    line,
    softline,
    ifBreak,
    breakParent,
    indentIfBreak,
  },
} = require("prettier/doc");

module.exports.languages = [
  {
    name: "liquidsoap",
    parsers: ["liquidsoap"],
    extensions: [".liq"],
    vscodeLanguageIds: ["liquidsoap"],
  },
];

module.exports.parsers = {
  liquidsoap: {
    parse: (json) => JSON.parse(json),
    astFormat: "liquidsoap",
    locStart: () => 0,
    locEnd: () => 0,
  },
};

const hasComments = (node) => {
  if (!node.ast_comments) return false;
  return !!node.ast_comments.length;
};

const disableNewLine = (node, path) => {
  if (path.stack.length == 1) return true;
  if (path.stack[path.stack.length - 2] === "definition") return true;
  if (
    path.stack.length > 2 &&
    path.stack[path.stack.length - 3].type === "block"
  )
    return true;
  if (
    path.stack.length > 2 &&
    path.stack[path.stack.length - 3].type === "simple_fun"
  )
    return true;
  if (path.stack[path.stack.length - 2] === "then") return true;
  if (path.stack[path.stack.length - 2] === "else") return true;
  if (
    path.stack[path.stack.length - 2] === "body" &&
    path.stack[path.stack.length - 3].type === "def"
  )
    return true;

  if (node.type === "def") return false;

  if (hasComments(node)) return false;

  return true;
};

const print = (path, options, print) => {
  const node = path.getValue();

  const print_let = (label) => {
    const print_pat = () => {
      if (!node.cast) return print("pat");

      return group([
        "(",
        indent([softline, print("pat")]),
        " ",
        ":",
        " ",
        indent(print("cast")),
        softline,
        ")",
      ]);
    };

    return [
      group([
        ...(label ? [label, " "] : []),
        ...(node.decoration ? [print("decoration"), " "] : []),
        print_pat(),
        ...(node.arglist
          ? [
              "(",
              group([
                indent([
                  softline,
                  join([",", line], path.map(print, "arglist")),
                ]),
                softline,
              ]),
              ")",
            ]
          : []),
        " ",
        "=",
        group([
          indent([
            ...(label === "def" ? [hardline] : [line]),
            print("definition"),
          ]),
        ]),
        ...(label === "def"
          ? [hardline, "end"]
          : node.body.type !== "def"
          ? [softline]
          : []),
      ]),
      ...(label === "def" ? [hardline] : []),
      ...(node.body.type !== "eof" ? [hardline, print("body")] : []),
    ];
  };

  const print_opt_typ = (arg) => [
    ...(node.typ ? ["(", softline] : []),
    arg,
    ...(node.typ ? [":", softline, print("typ"), softline, ")"] : []),
  ];

  const print_label = () =>
    node.label === ""
      ? print_opt_typ(node.as_variable)
      : [
          "~",
          ...print_opt_typ(
            group([
              node.label,
              ...(node.as_variable ? ["=", node.as_variable] : []),
            ])
          ),
        ];

  const print_fun_arg = () =>
    group([...print_label(), ...(node.default ? ["=", print("default")] : [])]);

  const print_app_arg = () =>
    group([...(node.label !== "" ? [node.label, "="] : []), print("value")]);

  const print_type_annotation = () => {
    switch (node.subtype) {
      case "named":
        return node.value;
      case "nullable":
        return [print("value"), "?"];
      case "fun_arg":
        return [
          ...(node.label ? ["~", node.label] : []),
          print("value"),
          ...(node.optional ? ["?"] : []),
        ];
      case "list":
        return group(["[", indent([softline, print("value")]), softline, "]"]);
      case "json_object":
        return [
          "[",
          "(",
          group([indent([softline, "string", "*", print("value")]), softline]),
          ")",
          "]",
          " ",
          "as",
          " ",
          "json.object",
        ];
      case "tuple":
        return group([
          "(",
          indent(group([softline, join(["*"], path.map(print, "value"))])),
          softline,
          ")",
        ]);
      case "arrow":
        return [
          group([
            "(",
            indent([softline, join([",", line], path.map(print, "args"))]),
            softline,
            ")",
          ]),
          "->",
          print("value"),
        ];
      case "method_annotation":
        return [
          ...(node.json_name
            ? [JSON.stringify(node.json_name), " ", "as", " "]
            : []),
          node.name,
          ...(node.optional ? ["?"] : []),
          ":",
          " ",
          print("value"),
        ];
      case "record":
        return group([
          "{",
          group([
            indent([softline, join([",", line], path.map(print, "value"))]),
            softline,
          ]),
          "}",
        ]);
      case "method":
        return [
          print("base"),
          ".",
          "{",
          group([
            indent([line, join([",", line], path.map(print, "value"))]),
            line,
          ]),
          "}",
        ];
      case "invoke":
        return group([print("value"), indent([softline, ".", node.method])]);
      case "source_annotation":
        return node.value.length === 0
          ? []
          : group([
              "(",
              indent([
                softline,
                join(
                  [",", line],
                  [
                    ...path.map(print, "value"),
                    ...(node.extensible ? ["..."] : []),
                  ]
                ),
              ]),
              softline,
              ")",
            ]);
      case "source_track_annotation":
        return [
          node.name,
          "=",
          node.value,
          ...(node.params.length === 0
            ? []
            : [
                group([
                  "(",
                  indent([
                    softline,
                    join([",", line], path.map(print, "params")),
                  ]),
                  softline,
                  ")",
                ]),
              ]),
        ];
      case "source":
        return [node.base, print("value")];
      default:
        throw `Uknown node: ${JSON.stringify(node, null, 2)}`;
    }
  };

  const print_value = () => {
    switch (node.type) {
      case "while":
        return group([
          "while",
          line,
          print("condition"),
          line,
          "do",
          line,
          print("loop"),
          line,
          "end",
        ]);
      case "for":
        return group([
          "for",
          line,
          node.variable,
          line,
          "=",
          line,
          print("from"),
          line,
          "to",
          line,
          print("to"),
          line,
          "do",
          indent([line, print("loop")]),
          line,
          "end",
        ]);
      case "iterable_for":
        return group([
          "for",
          line,
          node.variable,
          line,
          "=",
          line,
          print("iterator"),
          line,
          "do",
          print("do"),
          indent([line, print("loop")]),
          line,
          "end",
        ]);
      case "open":
        return ["open", " ", print("left"), hardline, print("right")];
      case "if_def":
        return group([
          breakParent,
          trim,
          node.negative ? "%ifndef" : "%ifdef",
          " ",
          node.condition,
          hardline,
          group([
            group([print("then")], { id: "then" }),
            ...(node.else
              ? [
                  ifBreak("", hardline, { groupId: "then" }),
                  trim,
                  "%else",
                  hardline,
                  group([print("else")], { id: "else" }),
                  ifBreak("", hardline, { groupId: "else" }),
                ]
              : [ifBreak("", hardline, { groupId: "then" })]),
          ]),
          trim,
          "%endif",
          ifBreak("", hardline),
        ]);
      case "if_encoder":
        return group([
          breakParent,
          trim,
          node.negative ? "%ifnencoder" : "%ifencoder",
          " ",
          "%",
          node.condition,
          hardline,
          group([
            print("then"),
            ...(node.else
              ? [
                  ifBreak("", hardline, { groupId: "then" }),
                  trim,
                  "%else",
                  hardline,
                  trim,
                  group([print("else")], { id: "else" }),
                  ifBreak("", hardline, { groupId: "else" }),
                ]
              : [ifBreak("", hardline, { groupId: "then" })]),
          ]),
          trim,
          "%endif",
          ifBreak("", hardline),
        ]);
      case "if_version":
        return group([
          breakParent,
          trim,
          "%ifversion",
          " ",
          node.opt,
          " ",
          node.version,
          hardline,
          group([
            print("then"),
            ...(node.else
              ? [
                  ifBreak("", hardline, { groupId: "then" }),
                  trim,
                  "%else",
                  hardline,
                  trim,
                  group([print("else")], { id: "else" }),
                  ifBreak("", hardline, { groupId: "else" }),
                ]
              : [ifBreak("", hardline, { groupId: "then" })]),
          ]),
          trim,
          "%endif",
          ifBreak("", hardline),
        ]);
      case "negative":
        return group(["-", print("value")]);
      case "append":
        return group([print("left"), "::", print("right")]);
      case "not":
        return ["not", " ", print("value")];
      case "var":
        return node.value;
      case "ground":
        return node.value;
      case "term":
        return print("value");
      case "ellipsis":
        return ["...", print("value")];
      case "argsof":
        return group([
          "%argsof",
          "(",
          group([
            node.source,
            ...(node.only.length !== 0 || node.except.length !== 0
              ? [
                  indent([
                    "[",
                    softline,
                    join(
                      [",", softline],
                      [...node.only, ...node.except.map((s) => `!${s}`)]
                    ),
                    "]",
                  ]),
                  softline,
                ]
              : []),
          ]),
          ")",
        ]);
      case "get":
        return group(["!", print("value")]);
      case "ptuple":
      case "tuple":
        return group([
          "(",
          group([
            indent([softline, join([",", line], path.map(print, "value"))]),
            softline,
          ]),
          ")",
        ]);
      case "list":
        return group([
          "[",
          indent(
            group([softline, join([",", line], path.map(print, "value"))])
          ),
          softline,
          "]",
        ]);
      case "pmeth":
        return group([
          "{",
          indent([softline, join([",", line], path.map(print, "value"))]),
          softline,
          "}",
        ]);
      case "pvar":
        return node.value.join(".");
      case "plist":
        return group([
          "[",
          indent([
            softline,
            join(
              [",", line],
              [
                ...path.map(print, "left"),
                ...(node.middle ? [group(["...", node.middle])] : []),
                ...path.map(print, "right"),
              ]
            ),
          ]),
          softline,
          "]",
        ]);
      case "invoke": {
        const invoke_meth = [
          ...(node.optional ? ["?"] : []),
          ".",
          print("meth"),
        ];

        return group([
          print("invoked"),
          ...(node.meth.type === "var" ? [indent(invoke_meth)] : invoke_meth),
        ]);
      }
      case "type_annotation":
        return print_type_annotation();
      case "parenthesis":
        return group(["(", indent([softline, print("value")]), softline, ")"]);
      case "block":
        return group(["begin", indent([line, print("value")]), line, "end"]);
      case "cast":
        return group(["(", print("left"), " ", ":", " ", print("right"), ")"]);
      case "fun":
        return group([
          "fun",
          " ",
          group([
            "(",
            indent([softline, join([",", line], path.map(print, "arguments"))]),
            softline,
            ")",
            " ",
            "->",
          ]),
          indent([line, print("body")]),
        ]);
      case "fun_arg":
        return print_fun_arg();
      case "app_arg":
        return print_app_arg();
      case "app":
        return group([
          print("op"),
          "(",
          ...(node.args.length === 0
            ? []
            : [
                indent([
                  softline,
                  group([join([",", line], path.map(print, "args"))]),
                ]),
                softline,
              ]),
          ")",
        ]);
      case "eof":
        return "";
      case "seq":
        return [
          group([
            ...(hasComments(node) || disableNewLine(node, path)
              ? []
              : [softline]),
            print("left"),
            ...(node.right.type === "def" ? [] : [softline]),
          ]),
          hardline,
          print("right"),
        ];
      case "def":
        return print_let("def");
      case "let":
        return print_let("let");
      case "binding":
        return print_let();
      case "simple_fun":
        return group(["{", indent([softline, print("value")]), softline, "}"]);
      case "if":
        return group([
          "if",
          indent([line, group(print("condition"))]),
          line,
          "then",
          indent([line, print("then")]),
          ...(node.elsif.length !== 0
            ? [line, join(line, path.map(print, "elsif"))]
            : []),
          ...(node.else ? [line, "else", indent([line, print("else")])] : []),
          line,
          "end",
        ]);
      case "elsif":
        return group([
          "elsif",
          indent([line, print("condition")]),
          line,
          "then",
          indent([line, print("then")]),
        ]);
      case "inline_if":
        return group([
          print("condition"),
          line,
          "?",
          group([indent([line, print("then")]), line]),
          ":",
          group([indent([line, print("else")]), line]),
        ]);
      case "infix":
        return group([
          print("left"),
          " ",
          node.op,
          indent([line, print("right")]),
        ]);
      case "bool":
        return group(
          join(
            [dedent(line), node.op, line],
            path.map(
              (v) => group([indent([softline, print(v)]), softline]),
              "value"
            )
          )
        );
      case "string_interpolation":
        return group(path.map(print, "value"));
      case "interpolated_string":
        return node.value;
      case "interpolated_term":
        return ["#{", print("value"), "}"];
      case "coalesce":
        return group([
          print("left"),
          indent([line, "??", indent([line, print("right")])]),
        ]);
      case "assoc":
        return group([
          print("left"),
          "[",
          indent([softline, print("right")]),
          softline,
          "]",
        ]);
      case "include_lib":
        return [
          trim,
          "%include",
          " ",
          "<",
          node.value,
          ">",
          hardlineWithoutBreakParent,
        ];
      case "include":
        return [
          trim,
          "%include",
          " ",
          '"',
          node.value,
          '"',
          hardlineWithoutBreakParent,
        ];
      case "include_extra":
        return [
          trim,
          "%include_extra",
          " ",
          '"',
          node.value,
          '"',
          hardlineWithoutBreakParent,
        ];
      case "time_interval":
        return [print("left"), "-", print("right")];
      case "time":
        return [
          ...(node.week ? ["" + node.week, "w"] : []),
          ...(node.hours ? ["" + node.hours, "h"] : []),
          ...(node.minutes ? ["" + node.minutes, "m"] : []),
          ...(node.second ? ["" + node.seconds, "s"] : []),
        ];
      case "encoder":
        return [
          "%",
          node.label,
          ...(node.params.length === 0
            ? []
            : [
                group([
                  "(",
                  indent([
                    softline,
                    join([",", line], path.map(print, "params")),
                  ]),
                  softline,
                  ")",
                ]),
              ]),
        ];
      case "regexp":
        return group(["r/", node.name, "/", ...node.flags]);
      case "methods":
        return [
          ...(node.base ? [print("base"), "."] : []),
          group([
            "{",
            indent([
              softline,
              join(
                [",", line],
                [
                  ...path.map(print, "methods"),
                  ...(node.spread ? [["...", print("spread")]] : []),
                ]
              ),
            ]),
            softline,
            "}",
          ]),
        ];
      case "method":
        return [node.name, "=", print("value")];
      case "try":
        return group([
          "try",
          indent(group([line, print("body")], { shouldBreak: true })),
          line,
          group([
            "catch",
            line,
            node.variable,
            ...(node.errors_list
              ? [group([line, ":", line, print("errors_list")])]
              : []),
            line,
            "do",
          ]),
          indent(group([line, print("handler")], { shouldBreak: true })),
          line,
          "end",
        ]);
      default:
        throw `Uknown node: ${JSON.stringify(node, null, 2)}`;
    }
  };

  return [
    ...(disableNewLine(node, path) ? [] : [hardlineWithoutBreakParent]),
    join(
      [hardlineWithoutBreakParent, hardlineWithoutBreakParent],
      (node.ast_comments || []).map((c) =>
        join([hardlineWithoutBreakParent], c)
      )
    ),
    ...(node.ast_comments?.length > 0 ? [hardlineWithoutBreakParent] : []),
    print_value(),
    ...(path.stack.length === 1 ? [hardlineWithoutBreakParent] : []),
  ];
};

module.exports.printers = {
  liquidsoap: {
    print,
  },
};
