import prettierDoc from "prettier/doc";

const {
  builders: {
    group,
    trim,
    indent,
    dedent,
    join,
    hardline,
    line,
    litteralline,
    softline,
    ifBreak,
    lineSuffix,
    fill,
  },
} = prettierDoc;

export const languages = [
  {
    name: "liquidsoap",
    parsers: ["liquidsoap"],
    extensions: [".liq"],
    vscodeLanguageIds: ["liquidsoap"],
  },
];

export const parsers = {
  liquidsoap: {
    parse: (json) => JSON.parse(json),
    astFormat: "liquidsoap",
    locStart: () => 0,
    locEnd: () => 0,
  },
};

const printString = (str) => {
  if (/(?<!\\)\n/.test(str)) return str;
  if (!/\s/.test(str)) return str;

  return fill(
    str
      .replace(/\\\n\s*/g, "")
      .split(/(\s)/)
      .map((s) =>
        s === " " ? ifBreak(group([" ", "\\", hardline, " "]), " ") : s,
      ),
  );
};

const print = (path, options, print) => {
  const node = path.getValue();

  const printIfDef = (...ifdef) => [
    group([trim, ...ifdef, hardline]),
    group([print("then")]),
    ...(node.else
      ? [group([hardline, trim, "%else", hardline]), group([print("else")])]
      : []),
    group([hardline, trim, "%endif"]),
  ];

  const printPat = () => {
    if (!node.cast) return print("pat");

    return group([
      "(",
      group([indent([softline, print("pat")]), line, ":"]),
      indent([line, print("cast")]),
      softline,
      ")",
    ]);
  };

  const newLine = (pos1, pos2) => {
    const posBefore = pos1?.[1]?.lnum;
    const posAfter = pos2?.[0]?.lnum;
    return posBefore && posAfter && posAfter - posBefore > 1 ? [hardline] : [];
  };

  const printEllipsis = (p) => {
    node.ast_comments = node.value.ast_comments;
    node.position = node.value.position;
    delete node.value.ast_comments;
    return group(["...", print(p)]);
  };

  const printComments = (node, content) => {
    if (!node.ast_comments) return [node.position, content];

    const beforeComments = node.ast_comments.before;
    const afterComments = node.ast_comments.after;

    delete node.ast_comments;

    const [contentWithBeforeComments, beforePosition] = beforeComments
      .reverse()
      .reduce(
        ([content, position], comment) => [
          [
            join(hardline, comment.value),
            hardline,
            ...(position === node.position
              ? newLine(comment.position, position)
              : [hardline]),
            ...content,
          ],
          comment.position,
        ],
        [[content], node.position],
      );

    const [contentWithAfterComments, afterPosition] = afterComments.reduce(
      ([content, position], comment) => [
        [
          ...content,
          ...(position === node.position
            ? [hardline, ...newLine(position, comment.position)]
            : [hardline, hardline]),
          lineSuffix(join(hardline, comment.value)),
        ],
        comment.position,
      ],
      [contentWithBeforeComments, node.position],
    );

    return [[beforePosition[0], afterPosition[1]], contentWithAfterComments];
  };

  const printSeq = (pos1, content1, pos2, content2) => {
    return [content1, hardline, ...newLine(pos1, pos2), content2];
  };

  const printDef = (pos1, node1) =>
    node.body.type === "eof"
      ? node1
      : printSeq(pos1, node1, node.body.position, print("body"));

  const joinWithComments = (join, p) => {
    const nodes = node[p].map((node) => {
      if (node.type === "term") {
        node.ast_comments = node.value.ast_comments;
        node.position = node.value.position;
        delete node.value.ast_comments;
      }

      const { ast_comments, position } = node;

      delete node.ast_comments;

      return { ast_comments, position };
    });

    const content = path.map(print, p);

    return content.reduce(
      ([position, result], c, idx) => {
        const isLast = idx === content.length - 1;
        const joinedContent = isLast ? c : [c, join];

        if (!nodes[idx].position)
          return [position, [result, joinedContent, ...(isLast ? [] : [line])]];

        const [nextPosition, contentWithComments] = printComments(
          nodes[idx],
          joinedContent,
        );

        return [
          [position[0], nextPosition[1]],
          [
            ...result,
            ...newLine(position, nextPosition),
            contentWithComments,
            ...(isLast ? [] : [line]),
          ],
        ];
      },
      [node.position, []],
    )[1];
  };

  const printOptTyp = (arg) => [
    ...(node.typ ? ["(", softline] : []),
    arg,
    ...(node.typ ? [":", softline, print("typ"), softline, ")"] : []),
  ];

  const printLabel = () =>
    node.label === ""
      ? printOptTyp(node.as_variable)
      : [
          "~",
          ...printOptTyp(
            group([
              node.label,
              ...(node.as_variable ? ["=", node.as_variable] : []),
            ]),
          ),
        ];

  const printFunArg = () =>
    group([...printLabel(), ...(node.default ? ["=", print("default")] : [])]);

  const printAppArg = () => {
    if (node.label)
      return group([node.label, "=", indent([softline, print("value")])]);
    return print("value");
  };

  const printTypeAnnotation = () => {
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
            indent([softline, joinWithComments([","], "args")]),
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
            indent([softline, joinWithComments([","], "value")]),
            softline,
          ]),
          "}",
        ]);
      case "method":
        return [
          print("base"),
          ".",
          "{",
          group([indent([line, joinWithComments([","], "value")]), line]),
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
                  ],
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
                  indent([softline, joinWithComments([","], "params")]),
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

  const printValue = () => {
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
          " ",
          group([node.variable, " ", "=", indent([line, print("from")]), line]),
          "to",
          indent([line, print("to")]),
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
        return printIfDef(
          node.negative ? "%ifndef" : "%ifdef",
          " ",
          node.condition,
        );
      case "if_encoder":
        return printIfDef(
          node.negative ? "%ifnencoder" : "%ifencoder",
          " ",
          "%",
          node.condition,
        );
      case "if_version":
        return printIfDef("%ifversion", " ", node.opt, " ", node.version);
      case "negative":
        return group(["-", print("value")]);
      case "append":
        return group([print("left"), "::", print("right")]);
      case "not":
        return group(["not", " ", print("value")]);
      case "var":
        return node.value;
      case "string":
        return printString(node.value);
      case "ground":
        return node.value;
      case "term":
        return print("value");
      case "ellipsis":
        return printEllipsis("value");
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
                      [...node.only, ...node.except.map((s) => `!${s}`)],
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
            indent([softline, joinWithComments([","], "value")]),
            softline,
          ]),
          ")",
        ]);
      case "list":
        return group([
          "[",
          indent(group([softline, joinWithComments([","], "value")])),
          softline,
          "]",
        ]);
      case "pmeth":
        return group([
          "{",
          indent([softline, joinWithComments([","], "value")]),
          softline,
          "}",
        ]);
      case "pvar":
        return group([indent(join([softline, "."], node.value))]);
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
              ],
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
        return printTypeAnnotation();
      case "parenthesis":
        return group(["(", indent([softline, print("value")]), softline, ")"]);
      case "block":
        return group(["begin", indent([line, print("value")]), line, "end"]);
      case "cast":
        return group([
          "(",
          group([indent([softline, print("left")]), line, ":"]),
          indent([line, print("right")]),
          softline,
          ")",
        ]);
      case "fun":
        return group([
          "fun",
          " ",
          group([
            "(",
            indent([softline, joinWithComments([","], "arguments")]),
            softline,
            ")",
            " ",
            "->",
          ]),
          indent([line, print("body")]),
        ]);
      case "fun_arg":
        return printFunArg();
      case "app_arg":
        return printAppArg();
      case "app":
        return group([
          print("op"),
          "(",
          ...(node.args.length === 0
            ? []
            : [
                indent([softline, group([joinWithComments([","], "args")])]),
                softline,
              ]),
          ")",
        ]);
      case "eof":
        return "";
      case "seq":
        if (node.right.type === "eof") return print("left");
        return printSeq(
          node.left.position,
          print("left"),
          node.right.position,
          print("right"),
        );
      case "def":
        return printDef(
          ...printComments(
            node,
            group([
              "def",
              " ",
              ...(node.decoration ? [print("decoration"), " "] : []),
              printPat(),
              ...(node.arglist
                ? [
                    "(",
                    group([
                      indent([softline, joinWithComments([","], "arglist")]),
                      softline,
                    ]),
                    ")",
                  ]
                : []),
              " ",
              "=",
              group([indent([hardline, print("definition")]), hardline, "end"]),
            ]),
          ),
        );
      case "let":
        return printDef(
          ...printComments(
            node,
            group([
              "let",
              " ",
              ...(node.decoration ? [print("decoration"), " "] : []),
              printPat(),
              " ",
              "=",
              group([indent([line, print("definition")])]),
            ]),
          ),
        );
      case "binding":
        return printDef(
          ...printComments(
            node,
            group([
              printPat(),
              " ",
              "=",
              group([indent([line, print("definition")])]),
            ]),
          ),
        );
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
              "value",
            ),
          ),
        );
      case "string_interpolation":
        return group(path.map(print, "value"));
      case "interpolated_string":
        return printString(node.value);
      case "interpolated_term":
        return group(["#{", indent([softline, print("value")]), softline, "}"]);
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
        return group([trim, "%include", " ", "<", node.value, ">"]);
      case "include":
        return group([trim, "%include", " ", '"', node.value, '"']);
      case "include_extra":
        return group([trim, "%include_extra", " ", '"', node.value, '"']);
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
                  indent([softline, joinWithComments([","], "params")]),
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
            indent([softline, joinWithComments([","], "methods")]),
            softline,
            "}",
          ]),
        ];
      case "method":
        return group([node.name, "=", indent([softline, print("value")])]);
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
    printComments(node, printValue())[1],
    ...(path.stack.length == 1 ? ["\n"] : []),
  ];
};

export const printers = {
  liquidsoap: {
    print,
  },
};

export default { languages, parsers, printers };
