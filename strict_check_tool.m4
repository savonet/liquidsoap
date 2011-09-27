AC_DEFUN([AC_CHECK_TOOL_STRICT],
[AC_CHECK_PROG([$1], [${ac_tool_prefix}$2], [${ac_tool_prefix}$2], [$3], [$4])])

AC_DEFUN([AC_PATH_TOOL_STRICT],
[AC_PATH_PROG([$1], [${ac_tool_prefix}$2], [$3])])


