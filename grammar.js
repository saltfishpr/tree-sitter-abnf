/**
 * @file Abnf grammar for tree-sitter
 * @author saltfishpr
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: "abnf",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
