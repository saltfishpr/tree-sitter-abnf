package tree_sitter_abnf_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_abnf "github.com/tree-sitter/tree-sitter-abnf/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_abnf.Language())
	if language == nil {
		t.Errorf("Error loading ABNF grammar")
	}
}
