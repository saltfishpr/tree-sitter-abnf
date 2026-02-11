import XCTest
import SwiftTreeSitter
import TreeSitterAbnf

final class TreeSitterAbnfTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_abnf())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading ABNF grammar")
    }
}
