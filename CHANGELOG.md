# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.1] - 2025-01-06

### Fixed
- Fixed context URL mapping for Fluree ledger context to properly handle `https://ns.flur.ee/ledger/v1` ([#42](https://github.com/fluree/json-ld/pull/42))
- Fixed compaction issue where derived context entries interfered with IRI compaction ([#42](https://github.com/fluree/json-ld/pull/42))
  - IRIs like `https://ns.flur.ee/ledger#v` now correctly compact to `"v"` instead of remaining as full IRIs
- Fixed processor API tests that were using incorrect context URLs

### Changed
- Incorporated fidx (Fluree index) namespace properties into the v1 context ([#42](https://github.com/fluree/json-ld/pull/42))
- Updated all EDN context files with new `:derived?` metadata to fix compaction

## [1.0.0] - 2024-12-09

### Added
- Initial release of Fluree JSON-LD library
- Full JSON-LD 1.1 support for expand, compact, and normalize operations
- Support for both Clojure and ClojureScript
- Pre-parsed external contexts for improved performance
- Integration with external JSON-LD processors

[Unreleased]: https://github.com/fluree/json-ld/compare/v1.0.1...HEAD
[1.0.1]: https://github.com/fluree/json-ld/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/fluree/json-ld/releases/tag/v1.0.0