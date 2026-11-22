# css-backend-lass

[css-protocol](https://github.com/egao1980/css-protocol) backend for [LASS](https://codeberg.org/shinmera/LASS) (Shinmera, zlib; imported unmodified through cl-stack-systems). It compiles LASS S-expressions into the css-protocol model and adds the extensions of [classie](https://github.com/egao1980/classie), which it supersedes: CSS unit arithmetic, colour functions, `:let*`, `:include`, `ceil`, and an ASDF component that builds a `.css` at system load.

**Status:** contract (slice H4h-0b-1): API, conditions and the full Rove suite are in; the implementation is stubbed. Brief: [cl-stack/docs/capabilities/css.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/css.md).

## License

MIT, see [LICENSE](LICENSE). LASS is zlib.
