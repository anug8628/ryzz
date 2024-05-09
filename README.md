# RYZZ

Hello! Welcome to the RYZZ programming language.

## Testing
To test the parser:
```
cd src/
ocamlbuild parse_test.native
./parse_test.native < test
```

To test semantics:
```
cd src/
ocamlbuild semantic_test.native
./semantic_test.native < test
```