# Examples

You can use the following command to run an example, replacing `<name>` with the
appropriate value:
```bash
spago -x example.dhall run -p examples/<name>.purs -m Example.<name>
```

## CSS output
The [`output`](./output) subdirectory contains the CSS output produced by each example.

## Type errors
The [`type-errors`](./type-errors) subdirectory contains examples of type safety that Tecton adds to CSS. All of these examples fail to compile (intentionally).
