Contributing
============

Welcome, and thanks for your interest in Tecton. Whether you are new to CSS and PureScript or here to teach us new tricks, we're glad to have you and look forward to your contributions. Listed below are a few ways you can help.

Asking questions
----------------
If you have a question, please [open an issue](https://github.com/nsaunders/purescript-tecton/issues/new) to discuss it. This might help us to find a defect; reveal an opportunity to improve the documentation or developer experience; or help someone facing a similar issue in the future.

Reporting defects
-----------------
Please [open an issue](https://github.com/nsaunders/purescript-tecton/issues/new) to discuss any defects you find. In addition to a brief description, include
* the code you wrote;
* the expected output;
* the compiler error or incorrect output you encountered; and
* if possible, a link to the relevant W3C specification, which can be found [here](https://www.w3.org/TR/css-2021/) or via Google search.

Improving documentation
-----------------------
Please share any suggestions for improving or adding to the documentation by [submitting a pull request](https://github.com/nsaunders/purescript-tecton/compare) or [opening an issue](https://github.com/nsaunders/purescript-tecton/issues/new).

Sharing resources
-----------------
If you have created a tutorial, auxiliary library, interesting code example, or other resource related to Tecton, please [share it with the PureScript community](https://discourse.purescript.org). Increasing awareness will bring more users (and potential contributors) to the project.

Submitting code
---------------
Pull requests are welcome, but we ask that you [open an issue](https://github.com/nsaunders/purescript-tecton/issues/new) to discuss your plans before making any significant investment of your valuable time.

### Development environment

You can use [Nix](https://github.com/NixOS/nix) to create a development environment with all required tooling. Simply run `nix-shell` in the root project directory, and you'll have everything you need to get started.

### Making changes

The `src` directory includes the following modules:
* [`Tecton`](./src/Tecton.purs), which exports most of the public API;
* [`Tecton.Internal`](./src/Tecton/Internal.purs), which contains internal and private functions and data types; and
* [`Tecton.Rule`](./src/Tecton/Rule.purs), for use with qualified-do syntax to create rulesets (not much to see here).

Usually your work will begin in the `Tecton.Internal` module. From this module, export the minimum functions, types, and classes required for client code to compile. Then, from the `Tecton` module, re-export only the subset that client code should interact with directly. This architecture clearly defines the public API while allowing implementation details to remain flexible.

After making any changes, run `purs-tidy format-in-place src` to format the code.

> ⚠️ **NOTE**: Only format the modules in the `src` directory, as `purs-tidy` doesn't seem to format style sheets as intended at this time.

Unit test(s) should accompany each change. Ensure that the system under test (e.g. the new property binding you added) is imported from the `Tecton` module so that tests accurately represent client code. Tests are organized into modules corresponding to W3C CSS specifications, which can be found [here](https://www.w3.org/TR/css-2021/), and almost always follow this format:

```purescript
   "margin:0"        `isRenderedFrom` margin := nil
-- ^ expected output                  ^ given this input
```

The `isRenderedFrom` utility, imported from `Test.Util`, provides an appropriate test description and the corresponding assertion automatically.

### Preparing your submission

Please verify your changes before submitting a pull request using the following commands:
1. `purs-tidy check src`, which ensures that modules in the `src` directory conform to the project's formatting standards;
2. `spago -x test.dhall test`, which runs the unit tests; and
3. `npm i && npm run check-examples`, which (as a basic sanity check) compares the output of each [example](./examples) to a previous snapshot.

> ℹ️ **NOTE**: If appropriate, running `npm run check-examples -- --update` will update the example output snapshots.

### Submitting a pull request

When you are ready to [submit a pull request](https://github.com/nsaunders/purescript-tecton/compare), please make sure to include:
1. a brief description of the change;
1. a link to the related issue, if applicable;
1. a link to any relevant [W3C specifications](https://www.w3.org/TR/css-2021/)
1. any design considerations you feel are important; and
1. any ideas you may have to improve upon your submission in the future.
