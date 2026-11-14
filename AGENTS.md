# prefix-units

The Linux GHC list in `.github/workflows/ci.yml` is the set of supported compilers.
`tested-with` lists those majors and must not list `latest`.

The library `base` lower bound is the `base` that ships with the oldest of those
compilers. The upper bound stays `< 5`.

Do not add a compatibility shim for a compiler the workflow does not build.

`cabal.project.local` and `dist-newstyle/` are generated. They stay gitignored.

When modifying the build configuration, keep the Makefile rules and the CI rules
in sync, so that both local runs and CI worker runs do the same. Also, see if
README needs to be updated.

When editing the actual code, make sure to add tests that cover all changes, and
to run tests (`make test`) and the linter (`make lint`) before committing the
code. While this only covers the local version of GHC, it is a baseline step.
