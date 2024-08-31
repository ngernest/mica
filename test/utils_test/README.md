# Alcotest Test Suite 

**How to add new tests**:
- Create a new `.ml` file containing the tests (or add unit tests to 
one of the existing test `.ml` files)
- `include` the name of the test module in [`all_tests.ml`](./all_tests.ml)
  - e.g. if the name of the new test module is `test_deriver`, put the following
   in `all_tests.ml`:
   ```ocaml
   include Test_deriver
   ...
   ```
- Then, invoke the newly-added test functions in [`test_runner.ml`](./test_runner.ml), 
which executes all the Alcotest test cases defined in this directory

- Note: [`boilerplate.ml`](./boilerplate.ml) contains Alcotest utilities (e.g. 
`testable` instances) to facilitate writing unit tests 
    - The [`mk_test`](https://github.com/ngernest/ppx_mica/blob/ebfbd0bfbacb23f0dc9cc27865d8f7155010d5fb/test/utils_test/boilerplate.ml#L13) function in particular simplifies the process
    of writing Alcotest test cases
