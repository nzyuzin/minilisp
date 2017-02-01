Tests
=====

Test files
----------

Test file is a regular text file containing a minischeme program with an
additional condition that the first line of a test file should contain a
comment with the expected evaluation result of the last line.

Example of a test file:

```scheme
; 42 ; we expect this value to be result of a program

(define id (lambda (x) x)) ; regular definition

(id 42) ; Last line of the text file, it should be evaluated to 42
```

Running the tests
-----------------

All tests can be run with `./test <filename> <expected_result>`. To automate
this, a script `run_all_tests` is provided in the root of the repository.  This
script will run all tests in alphabetic order with regard to `test_file` name.
The result of a test execution will be displayed as either `true` or `false`,
meaning that the test respectively succeeded or not.
