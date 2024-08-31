# Catching bugs in student HWs using Mica 
- Unfortunately, we are unable to post the code for students' homework submissions publicly on GitHub 

- [`mica.ml`](./lib/mica.ml) & [`test_script.ml`](./lib/test_script.ml) contains a variant of Mica's test suite for testing student submissions
- The main difference is that we use `base_quickcheck` instead of `Core.Quickcheck` (only the former has an option to continue running tests even after the property is falsified)
- We received 433 submissions in total, but 58 of them didn't compile, so they were been excluded from testing 
  - (In addition, another student submission was excluded from tests due to infinite recursion issues)
  
