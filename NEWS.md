# {emayili} 0.4.18

* Add envelope_details option.
* Enable {glue} interpolation in text().

# {emayili} 0.4.17

* Fix regression in From and Sender header fields.
* Improve test for formatting of header fields.

# {emayili} 0.4.16

* Add `address` class with the following methods:

  - `compliant()`
  - `raw()`
  - `display()`
  - `local()` and
  - `domain()`.

# {emayili} 0.4.15

* Moved testing to GitHub actions

# {emayili} 0.4.14

* The html() function can now read HTML from a file (#70).
  Thanks https://github.com/freuerde.

# {emayili} 0.4.13

* Fix from = NULL in envelope() (#69).
  Thanks https://github.com/stibu81.

# {emayili} 0.4.12

* Can specify explicit HELO domain (#68).
  Thanks https://github.com/Rdataflow.

# {emayili} 0.4.11

* Email addresses can include name. Both "Bart Simpson <bart@eatmyshorts.com>"
  and "bart@eatmyshorts.com" are valid (#67).

* Add `NEWS.md` (#66).
