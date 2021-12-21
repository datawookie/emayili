# {emayili} 0.7.2

* Add `smtpbucket()` which makes it easy to use SMTP Bucket for testing.

# {emayili} 0.7.1

* Fix encoding of non-ASCII characters in MIME headers.

# {emayili} 0.7.0

* Added full support for GPG signed and encrypted messages. (issue #84)

# {emayili} 0.6.9

* Added initial support for GPG signed and encrypted messages.

# {emayili} 0.6.8

* Added tests for `{Microsoft365R}` compatibility.

# {emayili} 0.6.7

* Added `request_receipt_read()`.
* Added support for `{htmltools}` `tagList` and vector of
  characters in `html()`. (@colinfay, issue #92)

# {emayili} 0.6.6

* Added `Content-Language` header field.
* Added `Comments` & `Keywords` header fields.
* Added support for Sendinblue and MailerSend.
* Fix attachment name (#91).
  Thanks https://github.com/nlooije.

# {emayili} 0.6.5

* Added `gmail()`, `sendgrid()` and `mailgun()`.
* Added `return_path()`.

# {emayili} 0.6.4

* Fix issue with inferring protocol (#89).
  Thanks https://github.com/stibu81.

# {emayili} 0.6.3

* Fix issue with appending more than two message parts (#87).
  Thanks https://github.com/tinku-borah.
* Added `inreplyto()` and `references()`.

# {emayili} 0.6.2

* Added `expires()`, `replyby()` and `sensitivity()`.
* Added disposition argument to `attachment()`.
* Merged `new_envelope()` into `envelope()`.
* Improved implementation of `qp_encode()` and `qp_decode()`.

# {emayili} 0.6.1

* Add `importance()` for `Importance` header field.
* Add `priority()` for `Priority` header field.
* Add `Content-MD5` header field.

# {emayili} 0.6.0

* Refactor representation of Mail and MIME headers.

# {emayili} 0.5.6

* Also support `include_css` in `html()`.

# {emayili} 0.5.5

* Use include_css option to specify what rendered CSS is included. Can specify
  a combination of `"rmd"`, `"bootstrap"` and `"highlight"`.

# {emayili} 0.5.4

* Wrap `<img>` in `<figure>` and support `fig.cap`, `fig.alt` & `fig.class`.

# {emayili} 0.5.3

* Cache rendered Markdown.

# {emayili} 0.5.2

* Pass params argument through to `rmarkdown::render()`.
* Include extra CSS.
* Include external CSS.
* Replace bare `"\n"` with `"\r\n"`.

# {emayili} 0.5.1

* Add `SystemRequirements` for Pandoc.
* Check for Pandoc before calling `render()`.
* Complete test coverage.

# {emayili} 0.5.0

* Interpolate in `subject()` and `html()`.
* Improved test coverage.

# {emayili} 0.4.20

* Render R Markdown into HTML body (separate functions for plain & R Markdown).
* Add `MIME` class.

# {emayili} 0.4.19

* Render plain Markdown into HTML body.

# {emayili} 0.4.18

* Add `envelope_details` and `envelope_invisible` options.
* Enable `{glue}` interpolation in `text()`.

# {emayili} 0.4.17

* Fix regression in `From` and `Sender` header fields.
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

* The `html()` function can now read HTML from a file (#70).
  Thanks https://github.com/freuerde.

# {emayili} 0.4.13

* Fix `from = NULL` in `envelope()` (#69).
  Thanks https://github.com/stibu81.

# {emayili} 0.4.12

* Can specify explicit HELO domain (#68).
  Thanks https://github.com/Rdataflow.

# {emayili} 0.4.11

* Email addresses can include name. Both `"Bart Simpson <bart@eatmyshorts.com>"`
  and `"bart@eatmyshorts.com"` are valid (#67).
* Add `NEWS.md` (#66).
