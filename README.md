
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emayili <img src="man/figures/emayili-hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/emayili)](https://cran.r-project.org/package=emayili)
[![Travis-CI build
status](https://travis-ci.org/datawookie/emayili.svg?branch=master)](https://travis-ci.org/datawookie/emayili)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/datawookie/emayili.svg)](https://codecov.io/github/datawookie/emayili)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

emayili is a package for sending emails from R. The design goals are:

  - works on all manner of SMTP servers and
  - has minimal dependencies (or dependencies which are easily
    satisfied).

The package name is an adaption of the Zulu word for email, imeyili.

## Installation

Get the stable version from
[CRAN](https://CRAN.R-project.org/package=emayili).

``` r
install.packages("emayili")
```

Or grab it directly from
[GitHub](https://github.com/datawookie/emayili).

``` r
# Install from the master branch.
remotes::install_github("datawookie/emayili")
# Install from the development branch.
remotes::install_github("datawookie/emayili", ref = "dev")
```

## Usage

First create a message object.

``` r
library(emayili)
library(magrittr)

email <- envelope()
```

### Creating a Message

The message has class `envelope`.

``` r
class(email)
```

    [1] "envelope"

Add addresses for the sender and recipient.

``` r
email <- email %>%
  from("alice@yahoo.com") %>%
  to("bob@google.com") %>%
  cc("craig@google.com")
```

There are also `bcc()` and `reply()` functions for setting the `Bcc` and
`Reply-To` fields.

Add a subject.

``` r
email <- email %>% subject("This is a plain text message!")
```

Add a text body. You can use `html()` to add an HTML body.

``` r
email <- email %>% text("Hello!")
```

Add an attachment.

``` r
email <- email %>% attachment("image.jpg")
```

You can also create the message in a single command:

``` r
email <- envelope(
  to = "bob@google.com",
  from = "alice@yahoo.com",
  subject = "This is a plain text message!",
  text = "Hello!"
)
```

Simply printing a message displays the header information.

``` r
email
```

    Date:         Fri, 23 Oct 2020 09:02:39 GMT
    From:         alice@yahoo.com
    To:           bob@google.com
    Cc:           craig@google.com
    Subject:      This is a plain text message!
    X-Mailer:     {emayili}-0.4.3

You can identify emails which have been sent using `{emayili}` by the
presence of an `X-Mailer` header which includes both the package name
and version.

If you want to see the complete MIME object, just convert to a string.

``` r
as.character(email)
```

### Adding an Inline Image

Adding an inline image to an HTML message is possible. First you’ll need
to [Base64 encode](https://en.wikipedia.org/wiki/Base64) the image.

``` r
img_base64 <- base64enc::base64encode("image.jpg")
```

Then create the HTML message body.

``` r
html_body <- sprintf('<html><body><img src="data:image/jpeg;base64,%s"></body></html>', img_base64)
```

And finally add it to the email.

``` r
email <- envelope() %>% html(html_body)
```

*Note:* It’s important that you specify the appropriate media type
(`image/jpeg` for JPEG and `image/png` for PNG).

### Sending a Message

Create a SMTP server object and send the message.

``` r
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "bob@gmail.com",
               password = "bd40ef6d4a9413de9c1318a65cbae5d7")
smtp(email, verbose = TRUE)
```

To see the guts of the message as passed to the SMTP server:

``` r
print(email, details = TRUE)
```

### Using STARTTLS

If you’re trying to send email with a host that uses the STARTTLS
security protocol (like Google Mail, Yahoo\! or AOL), then it will most
probably be blocked due to insufficient security. In order to circumvent
this, you can grant access to less secure apps. See the links below for
specifics:

  - [Google](https://myaccount.google.com/security)
    ([details](https://support.google.com/accounts/answer/6010255))
  - [Yahoo\!](https://login.yahoo.com/account/security) and
  - [AOL](https://login.aol.com/account/security).

## Similar Packages

There is a selection of other R packages which also send emails:

  - [blastula](https://cran.r-project.org/package=blastula)
  - [blatr](https://cran.r-project.org/package=blatr) (Windows)
  - [gmailr](https://cran.r-project.org/package=gmailr)
  - [mail](https://cran.r-project.org/package=mail)
  - [mailR](https://cran.r-project.org/package=mailR)
  - [sendmailR](https://cran.r-project.org/package=sendmailR)
  - [ponyexpress](https://github.com/ropenscilabs/ponyexpress)
