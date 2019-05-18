## Install

At the moment you need to install a specific branch of the `{curl}` package.

```
devtools::install_github("jeroen/curl", ref = "smtp")
```

Then install `{imeyili}`.

```
devtools::install_github("datawookie/imeyili")
```






https://ec.haxx.se/usingcurl-smtp.html (REFERENCE)
https://curl.haxx.se/libcurl/c/smtp-tls.html

$ export PASSWORD="my_secret_password"
$ curl smtps://smtp.gmail.com:465 -v --mail-from "andrew.b.collier@gmail.com" --mail-rcpt "andrew@exegetic.biz" --mail-rcpt "collierab@gmail.com" --ssl --ssl-reqd -u andrew.b.collier@gmail.com:$PASSWORD -T mail.txt --anyauth

$ curl smtp://smtp.gmail.com:587 -v --mail-from "andrew.b.collier@gmail.com" --mail-rcpt "andrew@exegetic.biz" --ssl -u andrew.b.collier@gmail.com:$PASSWORD -T mail.txt -k --anyauth

The contents of the `mail.txt` file are:

From: "User Name" <andrew.b.collier@gmail.com>
To: "John Smith" <andrew@exegetic.biz>
Cc: "John Smith" <collierab@gmail.com>
Subject: This is a test

Hi John,
I’m sending this mail with curl thru my gmail account.
Bye!

## Similar Packages

- [blastula](https://cran.r-project.org/web/packages/blastula/index.html)
- [blatr](https://cran.r-project.org/web/packages/blatr/index.html) (Windows)
- [gmailr](https://cran.r-project.org/web/packages/gmailr/index.html)
- [mail](https://cran.r-project.org/web/packages/mail/index.html)
- [mailR](https://cran.r-project.org/web/packages/mailR/index.html)
- [sendmailR](https://cran.r-project.org/web/packages/sendmailR/index.html)

## TODO

- tests
- CI/Travis
- multipart emails (base 64 encode images)




