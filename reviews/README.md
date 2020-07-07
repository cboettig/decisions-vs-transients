
Encrypt to my public key:

``` r
library(gpg)
```

    ## Found GPG 2.2.19. Using keyring: /home/rstudio/.gnupg

``` r
gpg::gpg_recv("3908E1CFD28B380C")
```

    ## Searching: https://keyserver.ubuntu.com

    ##      found   imported    secrets signatures    revoked 
    ##          1          0          0          0          0

``` r
msg <- gpg_encrypt("review-reply.Rmd", receiver =  "3908E1CFD28B380C")
writeLines(msg, "review-reply.Rmd.gpg")
```

## decrypt

(decrypt locally offline where my private key is available from yubikey,
`gpg -d review-reply.Rmd.gpg`)
