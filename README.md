rest-url-string
===============

For easy evaluation and printing of url's using REST

Use
===============

To decode URL's in-buffer, use ```rest-url-string-extract-print``` and ```rest-url-string-extract-decode-print```.

To reencode URL's, use ```rest-url-string-reencode-region-print```.

To make an http get call with the URL on the current line, call ```rest-url-string-http-get```.

If getting json responses back, I suggest looking into the json-reformat package.

Also provides functions for retrieving the base URL and query parameters. 

Install
===============

Add the path to directory which contains rest-url-string.el and require it in your .emacs
```
(add-to-list 'load-path "~/.emacs.d/manual-packages/rest-url-string")
(require 'rest-url-string)
```

License
===============

MIT
