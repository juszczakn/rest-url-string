rest-url-string
===============

For easy evaluation and printing of url's using REST

Use
===============

To decode URL's in-buffer, use ```rest-url-string-extract-print``` and ```rest-url-string-extract-decode-print```.

To reencode URL's, use ```rest-url-string-reencode-region-print```.

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
