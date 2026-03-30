(git-peek--find-root)
buffer-file-name
(file-relative-name buffer-file-name (git-peek--find-root))
(expand-file-name "~/Dropbox/minorugh.com/")
(with-current-buffer "index.html"
	 (list (git-peek--find-root)
               buffer-file-name
               (file-relative-name buffer-file-name (git-peek--find-root))))
(with-current-buffer "index.html"
	 (list
	  (git-peek--find-root)
	  buffer-file-name
	  (file-truename buffer-file-name)
	  (file-relative-name (file-truename buffer-file-name) (git-peek--find-root))))
	 

	 
(buffer-list)
(with-current-buffer (get-file-buffer "~/Dropbox/minorugh.com/index.html")
	 (list
	  (git-peek--find-root)
	  buffer-file-name
	  (file-truename buffer-file-name)
	  (file-relative-name (file-truename buffer-file-name) (git-peek--find-root))))
