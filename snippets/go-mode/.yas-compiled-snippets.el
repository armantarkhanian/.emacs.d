;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("wg" "type customType struct {\n	error       error\n	goroutineID int\n}\nnumGoroutines := 3\nch := make(chan customType, numGoroutines)\nfor n := 1; n <= numGoroutines; n++ {\n	go func(i int) {\n		var err error\n		defer func() {\n			ch <- customType{\n				goroutineID: i,\n				error:       err,\n			}\n		}()\n		if i%2 == 0 {\n			err = errors.New(\"invalid number\")\n		}\n	}(n)\n}\nfor n := 1; n <= numGoroutines; n++ {\n	resp := <-ch\n	if resp.error != nil {\n		fmt.Println(resp.goroutineID, \"goroutine has an error:\", resp.error)\n		continue\n	}\n	fmt.Println(resp.goroutineID, \"goroutine is done\")\n}\nfmt.Println(\"All goroutines done\")" "wg" nil nil nil "/home/arman/.emacs.d/snippets/go-mode/wg" nil nil)
                       ("newerr" "errors.New(\"$1\")" "newerr" nil nil nil "/home/arman/.emacs.d/snippets/go-mode/newerr" nil nil)
                       ("hw" "package main\n\nimport (\n    \"fmt\"\n)\n\nfunc main() {$0\n    fmt.Println(\"Hello, world\")\n}\n" "hw" nil nil nil "/home/arman/.emacs.d/snippets/go-mode/hw" nil nil)
                       ("cls" "// $1 \ntype ${1:} struct {\n  $0\n}" "struct" nil nil nil "/home/arman/.emacs.d/snippets/go-mode/cls" nil nil)))


;;; Do not edit! File generated at Tue Aug  3 10:39:57 2021
