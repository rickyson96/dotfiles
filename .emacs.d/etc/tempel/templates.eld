fundamental-mode
(lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Et harum de reud facilis est er expedit disti. Nam liber tempor cum soluta nobis elige quod maxim placeat facer possim omnis volupt assumenda est, omnis dolor repellenda.")
(pangram "The quick brown fox jumps over the lazy dog.")
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))


prog-mode
(boxes (make-string (+ 4 (length str)) (aref comment-start 0)) n
       (string-trim comment-start) " " (s str) " " (string-trim comment-start) n
       (make-string (+ 4 (length str)) (aref comment-start 0)) q)


emacs-lisp-mode

(defadvice "(define-advice " (p "fun")
  " (" (p "how?") " (" (p "&rest _") ") " (p "name") ")" n>
  "\"" (p "docstring") "\"" n>
  p ")")

(adv "(define-advice " (p "fun")
  " (" (p "how?") " (" (p "&rest _") ") " (p "name") ")" n>
  "\"" (p "docstring") "\"" n>
  p ")")

(defun "(defun " (p "name") " (" p ")\n  \"" (p "docs") "\"" n> r> ")")
(defmacro "(defmacro " (p "name") " (" p ")\n  \"" (p "docs") "\"" n> r> ")")
(au & ";;;###autoload")

(plz "(plz '" (p "post") " \"" (p "url") "\" "
  ":headers " (p "'(())") " "
  ":body (json-encode ' " q "((\"key\" . \"value\"))) "
  ":as #'json-read)")


org-mode

(verb-part "Content-Type: multipart/form-data; boundary={{(verb-boundary)}}" n
"{{(verb-part \"file\" \"file1.txt\")}}" n
"Content-Type: text/plain" n
"" n
"{{(verb-read-file "documents/file1.txt")}}" n
"{{(verb-part \"file\" \"file2.xml\")}}" n
"Content-Type: application/xml" n
"" n
"{{(verb-read-file "documents/file2.xml")}}" n
"{{(verb-part)}}^" n
)

(nsrc & "#+NAME: " p n "#+begin_src " p n r n "#+end_src"
 :post (org-edit-src-code))


markdown-mode
(details "<details><summary>" p "</summary>" n
         q n
         "</details>")


typescript-ts-mode
(f "function " (p "name") "(" p "): " (p "return") " {" n> q n "}")
(af "async " "function " (p "name") "(" p "): Promise<" (p "return") "> {" n> q n "}")
(ef "export " (t f))
(eaf "export " (t af))

(l "(" p ") => {" q "}")
(lr "(" p "): " (p "return") " => {" q "}")


typescript-ts-mode :when (string-match-p (rx (0+ nonl) ".test.ts" eos) (file-name-nondirectory (buffer-file-name)))
;; jest

(be "beforeEach(() => {" n> q n> "});")
(ba "beforeAll(() => {" n> q n> "});")
(ae "afterEach(() => {" n> q n> "});")
(aa "afterAll(() => {" n> q n> "});")
(bea "beforeEach(async () => {" n> q n> "});")
(baa "beforeAll(async () => {" n> q n> "});")
(aea "afterEach(async () => {" n> q n> "});")
(aaa "afterAll(async () => {" n> q n> "});")
(i "it('" p "', () => {" n> q n> "});")
(ia "it('" p "', async () => {" n> q n> "});")
(d "describe('" p "', () => {" n> q n> "});")
(da "describe('" p "', async () => {" n> q n> "});")

(ex "expect(" (p "expectation") ")")
(en (t ex) ".not")
(enc (t en) ".toHaveBeenCalled()" q)
(enil (t ex) ".toBeNil()" q)
(eco (t ex) ".toHaveBeenCalledOnce()" q)
(ecow (t ex) ".toHaveBeenCalledExactlyOnceWith(" q ");")
(eq (t ex) ".toEqual(" q ");")
(eb (t ex) ".toBe(" q ");")
(erj (t ex) ".toReject();" q)
(ers (t ex) ".toResolve();" q)


go-mode go-ts-mode
(ctxc "ctx context.Context")
(func "func " p "(" p ") {" n> q n "}" >)
(ifel "if " p " {" n> p n "} else {" > n> p n "}" > q)
(tst "func Test" p "(t *testing.T) { " n> q n "}")
(tstf "func (t *testing.T) { " n> q n "}")
(tsts "tests := []struct{" n> "name string" n> q n "}{}")
(tstr "for _, test := range tests {" n> "t.Run(" (p "test.name") ", func (t *testing.T) { " n> q n "})" n "}")
(tscmp "if diff := cmp.Diff(" (p "want") ", " (p "got") "); diff != \"\" {" n
       "t.Errorf(\"" (p "function") " mismatch, (-want,+got):\\n%s\"" q ", diff)" n
       "}")
(tswer "if test.wantErr != (err != nil) {" n>
	   "t.Errorf(\"wantErr is %t, but err is %v\", test.wantErr, err)" n>
	   "}")
(tsen "if err != nil {" n>
	   "t.Errorf(\"unwanted error: %v\", err)" n>
	   "}")
(bench "func Benchmark" p " (b *testing.B) { " n> q n "}")
(ex "Expect(" (p "value") ")")
(exerr "Expect(err).NotTo(HaveOccurred())" q)
(exeq "Expect(" (p "w") ").To(Equal(" (p "expectation") "))" q)
(exhs "Expect(" (p "w") ").To(HaveHTTPStatus(" (p "http.StatusOK") "))")
(exhb "Expect(" (p "w") ").To(HaveHTTPBody(" (p "body") "))")
(exhttp (t exhs) n> (t exhb))


yaml-ts-mode
(str (p "name") ":" n> "  type: string" q)


sh-mode
(echo "echo \"" q "\" >&2")


sql-mode
(ine "IF NOT EXISTS ")
(ie "IF EXISTS ")
(ct "CREATE TABLE " (t ine) (p "table_name") "(" n> q n ");")
(dt "DROP TABLE " (t ie) (p "table_name") ";" q)
