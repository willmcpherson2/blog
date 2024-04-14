(require 'ox)

(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-html-postamble nil
      org-html-validation-link nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />")

(setq org-export-global-macros
      '(("date" . "@@html:<div class=\"date\"><time datetime=\"$1\">$1</time></div>@@")))
