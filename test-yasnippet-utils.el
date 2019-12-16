(ert-deftest test-my-yas-format-snippet$ ()
  (should
   (string= "<div ${1: attrs}>${2:inner}$0</div>
<div ${3:$1}>${4:$2}</div>
<div ${5:$1}>${6:$2}</div>"
            (my-yas-format-snippet "<div ${1: attrs}>${2:inner}$0</div>" 3))))