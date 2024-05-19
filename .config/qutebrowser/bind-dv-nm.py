c.bindings.default = {}
c.input.forward_unbound_keys = 'all'
c.input.match_counts = False

c.bindings.commands['normal'] = {
    '<alt-n>': 'tab-next',
    '<alt-p>': 'tab-prev',
    '<alt-shift-n>': 'tab-move +',
    '<alt-shift-p>': 'tab-move -',

    '<alt-h>': 'back',
    '<alt-l>': 'forward',
    '<alt-t>': 'cmd-set-text -s :tab-focus',
    '<ctrl-k>': 'tab-close',
    '<ctrl-u>': 'undo',
    '<ctrl-shift-u>': 'cmd-set-text -s :undo',
    '<alt-x>': 'cmd-set-text :',
    '<ctrl-g>': 'clear-keychain ;; search ;; fullscreen --leave',
    '<escape>': 'clear-keychain ;; search ;; fullscreen --leave ;; fake-key <escape>',

    # scrolling
    '<alt-.>': 'scroll-to-perc',
    '<alt-,>': 'scroll-to-perc 0',
    '<ctrl-v>': 'scroll-page 0 0.5',
    '<alt-v>': 'scroll-page 0 -0.5',
    '<ctrl-h>': 'scroll-page -0.5 0',
    '<ctrl-n>': 'scroll-page 0.5 0',
    '<ctrl-t>': 'scroll-page 0 0.5',
    '<ctrl-c>': 'scroll-page 0 -0.5',

    # reload
    '<ctrl-r>': 'reload',
    '<alt-r>': 'reload',
    '<alt-shift-r>': 'reload -f',

    # open
    '<ctrl-o>': 'cmd-set-text -s :open',
    '<ctrl-shift-o>': 'cmd-set-text -s :open -t',
    '<Alt-o>o': 'cmd-set-text -s :open',
    '<Alt-o>t': 'cmd-set-text -s :open -t',
    '<alt-o>b': 'cmd-set-text -s :open -b',
    '<Alt-o>l': 'cmd-set-text :open {url:pretty}',
    '<Alt-o>L': 'cmd-set-text :open -t -r {url:pretty}',
    '<Alt-o>p': 'cmd-set-text -s :open -p',
    '<alt-o>g': 'tab-give',
    '<alt-o>G': 'cmd-set-text -s :tab-give',
    '<alt-o>n': 'open -w',
    '<alt-o>w': 'cmd-set-text -s :open -w',
    '<alt-o>c': 'tab-clone',

    # hint
    '<ctrl-e>': 'hint',
    '<ctrl-shift-e>': 'hint all tab',
    '<alt-e><tab>': 'hint inputs --first',
    '<alt-e>i': 'hint inputs',
    '<alt-e>y': 'hint links yank',
    '<alt-e>r': 'hint --rapid links tab-bg',
    '<alt-e>t': 'hint all tab',
    '<alt-e>e': 'hint',

    # toggle
    '<alt-c>t': 'config-cycle -t tabs.show switching always',
    '<alt-c>w': 'devtools',
    '<alt-c><tab>': 'devtools-focus',
    '<alt-c>j': 'config-cycle -p -t -u *://*.{url:host}/* content.javascript.enabled ;; reload',
    '<alt-c>i': 'config-cycle -p -t -u *://*.{url:host}/* content.images ;; reload',
    '<alt-c>p': 'config-cycle -p -t -u *://*.{url:host}/* content.plugins ;; reload',

    # search
    '<ctrl-s>': 'cmd-set-text /',
    '<alt-s>s': 'cmd-set-text /{primary}',
    '<alt-s>y': 'cmd-set-text /{clipboard}',

    # yank
    '<ctrl-w>': 'yank selection',
    '<alt-w>w': 'yank',
    '<alt-w>s': 'yank selection',
    '<alt-w>p': 'yank pretty-url',
    '<alt-y>y': 'open -- {clipboard}',
    '<alt-y>t': 'open -t -- {clipboard}',
    '<alt-y>s': 'open -t -- {primary}',
    '<ctrl-y>': 'insert-text {clipboard}',

    # zoom
    '<ctrl-=>': 'zoom-in',
    '<ctrl-->': 'zoom-out',
    '<ctrl-0>': 'zoom',

    # userscript
    "<alt-u>pp": "spawn -u qute-pass -m -d 'fuzzel -d'",
    "<alt-u>pP": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered",
    "<alt-u>pu": "spawn -u qute-pass -m -d 'fuzzel -d' --username-only",
    "<alt-u>pU": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered --username-only",
    "<alt-u>pw": "spawn -u qute-pass -m -d 'fuzzel -d' --password-only",
    "<alt-u>pW": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered --password-only",
    "<alt-u>po": "spawn -u qute-pass -m -d 'fuzzel -d' --otp-only",
    "<alt-u>pO": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered --otp-only",
    '<alt-u>m': 'spawn umpv {url}',
    '<alt-u>M': 'hint links spanw umpv {hint-url}',

    # modes
    '<ctrl-i>': 'mode-enter insert',
    '<alt-m>i': 'mode-enter insert',
    '<alt-m>p': 'mode-enter passthrough'
}

c.bindings.commands['command'] = {
    '<ctrl-s>': 'search-next',
    '<ctrl-r>': 'search-prev',
    '<alt-s>s': 'cmd-set-text /{primary}',
    '<alt-s>y': 'cmd-set-text /{clipboard}',

    '<alt-x>': 'cmd-set-text :',

    '<ctrl-p>': 'completion-item-focus prev',
    '<ctrl-n>': 'completion-item-focus next',
    '<tab>': 'completion-item-focus next',
    '<shift-tab>': 'completion-item-focus prev',

    '<alt-p>': 'command-history-prev',
    '<alt-n>': 'command-history-next',

    '<ctrl-g>': 'mode-leave',
    '<escape>': 'mode-leave',
    '<return>': 'command-accept',
    '<Left>': 'rl-backward-char',
    '<Right>': 'rl-forward-char',
    '<Up>': 'completion-item-focus prev',
    '<Down>': 'completion-item-focus next',

    '<ctrl-f>': 'rl-forward-char',
    '<ctrl-b>': 'rl-backward-char',
    '<ctrl-a>': 'rl-beginning-of-line',
    '<ctrl-e>': 'rl-end-of-line',
    '<alt-f>': 'rl-forward-word',
    '<alt-b>': 'rl-backward-word',
    '<ctrl-d>': 'rl-delete-char',
    '<alt-d>': 'rl-kill-word',
    '<alt-backspace>': 'rl-backward-kill-word',
    '<ctrl-w>': 'rl-backward-kill-word',
    '<ctrl-k>': 'rl-kill-line',
    '<ctrl-y>': 'cmd-set-text -a {clipboard}',
}

c.bindings.commands['insert'] = {
    '<Left>': 'fake-key <Left>',
    '<Right>': 'fake-key <Right>',
    '<Up>': 'fake-key <Up>',
    '<Down>': 'fake-key <Down>',
    '<ctrl-f>': 'fake-key <Right>',
    '<ctrl-b>': 'fake-key <Left>',
    '<ctrl-a>': 'fake-key <Home>',
    '<ctrl-e>': 'fake-key <End>',
    '<ctrl-n>': 'fake-key <Down>',
    '<ctrl-p>': 'fake-key <Up>',
    '<alt-f>': 'fake-key <Ctrl-Right>',
    '<alt-b>': 'fake-key <Ctrl-Left>',
    '<ctrl-d>': 'fake-key <Delete>',
    '<alt-d>': 'fake-key <Ctrl-Delete>',
    '<alt-backspace>': 'fake-key <Ctrl-Backspace>',
    '<ctrl-w>': 'fake-key <Ctrl-backspace>',
    '<ctrl-y>': 'insert-text {clipboard}',
    '<ctrl-v>': 'fake-key <PgDown>',
    '<alt-v>': 'fake-key <PgUp>',
    '<ctrl-k>': 'fake-key <Shift-End> ;; fake-key <ctrl-x>',
    '<alt-h>': 'fake-key <ctrl-a>',
    '<ctrl-/>': 'fake-key <ctrl-z>',
    '<ctrl-shift-?>': 'fake-key <ctrl-shift-z>',
    '<alt-w>': 'fake-key <ctrl-c>',
    '<ctrl-g>': 'mode-leave',
    '<escape>': 'mode-leave',
}

c.bindings.commands['hint'] = {
    '<ctrl-r>': 'hint --rapid links tab-bg',
    '<ctrl-o>': 'hint',
    '<ctrl-w>': 'hint links yank',
    '<ctrl-g>': 'mode-leave',
    '<escape>': 'mode-leave',
}

c.bindings.commands['caret'] = {
    '<ctrl-g>': 'mode-leave',
    '<escape>': 'mode-leave',
}

c.bindings.commands['yesno'] = {
    'y': 'prompt-accept yes',
    'n': 'prompt-accept no',
    'Y': 'prompt-accept --save yes',
    'N': 'prompt-accept --save no',
    '<escape>': 'mode-leave',
    '<alt-w>': 'prompt-yank',
    'w': 'prompt-yank',
    '<alt-g>': 'mode-leave',
}

c.bindings.commands['prompt'] = {
    '<return>': 'prompt-accept',
    '<escape>': 'mode-leave',
    '<ctrl-w>': 'rl-filename-rubout',
    '<ctrl-y>': 'rl-yank',
    '<ctrl-delete>': 'rl-rubout "/"',
    '<ctrl-x>': 'prompt-open-download',
    '<ctrl-r>': 'prompt-open-download --pdfjs',
    '<ctrl-y>': 'prompt-yank',
    '<ctrl-o>': 'prompt-fileselect-external',
    '<ctrl-g>': 'mode-leave',

    '<ctrl-f>': 'rl-forward-char',
    '<ctrl-b>': 'rl-backward-char',
    '<ctrl-a>': 'rl-beginning-of-line',
    '<ctrl-e>': 'rl-end-of-line',
    '<alt-f>': 'rl-forward-word',
    '<alt-b>': 'rl-backward-word',
    '<ctrl-d>': 'rl-delete-char',
    '<alt-d>': 'rl-kill-word',
    '<alt-backspace>': 'rl-backward-kill-word',
    '<ctrl-k>': 'rl-kill-line',
    '<ctrl-p>': 'prompt-item-focus prev',
    '<ctrl-n>': 'prompt-item-focus next',
    '<tab>': 'prompt-item-focus next',
    '<shift-tab>': 'prompt-item-focus prev',
}

c.bindings.commands['passthrough'] = {
    '<shift-escape>': 'mode-leave',
}
