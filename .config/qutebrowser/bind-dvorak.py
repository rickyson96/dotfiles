normal_bindings = {
    'N': 'tab-next',
    'P': 'tab-prev',
    '<alt-n>': 'tab-next',
    '<alt-p>': 'tab-prev',
    'n': 'scroll down',
    'p': 'scroll up',
    'h': 'scroll left',
    't': 'scroll right',
    'H': 'back',
    '<Alt-Shift-H>': 'back -t',
    'T': 'forward',
    '<Alt-Shift-T>': 'forward -t',
    '<alt-.>': 'scroll-to-perc',
    '<alt-,>': 'scroll-to-perc 0',
    'v': 'scroll-page 0 0.5',
    'V': 'scroll-page 0 -0.5',
    '<ctrl-v>': 'scroll-page 0 0.5',
    '<alt-v>': 'scroll-page 0 -0.5',
    'K': 'tab-close',

    # reload
    'r': 'reload',
    'R': 'reload -f',
    '<alt-r>': 'reload',
    '<alt-shift-r>': 'reload -f',

    # open
    'o': 'cmd-set-text -s :open',
    'O': 'cmd-set-text -s :open -t',
    '<ctrl-o>': 'cmd-set-text -s :open',
    '<ctrl-O>': 'cmd-set-text -s :open -t',
    '<Alt-o>o': 'cmd-set-text -s :open -b',
    '<Alt-o>O': 'cmd-set-text -s :open -b',
    '<Alt-o>l': 'cmd-set-text :open {url:pretty}',
    '<Alt-o>L': 'cmd-set-text :open -t -r {url:pretty}',
    '<Alt-o>p': 'cmd-set-text -s :open -p',

    # hint
    'e': 'hint',
    'E': 'hint all tab',
    '<ctrl-e>': 'hint inputs --first',
    '<alt-e>i': 'hint inputs',
    '<alt-e>y': 'hint links yank',
    '<alt-e>r': 'hint --rapid links tab-bg',

    # toggle
    '<alt-t><alt-t>': 'config-cycle -t tabs.show switching always',

    # search
    '/': 'cmd-set-text /',
    's': 'search-next',
    'S': 'search-prev',

    # yank
    '<alt-w>': 'yank',
    '<alt-shift-w>p': 'yank pretty-url',
    '<ctrl-y>': 'open -- {clipboard}',
    '<alt-y>t': 'open -t -- {clipboard}'
}

for k,v in normal_bindings.items():
    config.bind(k, v)
