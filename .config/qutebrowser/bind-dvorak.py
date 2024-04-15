normal_bindings = {
    'N': 'tab-next',
    'P': 'tab-prev',
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
    'k': 'tab-close',

    # open
    'o': 'cmd-set-text -s :open',
    'O': 'cmd-set-text -s :open -t',
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
    '<alt-e>y': 'hint links yank'
}

for k,v in normal_bindings.items():
    config.bind(k, v)
