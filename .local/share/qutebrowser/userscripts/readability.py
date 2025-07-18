#!/usr/bin/env python3
#
# Executes python-readability on current page and opens the summary as new tab.
#
# Depends on the python-readability package, or its fork:
#
#   - https://github.com/buriy/python-readability
#   - https://github.com/bookieio/breadability
#
# Usage:
#   :spawn --userscript readability
#
import codecs, os

tmpfile = os.path.join(
    os.environ.get('QUTE_DATA_DIR',
                   os.path.expanduser('~/.local/share/qutebrowser')),
    'userscripts/readability.html')

if not os.path.exists(os.path.dirname(tmpfile)):
    os.makedirs(os.path.dirname(tmpfile))

# Styling for dynamic window margin scaling and line height
HEADER = """
<!DOCTYPE html>
<html>
<head>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>%s</title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <style type="text/css">
        body {
            background: #fcf7ef;
            margin: 40px auto;
            max-width: 70%;
            line-height: 1.4;
            padding: 0 10px;
        }
        h1, h2, h3 {
            line-height: 1.2;
        }
    </style>
</head>
"""

with codecs.open(os.environ['QUTE_HTML'], 'r', 'utf-8') as source:
    data = source.read()

    try:
        from breadability.readable import Article as reader
        doc = reader(data, os.environ['QUTE_URL'])
        title = doc._original_document.title
        content = HEADER % title + doc.readable + "</html>"
    except ImportError:
        from readability import Document
        doc = Document(data)
        title = doc.title()
        content = doc.summary().replace('<html>', HEADER % title)

    # add a class to make styling the page easier
    content = content.replace('<body>', '<body class="qute-readability">')

    with codecs.open(tmpfile, 'w', 'utf-8') as target:
        target.write(content.lstrip())

    with open(os.environ['QUTE_FIFO'], 'w') as fifo:
        fifo.write('open -t %s' % tmpfile)
