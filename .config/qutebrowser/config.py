# enable prompt based configuration
# such as: google-meet always accept video and audio request
# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config: ConfigAPI = config  # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig(True)

c.tabs.position = "left"

c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "w": "https://en.wikipedia.org/wiki/Special:Search?search={}&go=Go&ns0=1",
    "n": "https://search.nixos.org/packages?channel=unstable&query={}",
    "no": "https://search.nixos.org/options?channel=23.05&query={}",
    "nw": "https://nixos.wiki/index.php?search={}",
    "hm": "https://mipmip.github.io/home-manager-option-search/?query={}",
    "g": "https://www.google.com/search?hl=en&q={}",
    "d": "https://duckduckgo.com/?q={}",
    "akops": "https://gamepress.gg/arknights/search?query={}",
    "gmeet": "https://meet.google.com/{}",
    "gmeetbit": "https://meet.google.com/{}?authuser=1",
    "genius": "https://genius.com/search?q={}",
    "shopee": "https://shopee.co.id/search?keyword={}"
}

config.unbind("co")

# c.input.insert_mode.auto_load = True
c.input.insert_mode.plugins = True

# c.input.insert_mode.auto_enter = False
# c.input.insert_mode.auto_leave = False
# c.input.insert_mode.plugins = False
#
# c.input.forward_unbound_keys = "all"
#
# c.input.match_counts = False
#
# c.bindings.default['normal'] = {}

normal_bindings = {
    "J": "tab-next",
    "K": "tab-prev",
    "D": "tab-close",
    "U": "undo",
    "d": "scroll-page 0 0.5",
    "u": "scroll-page 0 -0.5",
    "tt": "config-cycle -t tabs.show switching always",
    "tp": "config-cycle -t -p content.proxy system http://192.168.195.167:8181",
    "<ctrl+d>": "scroll-page 0 0.25",
    "<ctrl+u>": "scroll-page 0 -0.25",
    "zll": "spawn -u qute-pass -m -d 'fuzzel -d'",
    "zlL": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered",
    "zlu": "spawn -u qute-pass -m -d 'fuzzel -d' --username-only",
    "zlU": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered --username-only",
    "zlp": "spawn -u qute-pass -m -d 'fuzzel -d' --password-only",
    "zlP": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered --password-only",
    "zlo": "spawn -u qute-pass -m -d 'fuzzel -d' --otp-only",
    "zlO": "spawn -u qute-pass -m -d 'fuzzel -d' --unfiltered --otp-only",
    "zm": "spawn umpv {url}",
    "zM": "hint links spawn umpv {hint-url}",
    "<alt-shift-.>": 'scroll-to-perc'
}
for k,v in normal_bindings.items():
    config.bind(k, v)

config.source('bind-dvorak.py')

command_bindings = {
    "<Alt+n>": "command-history-next",
    "<Alt+p>": "command-history-prev",
    "<Ctrl+n>": "completion-item-focus next",
    "<Ctrl+p>": "completion-item-focus prev"
}
for k,v in command_bindings.items():
    config.bind(k, v, mode="command")

c.tabs.padding = {
    "bottom": 5,
    "top": 5,
    "left": 5,
    "right": 5
}

# Themes
white = "#fcf7ef"
white_alt = "#f0ece0"
white_dim = "#c5c3b8"
black_dim = "#59786f"
black = "#242521"
red = "#9f0d0f"
orange = "#bf4400"
yellow = "#a7601f"
green = "#006f00"
cyan = "#1f70af"
blue = "#375cc6"
violet = "#8448aa"
magenta = "#9a456f"

c.colors.completion.fg = black
c.colors.completion.odd.bg = white
c.colors.completion.even.bg = white
c.colors.completion.category.fg = blue
c.colors.completion.category.bg = white
c.colors.completion.category.border.top = white
c.colors.completion.category.border.bottom = white
c.colors.completion.item.selected.fg = black
c.colors.completion.item.selected.bg = white_alt
c.colors.completion.item.selected.border.top = white_alt
c.colors.completion.item.selected.border.bottom = white_alt
c.colors.completion.item.selected.match.fg = cyan
c.colors.completion.match.fg = cyan
c.colors.completion.scrollbar.fg = black
c.colors.completion.scrollbar.bg = white
c.colors.contextmenu.disabled.bg = white_alt
c.colors.contextmenu.disabled.fg = black_dim
c.colors.contextmenu.menu.bg = white
c.colors.contextmenu.menu.fg = black
c.colors.contextmenu.selected.bg = white_dim
c.colors.contextmenu.selected.fg = black
c.colors.downloads.bar.bg = white
c.colors.downloads.start.fg = white
c.colors.downloads.start.bg = blue
c.colors.downloads.stop.fg = white
c.colors.downloads.stop.bg = cyan
c.colors.downloads.error.fg = red
c.colors.hints.fg = white
c.colors.hints.bg = yellow
c.colors.hints.match.fg = black
c.colors.keyhint.fg = black
c.colors.keyhint.suffix.fg = black
c.colors.keyhint.bg = white
c.colors.messages.error.fg = white
c.colors.messages.error.bg = red
c.colors.messages.error.border = red
c.colors.messages.warning.fg = white
c.colors.messages.warning.bg = violet
c.colors.messages.warning.border = violet
c.colors.messages.info.fg = black
c.colors.messages.info.bg = white
c.colors.messages.info.border = white
c.colors.prompts.fg = black
c.colors.prompts.border = white
c.colors.prompts.bg = white
c.colors.prompts.selected.bg = white_dim
c.colors.prompts.selected.fg = black
c.colors.statusbar.normal.fg = green
c.colors.statusbar.normal.bg = white
c.colors.statusbar.insert.fg = white
c.colors.statusbar.insert.bg = blue
c.colors.statusbar.passthrough.fg = white
c.colors.statusbar.passthrough.bg = cyan
c.colors.statusbar.private.fg = white
c.colors.statusbar.private.bg = white_alt
c.colors.statusbar.command.fg = black_dim
c.colors.statusbar.command.bg = white_alt
c.colors.statusbar.command.private.fg = violet
c.colors.statusbar.command.private.bg = white_alt
c.colors.statusbar.caret.fg = white
c.colors.statusbar.caret.bg = violet
c.colors.statusbar.caret.selection.fg = white
c.colors.statusbar.caret.selection.bg = blue
c.colors.statusbar.progress.bg = blue
c.colors.statusbar.url.fg = black
c.colors.statusbar.url.error.fg = red
c.colors.statusbar.url.hover.fg = black
c.colors.statusbar.url.success.http.fg = magenta
c.colors.statusbar.url.success.https.fg = green
c.colors.statusbar.url.warn.fg = red
c.colors.tabs.bar.bg = white
c.colors.tabs.indicator.start = blue
c.colors.tabs.indicator.stop = cyan
c.colors.tabs.indicator.error = red
c.colors.tabs.odd.fg = black
c.colors.tabs.odd.bg = white
c.colors.tabs.even.fg = black
c.colors.tabs.even.bg = white
c.colors.tabs.pinned.even.bg = green
c.colors.tabs.pinned.even.fg = white
c.colors.tabs.pinned.odd.bg = green
c.colors.tabs.pinned.odd.fg = white
c.colors.tabs.pinned.selected.even.bg = white_dim
c.colors.tabs.pinned.selected.even.fg = black
c.colors.tabs.pinned.selected.odd.bg = white_dim
c.colors.tabs.pinned.selected.odd.fg = black
c.colors.tabs.selected.odd.fg = black
c.colors.tabs.selected.odd.bg = white_alt
c.colors.tabs.selected.even.fg = black
c.colors.tabs.selected.even.bg = white_alt
c.colors.webpage.bg = "#FFFFFF"


# workaround for https://github.com/qutebrowser/qutebrowser/issues/7489
c.qt.chromium.low_end_device_mode = "never";

# c.qt.force_software_rendering = "chromium"
c.qt.highdpi = True

c.auto_save.session = True

c.completion.shrink = True
c.scrolling.smooth = True

c.content.javascript.clipboard = "access"
c.content.blocking.method = "both"

c.fonts.default_family = "IosevraRelaxedNerdFont"
c.fonts.default_size = "11pt"

c.fonts.web.family.standard = "IosevraAileRelaxedNerdFont"
c.fonts.web.family.fixed = "IosevraRelaxedNerdFont"

c.fonts.tabs.selected = "default_size default_family"
c.fonts.tabs.unselected = "default_size default_family"

c.tabs.title.format = "{audio}{current_title}"
c.tabs.title.format_pinned = "{audio}{current_title}"
c.tabs.show = "switching"
c.tabs.position = "left"
c.tabs.favicons.scale = 1.0
