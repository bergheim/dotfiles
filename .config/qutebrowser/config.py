config.load_autoconfig()
# config.bind('t', 'set-cmd-text -s :open -t')
#
config.bind('h', 'back')
config.bind('l', 'forward')

config.bind('-', 'zoom-out', mode='normal')
config.bind('=', 'zoom-in', mode='normal')
config.bind('0', 'zoom', mode='normal')

config.bind('<Alt-h>', 'tab-prev')
config.bind('<Alt-l>', 'tab-next')
config.bind('<Alt-p>', 'tab-prev')
config.bind('<Alt-n>', 'tab-next')
config.bind('K', 'tab-prev')
config.bind('J', 'tab-next')

config.bind('<Alt-Shift-h>', 'tab-move -')
config.bind('<Alt-Shift-l>', 'tab-move +')
config.bind('<Alt-Shift-p>', 'tab-move -')
config.bind('<Alt-Shift-n>', 'tab-move +')

config.unbind('<Ctrl+tab>')
config.bind('<Ctrl+tab>', 'tab-next')
config.bind('<Ctrl+Shift+tab>', 'tab-prev')
config.bind('<Ctrl+6>', 'tab-focus last')
config.bind('<Ctrl+o>', 'tab-focus last')

config.bind('<Ctrl+u>', 'fake-key <Shift+Home> ;; fake-key <Delete>', mode='insert')
config.bind('<Ctrl+w>', 'fake-key <Ctrl-backspace>', mode='insert')

config.bind('b', 'set-cmd-text -s :buffer')

config.bind('gi', 'hint inputs')
config.bind('gn', 'set tabs.position top')
config.bind('gN', 'set tabs.position left')

# these may seem wierd, but they mimic the spacemacs setup
# ideally , would be <space> but special keys are not allowed in a keychain
config.bind(',fed', 'config-edit')
config.bind(',fer', 'config-source')

config.bind(',td', f'config-cycle content.user_stylesheets "~/.config/qutebrowser/dark.css" "" ;; reload')
config.bind(',tl', f'config-cycle content.user_stylesheets "~/.config/qutebrowser/light.css" "" ;; reload')

config.bind(',tt', f'config-cycle tabs.position "top" "left" ;; reload')

config.bind(',l', 'spawn --userscript qute-pass')

# this is not supported right now
# see https://github.com/qutebrowser/qutebrowser/issues/1044
#config.bind('jk', 'leave-mode', mode='insert')

# config.bind('<Ctrl-u>', 'rl-unix-line-discard', mode='insert')
# config.bind('<Ctrl-a>', 'rl-beginning-of-line', mode='insert')
# config.bind('<Ctrl-e>', 'rl-end-of-line', mode='insert')
# config.bind('<Ctrl-w>', 'rl-end-word-rubout', mode='insert')

config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')
config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'prompt-item-focus next', mode='prompt')
config.bind('<Ctrl-j>', 'prompt-item-focus prev', mode='prompt')

# config.unbind("<ctrl+tab>")
# config.bind("<ctrl+tab>", "tab-next")
# config.bind("<ctrl+shift+tab>", "tab-prev")

c.editor.command = ["termite", "--name", "floating", "-e", "vim '{}'"]

c.aliases['cs'] = 'config-source'
c.aliases['mpv'] = 'spawn -d mpv --force-window=immediate {url}'
# c.aliases = {
#     "w": "session-save",
#     "cs": 'config-source',
#     "wq": "quit --save",
#     "mpv": "spawn -d mpv --force-window=immediate {url}",
#     "nicehash": "spawn --userscript nicehash",
#     "pass": "spawn -d pass -c",
# }

c.colors.tabs.even.bg = '#444'
c.colors.tabs.odd.bg = '#333'

# Foreground color of selected tabs
c.colors.tabs.selected.even.fg = 'black'
c.colors.tabs.selected.odd.fg = c.colors.tabs.selected.even.fg

# Background color of selected tabs
c.colors.tabs.selected.even.bg = '#ffb52a'
c.colors.tabs.selected.odd.bg = c.colors.tabs.selected.even.bg

## Background color for hints. Note that you can use a `rgba(...)` value
## for transparency.
## Type: QssColor
# c.colors.hints.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgba(255, 255, 0, 0.8), stop:1 rgba(255, 0, 0, 0.8))'
c.colors.hints.bg = 'rgba(255, 200, 40, 0.9)'

## CSS border value for hints.
c.hints.border = '1px solid black'
c.hints.mode = 'letter'
c.hints.uppercase = True

## Font color for hints.
## Type: QssColor
# c.colors.hints.fg = 'black'

## Font color for the matched part of hints.
## Type: QssColor
# c.colors.hints.match.fg = 'green'

monospace = "16px 'DejaVu Sans Mono'"
# monospace = "16px 'Anonymous Pro'"
# monospace = "14px 'Source Code Pro'"

# Font used in the completion categories.
c.fonts.completion.category = f"bold {monospace}"

# Font used in the completion widget.
c.fonts.completion.entry = monospace

# Font used for the debugging console.
c.fonts.debug_console = monospace

# Font used for the downloadbar.
c.fonts.downloads = monospace

# Font used in the keyhint widget.
c.fonts.keyhint = monospace

# Font used for error messages.
c.fonts.messages.error = monospace

# Font used for info messages.
c.fonts.messages.info = monospace

# Font used for warning messages.
c.fonts.messages.warning = monospace

# Font used for prompts.
c.fonts.prompts = monospace

# Font used in the statusbar.
c.fonts.statusbar = monospace

# Font used in the tab bar.
c.fonts.tabs = monospace


# The height of the completion, in px or as percentage of the window.
# c.completion.height = "40%"

# Whether quitting the application requires a confirmation.
# Valid values:
#   - always: Always show a confirmation.
#   - multiple-tabs: Show a confirmation if multiple tabs are opened.
#   - downloads: Show a confirmation if downloads are running
#   - never: Never show a confirmation.
c.confirm_quit = ["downloads"]

# Validate SSL handshakes.
# Valid values:
#   - true
#   - false
#   - ask
c.content.ssl_strict = True

# A list of user stylesheet filenames to use.
c.content.user_stylesheets = "user.css"

c.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101 Firefox/52.0'

c.content.cookies.store = False

c.scrolling.smooth = False
c.scrolling.bar = True

c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "g": "https://www.google.com/search?q={}",
    "gh": "https://github.com/search?utf8=%E2%9C%93&q={}"
}

c.auto_save.session = True

## Format to use for the tab title. The following placeholders are
## defined:  * `{perc}`: Percentage as a string like `[10%]`. *
## `{perc_raw}`: Raw percentage, e.g. `10`. * `{title}`: Title of the
## current web page. * `{title_sep}`: The string ` - ` if a title is set,
## empty otherwise. * `{index}`: Index of this tab. * `{id}`: Internal
## tab ID of this tab. * `{scroll_pos}`: Page scroll position. *
## `{host}`: Host of the current web page. * `{backend}`: Either
## ''webkit'' or ''webengine'' * `{private}`: Indicates when private mode
## is enabled. * `{current_url}`: URL of the current web page. *
## `{protocol}`: Protocol (http/https/...) of the current web page.
## Type: FormatString
c.tabs.title.format = '{index}:{title}'

## Format to use for the tab title for pinned tabs. The same placeholders
## like for `tabs.title.format` are defined.
## Type: FormatString
c.tabs.title.format_pinned = '{index}'

c.tabs.show = 'always'

# Open new tabs (middleclick/ctrl+click) in the background.
c.tabs.background = True

# Padding around text for tabs
c.tabs.padding = {
    "left": 5,
    "right": 8,
    "top": 8,
    "bottom": 5,
}

# Which tab to select when the focused tab is removed.
# Valid values:
#   - prev: Select the tab which came before the closed one (left in horizontal, above in vertical).
#   - next: Select the tab which came after the closed one (right in horizontal, below in vertical).
#   - last-used: Select the previously selected tab.
c.tabs.select_on_remove = "last-used"

# Behavior when the last tab is closed.
# Valid values:
#   - ignore: Don't do anything.
#   - blank: Load a blank page.
#   - startpage: Load the start page.
#   - default-page: Load the default page.
#   - close: Close the window.
c.tabs.last_close = "close"

## Position of the tab bar.
## Type: Position
## Valid values:
##   - top
##   - bottom
##   - left
##   - right
c.tabs.position = 'top'

# only for vertical
c.tabs.width = 300
