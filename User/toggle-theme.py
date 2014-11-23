import sublime
import sublime_plugin


class ToggleThemeCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        s = sublime.load_settings("Preferences.sublime-settings")
        options = args['options']
        if options[0]:
            current_scheme = s.get("color_scheme", options[0]['color_scheme'])
            current_theme  = s.get("theme", options[0]['theme'])
            current_option = s.get("color_scheme_option", -1)

            next_option = current_option + 1
            if next_option >= len(options):
                next_option = 0

            if options[next_option]['theme']:
                s.set('theme', options[next_option]['theme'])
            if options[next_option]['color_scheme']:
                s.set('color_scheme', options[next_option]['color_scheme'])
            s.set('color_scheme_option', next_option)

            sublime.save_settings("Preferences.sublime-settings")
