import sublime, sublime_plugin

class ToggleColorSchemeCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):

        scheme1 = args["color_scheme_1"]
        scheme2 = args["color_scheme_2"]
        current_scheme = self.view.settings().get("color_scheme")

        new_scheme = scheme1 if current_scheme == scheme2 else scheme2
        self.view.settings().set("color_scheme", new_scheme)