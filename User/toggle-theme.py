import sublime, sublime_plugin

class ToggleThemeCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):

        scheme1 = args["color_scheme_1"]
        scheme2 = args["color_scheme_2"]
        theme1  = args["theme_1"]
        theme2  = args["theme_2"]
        current_scheme = self.view.settings().get("color_scheme")
        current_theme  = self.view.settings().get("theme")

        new_scheme = scheme1 if current_scheme == scheme2 else scheme2
        self.view.settings().set("color_scheme", new_scheme)
        new_theme  = theme1 if current_theme  == theme2 else theme2
        self.view.settings().set("theme", new_theme)