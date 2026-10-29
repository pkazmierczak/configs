#!/usr/bin/env python3
# ~/.config/waybar/scripts/calendar-popup.py
"""Calendar dropdown for the waybar clock (GTK4 + gtk4-layer-shell + EDS).

    calendar-popup.py          toggle the dropdown
    calendar-popup.py --quit   stop the resident instance (e.g. after editing)

The first run stays resident; later runs reach it over D-Bus (GApplication)
and toggle it, so reopening is instant. Events come from Evolution Data
Server, i.e. the calendars GNOME Calendar/Evolution show, including GNOME
Online Accounts ones. Colours follow ~/.cache/theme-mode (theme-toggle.sh).

Keys: Esc close, Home today, PgUp/PgDn month, Enter open GNOME Calendar.
Clicking anywhere outside the dropdown (including the clock) closes it.

Needs: python3-gobject, gtk4-layer-shell, evolution-data-server (optional;
without it the event list is replaced by a hint).
"""

import ctypes
import ctypes.util
import datetime as dt
import re
import shutil
import string
import sys
import threading
import time
from dataclasses import dataclass
from pathlib import Path


def _preload_layer_shell():
    # gtk4-layer-shell must be linked before libwayland-client, i.e. before Gtk
    # is imported, or the window silently becomes a normal toplevel.
    for name in (ctypes.util.find_library("gtk4-layer-shell"),
                 "libgtk4-layer-shell.so.0", "libgtk4-layer-shell.so"):
        if name:
            try:
                ctypes.CDLL(name)
                return
            except OSError:
                pass


_preload_layer_shell()

import gi  # noqa: E402

gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")
from gi.repository import Gdk, Gio, GLib, Gtk, Pango  # noqa: E402
from gi.repository import Gtk4LayerShell as LayerShell  # noqa: E402

try:
    gi.require_version("ECal", "2.0")
    gi.require_version("EDataServer", "1.2")
    gi.require_version("ICalGLib", "3.0")
    from gi.repository import ECal, EDataServer, ICalGLib  # noqa: E402
    HAVE_EDS = True
except (ImportError, ValueError):
    HAVE_EDS = False

APP_ID = "local.waybar.CalendarPopup"
THEME_FILE = Path.home() / ".cache" / "theme-mode"
# Seconds each calendar backend may take to report "connected" before its
# cache is read. 0 would mean "wait forever" to EDS.
CONNECT_WAIT_S = 1
DEFAULT_COLOR = "#268bd2"
COLOR_RE = re.compile(r"^#[0-9a-fA-F]{6}$")

# Solarized, matching waybar/style-{light,dark}.css
PALETTES = {
    "dark": {"bg": "#002b36", "surface": "#073642", "fg": "#839496",
             "strong": "#93a1a1", "dim": "#586e75", "accent": "#268bd2"},
    "light": {"bg": "#fdf6e3", "surface": "#eee8d5", "fg": "#657b83",
              "strong": "#586e75", "dim": "#93a1a1", "accent": "#268bd2"},
}

CSS = string.Template("""
window.calendar-backdrop { background: transparent; }
window.calendar-popup {
    background-color: $bg;
    color: $fg;
    border: 2px solid $surface;
    border-radius: 10px;
    font-family: "JetBrainsMono Nerd Font", sans-serif;
    font-size: 13px;
}
.weekday { color: $dim; font-weight: bold; }
.date { color: $strong; font-size: 18px; font-weight: bold; }
.section { color: $accent; font-weight: bold; }
.event-time { color: $dim; font-size: 12px; }
.event-title { color: $strong; }
.event-loc { color: $dim; font-size: 11px; }
.empty { color: $dim; font-style: italic; }
separator { background-color: $surface; min-height: 1px; }
scrolledwindow, viewport { background: transparent; }

calendar { background: transparent; border: none; color: $fg; padding: 0; }
calendar > header { background: transparent; border: none; }
calendar > header > button {
    background: transparent; border: none; box-shadow: none; color: $fg;
}
calendar > header > button:hover { color: $accent; }
calendar > grid > label.day-name,
calendar > grid > label.week-number { color: $dim; }
calendar > grid > label.day-number { border-radius: 6px; }
calendar > grid > label.other-month { color: $dim; opacity: 0.5; }
calendar > grid > label:checked { background-color: $surface; }
calendar > grid > label.today { color: $accent; font-weight: bold; }
calendar > grid > label:selected { background-color: $accent; color: $bg; }

button.pill {
    background: $surface; color: $fg; border: none; box-shadow: none;
    border-radius: 6px; padding: 2px 12px; min-height: 0;
}
button.pill:hover { color: $accent; }
""")


def theme_mode():
    try:
        mode = THEME_FILE.read_text().strip()
    except OSError:
        mode = ""
    return mode if mode in PALETTES else "light"


@dataclass
class Event:
    summary: str
    location: str
    start: dt.datetime  # local, naive; all-day events start at midnight
    end: dt.datetime    # exclusive
    all_day: bool
    color: str

    def days(self):
        day = self.start.date()
        last = (self.end - dt.timedelta(microseconds=1)).date() \
            if self.end > self.start else day
        while day <= last:
            yield day
            day += dt.timedelta(days=1)


class EventStore:
    """Reads events from all enabled, visible EDS calendars.

    Blocking; call from one worker thread at a time.
    """

    def __init__(self):
        self._registry = None
        self._tz = None
        self._clients = {}  # source uid -> ECal.Client

    def _setup(self):
        if self._registry is None:
            self._registry = EDataServer.SourceRegistry.new_sync(None)
            self._tz = (ECal.util_get_system_timezone()
                        or ICalGLib.Timezone.get_utc_timezone())

    def _calendars(self):
        for source in self._registry.list_enabled(EDataServer.SOURCE_EXTENSION_CALENDAR):
            ext = source.get_extension(EDataServer.SOURCE_EXTENSION_CALENDAR)
            # Same visibility toggle GNOME Calendar and Evolution use.
            if ext.get_selected():
                color = ext.get_color() or ""
                yield source, color if COLOR_RE.match(color) else DEFAULT_COLOR

    def _client(self, source):
        uid = source.get_uid()
        if uid not in self._clients:
            try:
                client = ECal.Client.connect_sync(
                    source, ECal.ClientSourceType.EVENTS, CONNECT_WAIT_S, None)
            except GLib.Error as err:
                print(f"calendar {source.get_display_name()!r}: {err.message}",
                      file=sys.stderr)
                return None  # retried on the next fetch
            client.set_default_timezone(self._tz)
            self._clients[uid] = client
        return self._clients[uid]

    def _to_local(self, itime):
        if itime is None or itime.is_null_time():
            return None
        if itime.is_date():
            return dt.datetime(itime.get_year(), itime.get_month(), itime.get_day())
        # Floating times have no zone and are meant as local time.
        zone = itime.get_timezone() or self._tz
        return dt.datetime.fromtimestamp(itime.as_timet_with_zone(zone))

    def fetch(self, start, end):
        """Events overlapping [start, end) dates, recurrences expanded."""
        self._setup()
        t0 = int(time.mktime(start.timetuple()))
        t1 = int(time.mktime(end.timetuple()))
        events = []
        for source, color in self._calendars():
            client = self._client(source)
            if client is None:
                continue

            def on_instance(comp, istart, iend, *_args, color=color):
                if comp.get_status() == ICalGLib.PropertyStatus.CANCELLED:
                    return True
                begin = self._to_local(istart)
                if begin is None:
                    return True
                all_day = bool(istart.is_date())
                finish = self._to_local(iend)
                if finish is None or finish < begin:
                    finish = begin + dt.timedelta(days=1) if all_day else begin
                events.append(Event(
                    summary=comp.get_summary() or "(untitled)",
                    location=comp.get_location() or "",
                    start=begin, end=finish, all_day=all_day, color=color))
                return True

            try:
                client.generate_instances_sync(t0, t1, None, on_instance, None)
            except GLib.Error as err:
                print(f"calendar {source.get_display_name()!r}: {err.message}",
                      file=sys.stderr)
        events.sort(key=lambda e: (not e.all_day, e.start, e.summary.casefold()))
        return events


class CalendarPopup(Gtk.Window):
    def __init__(self, app):
        super().__init__(application=app, title="Calendar", resizable=False)
        self.add_css_class("calendar-popup")
        self._css = Gtk.CssProvider()
        Gtk.StyleContext.add_provider_for_display(
            Gdk.Display.get_default(), self._css,
            Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
        self._init_layer_shell()

        self._store = EventStore() if HAVE_EDS else None
        self._fetch_lock = threading.Lock()
        self._generation = 0
        self._reload_pending = False
        self._events_by_day = {}
        self._error = None
        self._loading = False
        self._backdrops = []

        root = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=8,
                       margin_top=14, margin_bottom=14,
                       margin_start=14, margin_end=14)
        root.set_size_request(320, -1)
        self.set_child(root)

        self._weekday = Gtk.Label(xalign=0, css_classes=["weekday"])
        self._date = Gtk.Label(xalign=0, css_classes=["date"])
        root.append(self._weekday)
        root.append(self._date)

        self._calendar = Gtk.Calendar(show_week_numbers=True)
        self._calendar.connect("notify::month", self._schedule_reload)
        self._calendar.connect("notify::year", self._schedule_reload)
        self._calendar.connect("day-selected", lambda *_: self._render_day())
        root.append(self._calendar)

        root.append(Gtk.Separator())
        self._day_title = Gtk.Label(xalign=0, css_classes=["section"])
        root.append(self._day_title)

        self._events_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)
        scroller = Gtk.ScrolledWindow(
            hscrollbar_policy=Gtk.PolicyType.NEVER,
            vscrollbar_policy=Gtk.PolicyType.AUTOMATIC,
            propagate_natural_height=True, max_content_height=240)
        scroller.set_child(self._events_box)
        root.append(scroller)

        buttons = Gtk.Box(spacing=6, halign=Gtk.Align.END)
        actions = [("Today", self.go_today)]
        if shutil.which("gnome-calendar"):
            actions.append(("Open Calendar", self.open_gnome_calendar))
        for label, callback in actions:
            button = Gtk.Button(label=label, css_classes=["pill"], focus_on_click=False)
            button.connect("clicked", lambda _b, cb=callback: cb())
            buttons.append(button)
        root.append(buttons)

        keys = Gtk.EventControllerKey()
        keys.connect("key-pressed", self._on_key)
        self.add_controller(keys)
        self.connect("close-request", self._on_close_request)

    def _init_layer_shell(self):
        if not LayerShell.is_supported():
            return
        LayerShell.init_for_window(self)
        LayerShell.set_namespace(self, "waybar-calendar")
        # Overlay keeps it above the backdrops, which live on the top layer.
        LayerShell.set_layer(self, LayerShell.Layer.OVERLAY)
        # Only the top edge is anchored, so it's centred under the clock;
        # the default exclusive zone of 0 keeps it below waybar.
        LayerShell.set_anchor(self, LayerShell.Edge.TOP, True)
        LayerShell.set_margin(self, LayerShell.Edge.TOP, 4)
        # Grab the keyboard while open, so focus_follows_mouse can't steal it
        # and Esc always works; outside clicks are caught by the backdrops.
        LayerShell.set_keyboard_mode(self, LayerShell.KeyboardMode.EXCLUSIVE)

    # --- visibility -------------------------------------------------------

    def toggle(self):
        if self.get_visible():
            self.dismiss()
        else:
            self.open()

    def open(self):
        self._css.load_from_string(CSS.substitute(PALETTES[theme_mode()]))
        now = dt.datetime.now()
        self._weekday.set_text(now.strftime("%A"))
        self._date.set_text(now.strftime("%-d %B %Y"))
        self.go_today()
        self._schedule_reload()  # always refresh, events may have changed
        if LayerShell.is_supported():
            monitors = Gdk.Display.get_default().get_monitors()
            self._backdrops = [
                Backdrop(self.get_application(), monitors.get_item(i), self.dismiss)
                for i in range(monitors.get_n_items())]
        self.present()

    def dismiss(self):
        for backdrop in self._backdrops:
            backdrop.destroy()
        self._backdrops = []
        self.set_visible(False)

    def _on_close_request(self, *_):
        self.dismiss()
        return True  # keep the window (and EDS connections) around

    # --- navigation -------------------------------------------------------

    def _selected(self):
        d = self._calendar.get_date()
        return dt.date(d.get_year(), d.get_month(), d.get_day_of_month())

    def go_today(self):
        self._calendar.select_day(GLib.DateTime.new_now_local())
        self._render_day()

    def _shift_month(self, months):
        self._calendar.select_day(self._calendar.get_date().add_months(months))

    def open_gnome_calendar(self):
        day = self._selected()
        self.dismiss()
        Gio.Subprocess.new(["gnome-calendar", "--date", day.isoformat()],
                           Gio.SubprocessFlags.NONE)

    def _on_key(self, _ctrl, keyval, _keycode, _state):
        handlers = {
            Gdk.KEY_Escape: self.dismiss,
            Gdk.KEY_Home: self.go_today,
            Gdk.KEY_Page_Up: lambda: self._shift_month(-1),
            Gdk.KEY_Page_Down: lambda: self._shift_month(1),
            Gdk.KEY_Return: self.open_gnome_calendar,
            Gdk.KEY_KP_Enter: self.open_gnome_calendar,
        }
        handler = handlers.get(keyval)
        if handler is None:
            return False
        handler()
        return True

    # --- events -----------------------------------------------------------

    def _schedule_reload(self, *_):
        # Dec -> Jan notifies both month and year; fetch once.
        if not self._reload_pending:
            self._reload_pending = True
            GLib.idle_add(self._reload)

    def _reload(self):
        self._reload_pending = False
        self._calendar.clear_marks()
        if self._store is None:
            self._render_day()
            return False
        d = self._calendar.get_date()
        year, month = d.get_year(), d.get_month()
        first = dt.date(year, month, 1)
        # Cover the leading/trailing days of other months shown in the grid.
        start = first - dt.timedelta(days=7)
        end = dt.date(year + month // 12, month % 12 + 1, 1) + dt.timedelta(days=14)
        self._generation += 1
        generation = self._generation
        self._loading = True
        self._render_day()

        def work():
            with self._fetch_lock:
                try:
                    events, error = self._store.fetch(start, end), None
                except Exception as err:  # registry/D-Bus failure
                    events, error = [], err
            GLib.idle_add(self._on_loaded, generation, (year, month), events, error)

        threading.Thread(target=work, daemon=True).start()
        return False

    def _on_loaded(self, generation, month, events, error):
        if generation != self._generation:
            return False  # a newer fetch is on its way
        by_day = {}
        for event in events:
            for day in event.days():
                by_day.setdefault(day, []).append(event)
        self._events_by_day = by_day
        self._error = error
        self._loading = False
        for day in by_day:
            if (day.year, day.month) == month:
                self._calendar.mark_day(day.day)
        self._render_day()
        return False

    def _render_day(self):
        day = self._selected()
        delta = (day - dt.date.today()).days
        self._day_title.set_text({0: "Today", 1: "Tomorrow", -1: "Yesterday"}.get(
            delta, day.strftime("%A %-d %B")))

        while (child := self._events_box.get_first_child()) is not None:
            self._events_box.remove(child)

        if self._store is None:
            return self._placeholder("Install evolution-data-server to see events")
        if self._error is not None:
            return self._placeholder(f"Couldn't read calendars: {self._error}")
        events = self._events_by_day.get(day, [])
        if not events:
            return self._placeholder("Loading…" if self._loading else "No events")
        for event in events:
            self._events_box.append(self._event_row(event, day))

    def _placeholder(self, text):
        self._events_box.append(Gtk.Label(
            label=text, xalign=0, wrap=True, css_classes=["empty"]))

    @staticmethod
    def _event_row(event, day):
        if event.all_day:
            when = "All day"
        else:
            midnight = dt.datetime.combine(day, dt.time())
            begin = "…" if event.start < midnight else f"{event.start:%H:%M}"
            if event.end == event.start:
                when = begin
            else:
                ends_later = event.end > midnight + dt.timedelta(days=1)
                when = f"{begin}–{'…' if ends_later else f'{event.end:%H:%M}'}"

        row = Gtk.Box(spacing=8)
        dot = Gtk.Label(valign=Gtk.Align.START)
        dot.set_markup(f'<span foreground="{event.color}">●</span>')
        row.append(dot)
        row.append(Gtk.Label(label=when, xalign=0, width_chars=11,
                             valign=Gtk.Align.START, css_classes=["event-time"]))

        text = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, hexpand=True)
        text.append(Gtk.Label(label=event.summary, xalign=0, wrap=True,
                              wrap_mode=Pango.WrapMode.WORD_CHAR, max_width_chars=26,
                              css_classes=["event-title"]))
        if event.location:
            text.append(Gtk.Label(label=event.location, xalign=0, max_width_chars=26,
                                  ellipsize=Pango.EllipsizeMode.END,
                                  css_classes=["event-loc"]))
        row.append(text)
        return row


class Backdrop(Gtk.Window):
    """Invisible full-output surface: a click anywhere but the popup closes it.

    Created fresh on every open, so it stacks above waybar (same top layer)
    and swallows the clock click that would otherwise reopen the popup.
    """

    def __init__(self, app, monitor, on_click):
        super().__init__(application=app, css_classes=["calendar-backdrop"])
        LayerShell.init_for_window(self)
        LayerShell.set_namespace(self, "waybar-calendar-backdrop")
        LayerShell.set_layer(self, LayerShell.Layer.TOP)
        LayerShell.set_monitor(self, monitor)
        LayerShell.set_exclusive_zone(self, -1)  # cover waybar too
        for edge in (LayerShell.Edge.TOP, LayerShell.Edge.BOTTOM,
                     LayerShell.Edge.LEFT, LayerShell.Edge.RIGHT):
            LayerShell.set_anchor(self, edge, True)
        LayerShell.set_keyboard_mode(self, LayerShell.KeyboardMode.NONE)
        self.set_child(Gtk.Box(hexpand=True, vexpand=True))
        click = Gtk.GestureClick(button=0)  # any button
        click.connect("pressed", lambda *_: on_click())
        self.add_controller(click)
        self.present()


class App(Gtk.Application):
    def __init__(self):
        super().__init__(application_id=APP_ID)
        self._popup = None

    def do_startup(self):
        Gtk.Application.do_startup(self)
        self.hold()  # stay resident while the popup is hidden
        quit_action = Gio.SimpleAction.new("quit", None)
        quit_action.connect("activate", lambda *_: self.quit())
        self.add_action(quit_action)

    def do_activate(self):
        if self._popup is None:
            self._popup = CalendarPopup(self)
            self._popup.open()
        else:
            self._popup.toggle()


def main():
    app = App()
    if sys.argv[1:] == ["--quit"]:
        app.register(None)
        if app.get_is_remote():
            app.activate_action("quit", None)
            app.get_dbus_connection().flush_sync(None)
        return 0
    return app.run(None)


if __name__ == "__main__":
    sys.exit(main())
