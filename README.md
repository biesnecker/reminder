# reminder

A simple program to provide reminders for upcoming events. It is designed to be
added to your `.bashrc`/`.zshrc`/etc. and run at terminal creation.

## Usage

```
Usage: reminder FILE [-a|--lookahead ARG] [-b|--lookback ARG] [--no-color]
  Shows reminders on the commandline

Available options:
  -a,--lookahead ARG       How many days ahead to surface reminders (default: 3)
  -b,--lookback ARG        How many days to surface reminders after they've
                           passed (default: 0)
  --no-color               Disable colors
  -h,--help                Show this help text
```

The file argument is a text file with reminder entries on each line. A basic
example is:

```
2020-01-01 This is a reminder
```

Based on the `lookahead` and `lookback` options, reminders from the past and
future will also be shown.

Wildcards are also supported. A reminder like:

```
*-04-29 Joe's birthday
```

Will be shown each year on April 29th. Wildcards are supported in the year,
month, and day positions, with expected results. For example, to show a reminder
every day each April, you could do this:

```
*-04-* This is a reminder
```

Currently other options (such as being able to specify the last day of the
month, for instance) are not yet supported but may be in the future.
