# Case study: a bug that looked nothing like its cause

## Symptom

In the original validation pipeline, each recording's wearable beats were
supposed to be tagged with which task phase they fell in (a calm baseline, a
cognitively demanding task, two mildly stressful tasks, a novelty/surprise
task). After a refactor, that tagging came back essentially empty for almost
every recording — as if the wearable had recorded no beats at all during any
scheduled task.

It had recorded plenty of beats. They just weren't landing inside any of the
scheduled time windows.

## First hypotheses (and why they were wrong)

The obvious suspects were checked first: had the matching tolerance become
too strict? Had a column got renamed and silently defaulted to `NA` somewhere
in the join? Had the device clock drifted so far that a static tolerance
window could no longer be trusted? None of these held up under inspection —
tolerance windows were reasonable, columns were populated, and drift alone
doesn't produce a near-*total* mismatch.

## Finding the actual pattern

The turning point was adding a diagnostic line that printed, for a sample of
recordings, the time-of-day range of the wearable's matched beats side by
side with the time-of-day range the schedule expected:

```
DIAGNOSE (subject X, 2024-03-06): beats fall at 09:06-09:28;
phase windows expect 10:07-10:29.
```

A one-hour gap. Not zero — a *specific*, consistent, non-trivial offset. The
next recording, from June, showed a two-hour gap instead of one. That is the
signature of daylight saving time, not a matching bug: Central Europe is
UTC+1 in winter (CET) and UTC+2 in summer (CEST), and the gap tracked exactly
which one applied on each recording's date.

## Root cause

The raw device timestamps were timezone-naive strings — genuinely correct
local wall-clock time, but with no explicit UTC offset in the string itself
(e.g. `"2024-06-13 10:06:59"`, no `Z` suffix, no `+02:00`). Somewhere in the
pipeline, those strings passed through a parser that — reasonably, by its own
default — assumed UTC for any timezone-naive input. The scheduled task
windows, generated from a different code path, were correctly parsed with
the local timezone. Both timestamps displayed identical digits and both
*meant* to represent the same real-world moment, but the two parsers now
disagreed about which instant those digits actually pointed to, by exactly
the local UTC offset.

## Fix and verification

The fix re-anchors the timezone-naive timestamps to the correct local
timezone before any comparison happens, rather than relying on a parser's
default assumption. After the fix, the offset disappeared and matched-beat
counts recovered to the expected range across both winter and summer
recordings — confirming the diagnosis rather than just papering over the
symptom.

## Why this is worth telling

The bug was invisible at the code-review level — every individual line of
parsing code was locally "correct" for what it was doing. It only became
diagnosable by comparing *behaviour* across conditions (which season a
recording fell in) rather than staring harder at the code, and by resisting
the temptation to patch the matching tolerance as a workaround once the
timing symptom was visible. `R/02_diagnose_and_fix_timezone_bug.R` in this
repository reproduces the same bug shape end-to-end against synthetic data,
including the diagnostic evidence table that pointed at DST specifically.
