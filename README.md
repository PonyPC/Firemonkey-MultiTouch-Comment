# Working with Multi-Touch Input in Delphi XE7 (Part 1)

## Introduction

Before the release of XE7, handling multi-touch inputs in Delphi (_and C++ Builder_) applications was quite a complicated affair. While Delphi 2010 introduced “Gesture” support, this solution was less than ideal for real-time multi-touch input handling, as an action bound to a Gesture only executed _after_ the Gesture had been completed, with no _simple_ method of directly intercepting individual touch points to integrate custom behaviour.

Fortunately, XE7 solves this problem by giving us direct access to real-time multi-touch input data. With this data, we can implement custom touch handling for individual points, including our _own_ Gesture detection solutions.

Best of all, it would appear that this new multi-touch feature works identically across all platforms presently supported by Delphi (_Win32, Win64, Mac, Android and iOS_).

## The “OnTouch” Property

[![FireMonkey Form's OnTouch Event](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/blob/main/OnTouch-Event.png?raw=true)](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/blob/main/OnTouch-Event.png?raw=true)

FireMonkey Form’s OnTouch Event

By hooking the _OnTouch_ Event of a FireMonkey Form, we can begin to implement our own multi-touch input handler.

<pre class="brush: delphi; light: true; title: ; notranslate" title="">procedure TForm2.FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
begin

end;</pre>

As you can see from this code snip (_above_), the _OnTouch_ event provides us with several key parameters:

*   _Sender_ is of course a reference to the _Control_ from whence the _OnTouch_ event was executed.
*   _Touches_ provides us with an Array of _Point Information_ representing each individual touch point.
*   _Action_ tells us whether our touch points have ended (_Up_), begun (_Down_), are in motion (_Move_), have been “cancelled” (_Cancel_), or something unknown and crazy (_None_).

You may notice that the multi-touch input handling is handled very differently from keyboard and mouse inputs, where we’re provided with _OnKeyDown_, _OnKeyUp_, _OnMouseDown_, _OnMouseMove_ and _OnMouseUp_ events respectively. I am unsure as to why Embarcadero chose to provide a single “catch-all” event property for _all_ _Touch_ input actions, but we can quite easily separate the singular _OnTouch_ event into its discrete actions of influence.

<pre class="brush: delphi; light: true; title: ; notranslate" title="">type
  TForm2 = class(TForm)
    procedure FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
  private
    procedure TouchNone(Sender: TObject; const Touches: TTouches);
    procedure TouchUp(Sender: TObject; const Touches: TTouches);
    procedure TouchDown(Sender: TObject; const Touches: TTouches);
    procedure TouchMove(Sender: TObject; const Touches: TTouches);
    procedure TouchCancel(Sender: TObject; const Touches: TTouches);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
begin
  case Action of
    TTouchAction.None: TouchNone(Sender, Touches);
    TTouchAction.Up: TouchUp(Sender, Touches);
    TTouchAction.Down: TouchDown(Sender, Touches);
    TTouchAction.Move: TouchMove(Sender, Touches);
    TTouchAction.Cancel: TouchCancel(Sender, Touches);
  end;
end;

procedure TForm2.TouchCancel(Sender: TObject; const Touches: TTouches);
begin

end;

procedure TForm2.TouchDown(Sender: TObject; const Touches: TTouches);
begin

end;

procedure TForm2.TouchMove(Sender: TObject; const Touches: TTouches);
begin

end;

procedure TForm2.TouchNone(Sender: TObject; const Touches: TTouches);
begin

end;

procedure TForm2.TouchUp(Sender: TObject; const Touches: TTouches);
begin

end;</pre>

So we now have (_in the code snip above_) a separate procedure defined for each multi-touch input action, and a simple “case-of” statement passes the appropriate parameter data along to the appropriate handling procedure. All very basic stuff, so let’s move on…

## How Delphi handles Touch Actions

At this point, it is critical to understand _when_ the _OnTouch_ event is called, and what _Touch Action_ is taking place.

Since you can introduce one or more _Touch Points_ subsequent to the start of a one or more previous _Touch Points,_ and likewise you can remove one or more _Touch Points_ at any time while other _Touch Points_ remain in play, it’s important to understand how the _OnTouch_ event responds to changes in the number of _Touch Points_ taking place from the moment a _Touch Action_ begins, and that _Touch Action_ ends.

A _Touch Action_ begins when one _Touch Point_ is placed on the screen. So, if you place one finger on the screen, you have begun a _Touch Action_. When that finger _leaves_ the screen (_either by going outside of the touch area, or by being lifted from the screen entirely_) that _Touch Action_ ends.

_When it comes to Multi-Touch, however, the rules change._

### Problems determining the Start and End of a Multi-Touch Event

For one thing, you cannot _assume_ that a _new_ _Touch Event_ is beginning each time a _Touch Down Action_ occurs, or that a _Touch Event_ is ending each time a _Touch Up_ or _Touch Cancel Action_ occurs. This is because the introduction of a new _Touch Point_, or removal of an existing _Touch Point_ can occur _at any time_.

To illustrate this point, I added a second form to the example code snip provided above, dropped a single _TMemo_ control on it, and modified the main unit as follows:

<pre class="brush: delphi; light: true; title: ; notranslate" title="">type
  TForm2 = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
  private
    procedure TouchNone(Sender: TObject; const Touches: TTouches);
    procedure TouchUp(Sender: TObject; const Touches: TTouches);
    procedure TouchDown(Sender: TObject; const Touches: TTouches);
    procedure TouchMove(Sender: TObject; const Touches: TTouches);
    procedure TouchCancel(Sender: TObject; const Touches: TTouches);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Unit3;

{$R *.fmx}

procedure TForm2.FormShow(Sender: TObject);
begin
  Form3.Show;
end;

procedure TForm2.FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
begin
  case Action of
    TTouchAction.None: TouchNone(Sender, Touches);
    TTouchAction.Up: TouchUp(Sender, Touches);
    TTouchAction.Down: TouchDown(Sender, Touches);
    TTouchAction.Move: TouchMove(Sender, Touches);
    TTouchAction.Cancel: TouchCancel(Sender, Touches);
  end;
end;

procedure TForm2.TouchCancel(Sender: TObject; const Touches: TTouches);
var
  I: Integer;
begin
  Form3.Memo1.Lines.Add('Touch Cancel:');
  for I := Low(Touches) to High(Touches) do
    Form3.Memo1.Lines.Add(Format(#9 + #9 + 'Point %d (%n, %n)', [I, Touches[I].Location.X, Touches[I].Location.Y]));
end;

procedure TForm2.TouchDown(Sender: TObject; const Touches: TTouches);
var
  I: Integer;
begin
  Form3.Memo1.Lines.Add('Touch Down:');
  for I := Low(Touches) to High(Touches) do
    Form3.Memo1.Lines.Add(Format(#9 + #9 + 'Point %d (%n, %n)', [I, Touches[I].Location.X, Touches[I].Location.Y]));
end;

procedure TForm2.TouchMove(Sender: TObject; const Touches: TTouches);
var
  I: Integer;
begin
  Form3.Memo1.Lines.Add('Touch Move:');
  for I := Low(Touches) to High(Touches) do
    Form3.Memo1.Lines.Add(Format(#9 + #9 + 'Point %d (%n, %n)', [I, Touches[I].Location.X, Touches[I].Location.Y]));
end;

procedure TForm2.TouchNone(Sender: TObject; const Touches: TTouches);
var
  I: Integer;
begin
  Form3.Memo1.Lines.Add('Touch None:');
  for I := Low(Touches) to High(Touches) do
    Form3.Memo1.Lines.Add(Format(#9 + #9 + 'Point %d (%n, %n)', [I, Touches[I].Location.X, Touches[I].Location.Y]));
end;

procedure TForm2.TouchUp(Sender: TObject; const Touches: TTouches);
var
  I: Integer;
begin
  Form3.Memo1.Lines.Add('Touch Up:');
  for I := Low(Touches) to High(Touches) do
    Form3.Memo1.Lines.Add(Format(#9 + #9 + 'Point %d (%n, %n)', [I, Touches[I].Location.X, Touches[I].Location.Y]));
end;</pre>

What this program does is output a dump of the _Touch Actions_ and their respective _Touch Point Data_ into that _TMemo_ control on the other _Form_ so that we can see exactly what _Touch Actions_ are taking place each time _OnTouch_ is called.

This is actually a much more reliable way of debugging multi-touch event data than using the debugger, as the debugger would cause fragmentation of the input data each time the IDE broke out at a breakpoint for introspection.

I ran this program and momentarily touched three fingers onto my multi-touch Windows tablet device for less than one second. From _my_ perspective, all three fingers made contact with the screen at _exactly_ the same moment, and broke contact with the screen again at _exactly_ the same moment, however the data log tells a very different story:

<pre>Touch Down:
        Point 0 (196.00, 275.00)
Touch Down:
        Point 0 (196.00, 275.00)
        Point 1 (285.00, 142.00)
Touch Down:
        Point 0 (196.00, 275.00)
        Point 1 (285.00, 142.00)
        Point 2 (416.00, 123.00)
Touch Move:
        Point 0 (196.00, 275.00)
        Point 1 (285.00, 142.00)
        Point 2 (416.00, 125.00)
Touch Move:
        Point 0 (196.00, 275.00)
        Point 1 (285.00, 142.00)
        Point 2 (416.00, 125.00)
Touch Move:
        Point 0 (196.00, 275.00)
        Point 1 (285.00, 142.00)
        Point 2 (416.00, 125.00)
Touch Move:
        Point 0 (196.00, 275.00)
        Point 1 (285.00, 142.00)
        Point 2 (416.00, 125.00)
Touch Move:
        Point 0 (197.00, 276.00)
        Point 1 (285.00, 142.00)
        Point 2 (415.00, 127.00)
Touch Move:
        Point 0 (197.00, 276.00)
        Point 1 (285.00, 142.00)
        Point 2 (415.00, 127.00)
Touch Move:
        Point 0 (285.00, 142.00)
        Point 1 (418.00, 135.00)
Touch Up:
        Point 0 (285.00, 142.00)
        Point 1 (418.00, 135.00)
Touch Move:
        Point 0 (288.00, 141.00)
Touch Up:
        Point 0 (288.00, 141.00)</pre>

What we can see in the input dump above is that the _Touch Down_ action was executed _three separate times_, once for each additional input, suggesting that – despite appearing from my perspective that all three fingers contacted the screen at the same exact moment – there was an imperceptible delay between each of my fingers making contact with the screen.

This means that, if we were to try and determine when a _Touch Action_ begins by toggling its state using _Touch Down_, we would have problems as this _Touch Action_ is essentially re-entrant (_it can occur multiple times without a Touch Up or Touch Cancel action terminating that state_).

We can also see from this input dump that the _Touch Move_ action occurs repeatedly for the entire duration of contact with the screen, _even if none of the Touch Points have actually changed their respective positions_.

Finally, we can see that while _three_ separate _Touch Down_ events occurred at the beginning of our input, only _two_ _Touch Up_ events occurred at the end. Presumably this is because two of my fingers left the surface of the screen at exactly the same time, and the other (_most likely my index finger_) left the surface a _fraction of a second_ later.

These details are _very important_, particularly if we want to track a complete multi-touch input event from start to finish, as we need to figure out the most reliable way to determine when a multi-touch input event starts, and when it finishes.

Tracking a multi-touch input event from start to finish is particularly useful if you want to draw multiple paths using the movement data for each respective _Touch Point_, or (_more usefully_) for enabling _multiple simultaneous drag and drop operations_ in your applications.

### Problems with the Order of Contact

Another even more challenging issue is keeping track of the order in which _Touch Points_ are terminated from a touch event.

Here’s another input log where-in I placed three fingers on the screen in turn (_index, middle, ring finger_). I then removed my _middle_ finger, followed by my _index_ finger, followed by my _ring_ finger.

Think of this as:

Down: 1, 2, 3  
Up: 2, 1, 3

Here’s what that log looks like:

<pre>**Touch Down:**
 **Point 0 (200.00, 312.00)**
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
Touch Move:
        Point 0 (200.00, 312.00)
**Touch Down:**
 **Point 0 (200.00, 312.00)**
 **Point 1 (272.00, 152.00)**
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (272.00, 152.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (272.00, 153.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (272.00, 153.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (272.00, 153.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (272.00, 153.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (272.00, 153.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (271.00, 154.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (271.00, 155.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (271.00, 156.00)
**Touch Down:**
 **Point 0 (200.00, 312.00)**
 **Point 1 (271.00, 156.00)**
 **Point 2 (390.00, 82.00)**
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (271.00, 158.00)
        Point 2 (390.00, 82.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (271.00, 159.00)
        Point 2 (390.00, 82.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (270.00, 159.00)
        Point 2 (390.00, 82.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (270.00, 159.00)
        Point 2 (388.00, 82.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (270.00, 159.00)
        Point 2 (388.00, 82.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (270.00, 159.00)
        Point 2 (387.00, 83.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (270.00, 159.00)
        Point 2 (387.00, 83.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (269.00, 157.00)
        Point 2 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (269.00, 157.00)
        Point 2 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (201.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (200.00, 312.00)
        Point 1 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (387.00, 81.00)
Touch Move:
        Point 0 (388.00, 81.00)
Touch Move:
        Point 0 (388.00, 77.00)
**Touch Up:**
 **Point 0 (388.00, 77.00)**</pre>

Note that I have not omitted a single piece of information from this input dump, but I _have_ highlighted the _Touch Down_ and _Touch Up_ actions.

This illustrates one shocking (_and troublesome_) issue: while you can clearly see that I did indeed place three fingers on the screen, only _one Touch Up_ action executed at the very end of the log.

Now, had I removed all three fingers from the screen at exactly the same time, this would make some sense. However, you can _clearly see_ from this input dump when each of my fingers left the screen (_based on the reduction in the number of Points being displayed in the log_), and that the _Touch Up_ actions did not occur until the very last finger left the screen.

This is particularly confusing when you consider that, in the _previous_ test case, there were _two_ separate _Touch Up_ actions, presumably suggesting that two fingers left the screen simultaneously, with the third following a fraction of a second later. We would have _expected_ there to have been _three_ separate _Touch Up_ actions in _this_ test, as I deliberately removed each finger from the screen in turn.

This means that we cannot rely _at all_ on either the _Touch Down_ or _Touch Up_ actions to determine when a _Touch Event_ begins or ends.

Indeed, we can see from these two tests that the execution of the _Touch Down_ and _Touch Up_ actions cannot be trusted to behave in a consistent way.

Another critical piece of information this test illustrates is that, when you remove a _Touch Point_, the Array position of all _subsequent Touch Points_ shifts back by 1\. This means that the _Index_ of a _Touch Point_ is not necessarily going to be the same at the _end_ of a _Touch Action_ as it was at the _beginning_.

