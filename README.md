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


## Previously…

We took a look at how the _OnTouch_ event introduced in XE7 works, and discovered some potential stumbling blocks when it comes to taking advantage of this new feature.

In this part of the series, we’re going to look at potential “workarounds” for these problems; alternative (_albeit slightly more complicated_) methods of harnessing multi-touch input information for practical purposes in Delphi applications.

## Another “quirk” in the way XE7’s Multi-Touch Input Handler Works

Since posting the first article in this series, I have conducted many more tests, and discovered that it continues to track _Touch Move_ and _Touch Up_ events even after one or more _Touch Points_ leave the constraints of your Form.

Now, this shouldn’t necessarily be considered a “bug” or even a problem, as it means we can (_in theory anyway_) use multi-touch input to perform “drag and drop” operations on Windows (_possibly also Mac_) beyond the constraints of your Form(s).

It is, of course, fairly trivial to determine whether or not a _Touch Point_ is inside the constraints of your Form.

I just wanted to share this observation for the benefit of others, as it may just effect the way you work with multi-touch input in your applications.

## Detecting when a Multi-Touch Input Event Begins and Ends

We previously discovered that th_e Touch Down_ action seems to occur once for each additional _Touch Point_ (_finger_) introduced to the touch screen (_effectively making that action “re-entrant”_), yet the _Touch Up_ action doesn’t _necessarily_ occur separately for each _Touch Point_ removed from the touch screen.

So, since the _OnTouch_ event behaves in a somewhat-inconsistent way, we need an _inventive_ solution to decide when a multi-touch input event begins and ends.

Since we _know_ that a multi-touch input event will begin with a _Touch Down_ action, we can define a Boolean member to act as a “flag”. Also, we can say with relative certainty that a multi-touch input event will end either with a _Touch Up_ action, or a _Touch Cancel_ action, and I’ve determined that, where it ends with a _Touch Up_ action, that action will contain either _one or zero_ items in the _Touch Point_ array (_Touches_ parameter).

This means we have a logical principle on which to base our multi-touch input event detection.

<pre class="brush: delphi; light: true; title: ; notranslate" title="">unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
  private
    FTouching: Boolean;
    FTouchHistory: Array of TTouches;
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

procedure TForm2.FormCreate(Sender: TObject);
begin
  FTouching := False;
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
begin
  FTouching := False;
  Label1.Text := 'Touch Cancelled!';
end;

procedure TForm2.TouchDown(Sender: TObject; const Touches: TTouches);
begin
  if (not FTouching) then
  begin
    FTouching := True;
    Label1.Text := Format('Touch Points: %d', [Length(Touches)]);
  end;
end;

procedure TForm2.TouchMove(Sender: TObject; const Touches: TTouches);
begin
  Label1.Text := Format('Touch Points: %d', [Length(Touches)]);
end;

procedure TForm2.TouchNone(Sender: TObject; const Touches: TTouches);
begin
  // I genuinely don't know what this is for!
end;

procedure TForm2.TouchUp(Sender: TObject; const Touches: TTouches);
begin
  if (FTouching) then
  begin
    if Length(Touches) <= 1 then
    begin
      FTouching := False;
      Label1.Text := 'Not Touching';
    end else
      Label1.Text := Format('Touch Points: %d', [Length(Touches)]);
  end;
end;

end.</pre>

This code snip (_above_) works on Win32, Win64, Mac, Android and iOS. It illustrates a way of determining when an individual multi-touch input event begins and ends (_in this case, by displaying the current “Touch State” on a TLabel control on the Form_).

Pay special attention to the _TouchUp_ procedure, which uses a simple _if_ statement to determine whether or not we should consider the multi-touch input event as completed.

Put simply, while we won’t necessarily always get a _Touch Up_ action when each separate _Touch Point_ is lifted from the screen, we _always_ get at least _one_ (_assuming that we don’t get a Touch Cancel event instead, I mean_), and this final _Touch Up_ action will only ever contain either one or zero _Touch Points_ in the _Touches_ array.

This gives us a very simple workaround for that inconsistent behaviour, one which (_according to my extensive testing, at least_) appears to work 100% of the time.

The _FTouching_ boolean member plays a fairly important role, in that it prevents _TouchDown_ from acting in a re-entrant manner. It would be problematic (_to say the least_) if we allowed the _TouchDown_ procedure to perform its function multiple times during an existing _Touch Event_… particularly when you consider a “drag and drop” operation as an example.

I also just want to point out that I have been completely unable to get _TouchNone_ to occur, on _any_ platform, under _any_ set of circumstances. At this point, I have genuinely _no idea_ what that particular _Touch Action Type_ is for, or under what set of bizarre circumstances it will occur.

[You can download the working demo _tidied up a lot_ of this code snip here](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/raw/main/Touch_Start_End_Detection.zip "Demos - Touch Start End Detection")[)](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/raw/main/Touch_Start_End_Detection.zip "Demos - Touch Start End Detection").

## Something Fun: Moving Controls At Runtime (using multi-touch input)

Okay, now that we’ve solved the problem of adequately detecting when a multi-touch event starts and ends, let’s put this solution to use and make something entertaining.

What we’re going to do now is make a little “multi-touch playground” project, allowing us to drag multiple controls around the screen at the same time with individual fingers on a touch screen. At face value, this little “toy project” might not seem all that useful, however this same principal can be applied to a simultaneous drag-and-drop solution that _could_ be useful.

[![Multi-Touch Playground - Moving Controls](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/blob/main/Multi-Touch-Playground-Moving-Controls.png?raw=true)](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/blob/main/Multi-Touch-Playground-Moving-Controls.png?raw=true)

Multi-Touch Playground – Moving Controls


Basically, with this demo we can simply drop a bunch of controls onto the Form, and use multi-touch input to move one or more of those controls around.

Here’s the code for this demo:

<pre class="brush: delphi; light: true; title: ; notranslate" title="">unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.StdCtrls;

type
  TControlTouch = record
    Control: TControl;
    Origin: TPointF;
  end;

  TfrmMain = class(TForm)
    Rectangle1: TRectangle;
    RoundRect1: TRoundRect;
    Ellipse1: TEllipse;
    Pie1: TPie;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
  private
    FTouching: Boolean;
    FControls: Array of TControlTouch;
    function GetControlAtPoint(const APoint: TPointF): TControl;
    procedure GetControlsAtEachTouchPoint(const Touches: TTouches);
    procedure TouchEnd;
    procedure TouchNone(Sender: TObject; const Touches: TTouches);
    procedure TouchUp(Sender: TObject; const Touches: TTouches);
    procedure TouchDown(Sender: TObject; const Touches: TTouches);
    procedure TouchMove(Sender: TObject; const Touches: TTouches);
    procedure TouchCancel(Sender: TObject; const Touches: TTouches);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
begin
  case Action of
    TTouchAction.None: TouchNone(Sender, Touches);
    TTouchAction.Up: TouchUp(Sender, Touches);
    TTouchAction.Down: TouchDown(Sender, Touches);
    TTouchAction.Move: TouchMove(Sender, Touches);
    TTouchAction.Cancel: TouchCancel(Sender, Touches);
  end;
end;

function TfrmMain.GetControlAtPoint(const APoint: TPointF): TControl;
var
  I: Integer;
  LObject: TControl;
  LRect: TRectF;
begin
  Result := nil;
  for I := ChildrenCount - 1 downto 0 do
  begin
    if (Children[I] is TControl) then
      if TControl(Children[I]).HitTest then
      begin
        LObject := TControl(Children[I]);
        LRect := RectF(LObject.Position.X,
                       LObject.Position.Y,
                       LObject.Position.X + LObject.Width,
                       LObject.Position.Y + LObject.Height);
        if LRect.Contains(APoint) then
        begin
          Result := LObject;
          Break;
        end;
      end;
  end;
end;

procedure TfrmMain.GetControlsAtEachTouchPoint(const Touches: TTouches);
var
  I: Integer;
begin
  // Hold a reference to whatever controls are under each respective touch point
  SetLength(FControls, Length(Touches));
  for I := Low(Touches) to High(Touches) do
  begin
    FControls[I].Control := GetControlAtPoint(Touches[I].Location);
    FControls[I].Origin := Touches[I].Location;
  end;
end;

procedure TfrmMain.TouchCancel(Sender: TObject; const Touches: TTouches);
begin
  TouchEnd;
end;

procedure TfrmMain.TouchDown(Sender: TObject; const Touches: TTouches);
begin
  if (not FTouching) then
  begin
    FTouching := True;
    // Release any existing history
    SetLength(FControls, 0);
  end;
  GetControlsAtEachTouchPoint(Touches);
end;

procedure TfrmMain.TouchEnd;
begin
  FTouching := False;
end;

procedure TfrmMain.TouchMove(Sender: TObject; const Touches: TTouches);
var
  I: Integer;
  LDifference: TPointF;
begin
  if Length(Touches) = Length(FControls) then
  begin
    // Move the controls
    for I := Low(Touches) to High(Touches) do
      if FControls[I].Control <> nil then
      begin
        LDifference := PointF(Touches[I].Location.X - FControls[I].Origin.X,
                              Touches[I].Location.Y - FControls[I].Origin.Y);
        FControls[I].Control.Position.X := FControls[I].Control.Position.X + LDifference.X;
        FControls[I].Control.Position.Y := FControls[I].Control.Position.Y + LDifference.Y;
        FControls[I].Origin := Touches[I].Location;
      end;
  end else
    GetControlsAtEachTouchPoint(Touches);
end;

procedure TfrmMain.TouchNone(Sender: TObject; const Touches: TTouches);
begin

end;

procedure TfrmMain.TouchUp(Sender: TObject; const Touches: TTouches);
begin
  if (FTouching) and (Length(Touches) <= 1) then
    TouchEnd
  else
    GetControlsAtEachTouchPoint(Touches);
end;

end.</pre>

This code snip (_above_) obtains a handle on whatever control is underneath each _Touch Point_, and basically moves them by their respective offsets on each _Touch Move_ event.

> Please note that this demo source observes the value of each control’s _HitTest_ property, so if you set _HitTest_ to _False_, you won’t be able to drag it around (such as the two _TLabel_ controls included in the downloadable sample)

When the number of touch points changes, it re-establishes the Array of relevant controls for each _Touch Point_.

As with all the other demos in this series of articles, this will work on Windows, Mac, iOS and Android.

[You can download the working demo of this code snip here](https://github.com/PonyPC/Firemonkey-MultiTouch-Comment/raw/main/Touch_Moving_Controls.zip "Demos - Touch Playground (Moving Controls)").


# Russia Reference
# <span class="post__title-text">Поддержка MultiTouch + Gestures в Delphi XE4</span>

*   [Программирование](https://habr.com/ru/hub/programming/ "Вы не подписаны на этот хаб"),
*   [Delphi](https://habr.com/ru/hub/delphi/ "Вы не подписаны на этот хаб")

<div class="post__body post__body_full">

<div class="post__text post__text-html post__text_v1" id="post-content-body">

<div style="text-align:center;">![](https://habrastorage.org/files/66f/57b/fe5/66f57bfe574148dc97ffcc9c3f8d2ede.jpg)</div>

Как-то незаметно для меня прошли все эти новые веяния в виде активных мониторов, на которые пользователь может тыкать пальцами. И знать бы о них не знал, кабы месяца три назад начальство не приобрело себе ноутбук, который можно порвать на две части (экран отдельно, клавиатура отдельно), причем не Surface какой-то, пропиаренный MS, а уже самый проходной у пользователей – от [ASUS](http://www.asus.com/in-search-of-incredible/ru-ru/asus-transformer-book-t300), за гораздо меньшие деньги (относительно).  
Да и закуплен был данный девайс не просто так – задача выросла оттуда, откуда и не ждали.  

Де юре: мы проводим огромное количество семинаров в месяц, и вот именно на них, нашим лекторам начали демонстрировать несовместимость нашего ПО с этим пресловутым тачем.  

Де факто: на почту саппорта начали сыпаться гневные письма пользователей плана – «я два раза тапнула, а оно не тапнулось, может не так тапнула?»  
А начальство все это скрупулезно отслеживало на своем «порватом» ноутбуке и готовило ТЗ.  

И вот настал тот день. Мне на рабочий стол был воздвигнут третий монитор, [23 дюйма от LG](http://www.ixbt.com/monitor/lg-23et63v.shtml) (с поддержкой Touch ввода аж 10 пальцами) и поставлена задача – это должно заработать в течении трех дней!  

А работаю то я в XE4 – беда.  
<a name="habracut"></a>  

#### 0\. Анализ проблемы

Благо я знаком с множеством компетентных товарищей (включая Embarcadero MVP), с которыми можно посоветоваться, с какой стороны вообще подойти к поддержке Touch, но… вычитав досконально линки на технические статьи (присланные ими) о поддержке мультитача, я понял, что в XE4 мне ничего не светит. Доступные мне возможности VCL сильно ограничены.  

Немного почитав конференции Embarcadero я узнал, что мультитач, с некоторыми ограничениями, стал доступен только в XE7 (однако).  

Не уверен что начальство оценило бы, если бы я сказал, что самый простой способ решения задачи выглядит в виде апдейта на ХЕ7 (плюс время затраты на проверку кода на совместимость после апа).  

Поэтому смотрим что мне доступно в XE4:  
**плюсы:**  
— она знает о жестах (Gesture).  
**минусы:**  
— она не знает о Touch (знает, но не предоставляет внешнего обработчика);  
— она не знает о Gesture при помощи двух точек ввода (двумя и более пальцами).  

А теперь посмотрим что мне не доступно:  

1.  Я не могу расширить класс TRealTimeStylus введением поддержки интерфейса IRealTimeStylus3 до кучи к IStylusAsyncPlugin просто потому, что он спрятан от меня внутри TPlatformGestureEngine аж в strict private type секции класса.
2.  Мне не предоставлен полноценный обработчик сообщения WM_TOUCH, хотя данное сообщение обрабатывается внутри TWinControl.WndProc:

    WM_TOUCH:
      with FTouchManager do
        if (GestureEngine <> nil) and (efTouchEvents in GestureEngine.Flags) then
          GestureEngine.Notification(Message);

Как можно увидеть по коду, управление идет сразу на движок распознавания жестов.  
Хотя казалось бы – а зачем мне гестуры, если я хочу двигать пять картинок по канвасу в том порядке, который гестуры явно не распознают?  

Конечно, во втором случае я могу сам перекрыть WM_TOUCH, но раз уж кто-то взялся за его обработку и получил данные, почему бы их не отдать наружу, избавив разработчика от повторного дубляжа кода?  

Поэтому зайдем с другого бока.  

#### 1\. Постановка задачи

Наше ПО — это по сути очень сильно навороченный Excel, правда, с заточкой под определенный контингент пользователей, в данном случае сметчиков. Впрочем, немного перефразирую: дистанция между возможностями нашего софта и Excel примерно аналогична разнице между MsPaint и Adobe Photoshop.  
Наши пользователи тоже могут реализовать в Excel некий документ в виде сметы, так же как и рисунок в MsPaint. Весь цимус в результате.  

Разработан проект был по иидеологииWYSIWYG, и представляет из себя в 90 процентов случаев некий кастомный класс (от TCustomControl), реализующий грид, в котором пользователь работает, так же как с обычным бумажным документом.  

Выглядит примерно так: (скриншот сделан во время операции DragDrop позиции, на стрелку не обращайте внимания, бо картинка выдрана из какой-то техсопроводиловки и указывает на плавающий Hint, типа фишка.  

<div style="text-align:center;">![](https://habrastorage.org/files/c5c/a93/2a6/c5ca932a6210486ea4600c09a377c143.png)</div>

В этом контроле отсутствуют такие стандартные понятия, как скролл. Он, конечно, есть, но им эмулируется манипуляция с колонками в случае подвижки по горизонтали, либо в случае вертикального смещения – переход на следующую строку листа.  
Он не воспринимает стандартные сообщения о скролировании.  

В базовом варианте (который выдает ОС) он умеет получать события о клике мышки, эмулируемое системой через тап на тачскрине, и WM_MOUSEMOVE, так же эмулируемое системой через тач.  

**А что нужно:**  

*   Единственное, что умеет текущий вариант Gesture – тап двумя пальцами для вызова PopupMenu по координатам тапа;
*   Скролирование влево/право/вверх/вниз посредством свайпа двумя пальцами на тачскрине;
*   Эмуляция команд «назад/вперед», посредством свайпа тремя пальцами на тачскрине.

С учетом того что Gesture в XE4 принципиально не заточены на мультитач (даже на уровне редактора гестур), а задачу решать надо, я грустил целый вечер и… с утра приступил к работе.  

#### 2\. Используемые термины

Как я говорил ранее, я не огромный специалист во всех этих новых веяниях, поэтому в статье я буду оперировать следующими определениями (вполне вероятно, что неправильными):  

**Тап** — аналог клика мышкой, событие, возникающее при одинарном коротком нажатии пальцем на тачскрин.  
**Тач** (или точка тача) — нечто описывающее ситуацию, когда палец контактирует с тачскрином (и обрабатывается сообщение WM_TOUCH).  
**Маршрут** — список координат, над которыми пользователь провел пальцем (перемещалась точка тача).  
**Сессия** — начинается, когда палец коснулся тачскрина, продолжается, когда пользователь водит по нему, не отпуская пальца, и завершается, когда палец убран. На протяжении сессии строится ее маршрут.  
**Жест** (Gesture) — некий шаблонный эталон маршрута, с которым сравнивается маршрут сессии. К примеру пользователь ткнул пальцем, потянул влево и отпустил — это жест с идентификатором sgiLeft.  

#### 3\. Разбираемся с обработкой WM_TOUCH

Для начала необходимо определиться – а поддерживает ли вообще наше железо мультитач?  
Для этого достаточно вызвать GetSystemMetrics с параметром SM_DIGITIZER и проверить результат на наличие двух флагов: NID_READY и NID_MULTI_INPUT.  

Грубо:  

    tData := GetSystemMetrics(SM_DIGITIZER);
    if tData and NID_READY <> 0 then
      if tData and NID_MULTI_INPUT <> 0 then
        ... все хорошо, можно работать

К сожалению, если у вас нет устройств с поддержкой мультитача работающих на OS Windows, то дальнейшая часть статьи будет для вас просто теорией, без возможности проверки результата. Можно попробовать использовать эмулятор тача из [Microsoft Surface 2.0 SDK](http://www.microsoft.com/en-us/download/details.aspx?id=26716), но я с ним не экспериментировал.  

НО!!! Если ваш девайс поддерживает мультитач, то можно попробовать его пощупать. Для этого выберем произвольное окно (к примеру главную форму) и скажем:  

    RegisterTouchWindow(Handle, 0);

Без вызова данной функции наше выбранное окно не будет принимать сообщения WM_TOUCH.  
«Отлучить» окно от получения данного сообщения поможет функция UnregisterTouchWindow.  

Декларируем обработчик сообщения WM_TOUCH.  

    procedure WmTouch(var Msg: TMessage); message WM_TOUCH;

И начинаем разбираться – что он нам вообще дает.  

Итак, параметр WParam данного сообщения содержит количество активных точек тача, о котором нам хочет сообщить система. Причем это число хранится только в нижних двух байтах, что намекает о возможности поддержки системой до 65535 точек ввода.  

Я пытался такое прикинуть – не получилось, бо у меня монитор держит максимум 10 пальцев. Хотя, в этом и есть цимус, если оглядываться на современные фантастические фильмы, где показаны некие виртуальные столы с данными, с которыми работает куча людей, имеющих возможность туда тыкать всеми десятью каждый (ну, к примеру, «Аватар» тот-же, или «Обливион»).  

Молодцы, заложились на перспективу, хотя, как оказалось – это уже давно работает и без фильмов, просто я не всегда слежу за новинками. К примеру, вот такой 46 дюймовый девайс был представлен на выставке «Consumer Electronics Show 2011»:  

<div style="text-align:center;">![](https://habrastorage.org/files/6f7/40c/466/6f740c466fc54330b243e2481d60bfce.jpg)</div>

Впрочем, не будем отвлекаться:  
А вот LParam данного сообщения является неким хэндлом, через который можно получить более подробную информацию о сообщении посредством вызова функции GetTouchInputInfo.  
Если после вызова GetTouchInputInfo повторный вызов данной функции не требуется, то MSDN рекомендует сказать CloseTouchInputHandle, но это не обязательно, т.к. очистка данных в куче все равно произойдет автоматом при передаче управления на DefWindowProc или при попытке отправки данных через SendMessage/PostMessage.  
Более [подробнее тут](http://msdn.microsoft.com/ru-ru/library/windows/desktop/dd317341(v=vs.85).aspx).  

Что от нас требует функция GetTouchInputInfo:  

1.  Ей необходим сам хэндл, с которым она будет работать;
2.  Ей необходим выделенный буфер ввиде массива из элементов TTouchInput, в котором она разместит всю информацию о событии;
3.  Размер этого массива;
4.  Размер каждого элемента массива.

Опять молодцы: при помощи четвертого пункта, сразу заложились на возможность изменения структуры TTouchInput в следующих версиях ОС (даже интересно, что туда еще можно добавить?  

Если сильно грубо, то ее вызов выглядит так:  

    var
      Count: Integer;
      Inputs: array of TTouchInput;
    begin
      Count := Msg.WParam and $FFFF;
      SetLength(Inputs, Count);
      if GetTouchInputInfo(Msg.LParam, Count, @Inputs[0], SizeOf(TTouchInput)) then
        // ... что-то делаем с полученной информацией
      CloseTouchInputHandle(Msg.LParam);

Это все. А теперь попробуем разобраться с данными, которые хранятся в массиве Inputs.  

#### 4\. Обрабатываем TTouchInput

С этого самого момента начинается самое интересное.  

Размер массива TTouchInput зависит от того, сколько пальцев приложено к тачскрину.  
Для каждой точки тача (пальца) система генерирует уникальный ID, который не изменяется в течении всей сессии (от момента касания пальцем, до… пока мы его не убрали).  
Этот ID отображен на каждый элемент TTouchInput массива и хранится в параметре dwID.  

Кстати о сессиях:  
Сессия, это… Ну давайте вот так:  

<div style="text-align:center;">![](https://habrastorage.org/files/b44/bf1/009/b44bf1009ac64d8e8bde831f99880bc6.png)</div>

На картинке отображено ровно 10 сессий (под каждый палец), показан их маршрут (массив точек над которыми перемещался палец в рамках каждой сессии), причем, каждая из сессий еще не завершена (пальцы все еще приложены к тачскрину).  

Впрочем, вернемся обратно к структуре TTouchInput.  
По сути, для нормальной работы с тачем от данной структуры нам требуется всего лишь несколько параметров:  

    TOUCHINPUT = record
      x: Integer; // абсолютные координаты
      y: Integer; // точки тача
      hSource: THandle; // хэндл окна, обрабатывающего сообщение
      dwID: DWORD; // уникальный идентификатор точки
      dwFlags: DWORD; // текущее состояние точки
      // все остальное в принципе не нужно
      dwMask: DWORD;
      dwTime: DWORD;
      dwExtraInfo: ULONG_PTR;
      cxContact: DWORD;
      cyContact: DWORD;
    end;

Давайте сразу начнем с реализации демо-приложения.  
Создайте новый проект и на главной форме разместите TMemo, в которое быдет выводится лог работы с тачем.  

В конструкторе формы подключаем ее к обработке сообщения WM_TOUCH:  

    procedure TdlgSimpleTouchDemo.FormCreate(Sender: TObject);
    begin
      RegisterTouchWindow(Handle, 0);
    end;

Теперь пишем обработчик события:  

    procedure TdlgSimpleTouchDemo.WmTouch(var Msg: TMessage);

      function FlagToStr(Value: DWORD): string;
      begin
        Result := '';
        if Value and TOUCHEVENTF_MOVE <> 0 then
          Result := Result + 'move ';
        if Value and TOUCHEVENTF_DOWN <> 0 then
          Result := Result + 'down ';
        if Value and TOUCHEVENTF_UP <> 0 then
          Result := Result + 'up ';
        if Value and TOUCHEVENTF_INRANGE <> 0 then
          Result := Result + 'ingange ';
        if Value and TOUCHEVENTF_PRIMARY <> 0 then
          Result := Result + 'primary ';
        if Value and TOUCHEVENTF_NOCOALESCE <> 0 then
          Result := Result + 'nocoalesce ';
        if Value and TOUCHEVENTF_PEN <> 0 then
          Result := Result + 'pen ';
        if Value and TOUCHEVENTF_PALM <> 0 then
          Result := Result + 'palm ';
        Result := Trim(Result);
      end;

    var
      InputsCount, I: Integer;
      Inputs: array of TTouchInput;
    begin
      // получаем количество точек тача
      InputsCount := Msg.WParam and $FFFF;
      // подготавливаем соответствующий массив данных
      SetLength(Inputs, InputsCount);
      // получаем информацию по текущему событию
      if GetTouchInputInfo(Msg.LParam, InputsCount, @Inputs[0], SizeOf(TTouchInput)) then
      begin
        // закрываем хэндл (а можно и не закрывать)
        CloseTouchInputHandle(Msg.LParam);
        // выводим информацию на экран
        for I := 0 to InputsCount - 1 do
          Memo1.Lines.Add(Format('TouchInput №: %d, ID: %d, flags: %s',
            [I, Inputs[I].dwID, FlagToStr(Inputs[I].dwFlags)]));
      end;
    end;

Это все.  

Согласитесь – просто до невозможности. Все данные перед глазами.  
Попробуйте поэкспериментировать с этим кодом с использованием тачскрина и вы заметите, что разработчику, помимо привязки к ID каждого тача, передается еще определенный набор флагов, которые выводятся в лог.  
По данным лога сразу можно определить начало сессии тача (флаг TOUCHEVENTF_DOWN), перемещение каждого из пальцев по тачскрину (флаг TOUCHEVENTF_MOVE) и завершение сессии (флаг TOUCHEVENTF_UP).  

Выглядит вот так:  

<iframe width="420" height="315" src="https://www.youtube.com/embed/SR0pWZBPks0?feature=oembed" frameborder="0" allowfullscreen=""></iframe>  

Сразу оговорюсь об одной неприятности: не всегда в обработчик WM_TOUCH будут приходить сообщения от тачскрина с флагами TOUCHEVENTF_DOWN или TOUCHEVENTF_UP. Этот нюанс нужно учитывать при реализации своих «классов-оберток», о которых пойдет речь чуть ниже.  

**К примеру:**  
Наше приложение в данный момент отображает PopupMenu – нажатие на тачскрин приведет к его закрытию, но сообщение WM_TOUCH с флагом TOUCHEVENTF_DOWN к нам не придет, хотя последующие, с флагом TOUCHEVENTF_MOVE, мы получим достаточно успешно.  
То же относится и к показу PopupMenu в обработчике события TOUCHEVENTF_MOVE.  
В данном случае произойдет срыв сессии и сообщения WM_TOUCH с флагом TOUCHEVENTF_UP ждать не стоит.  

Такое поведение наблюдается под Windows 7 (32/64 бита), я даже допускаю – под Windows 8 и выше что-то поменялось, но у меня просто нет возможности проверить это сейчас (лень – второе я).  

Впрочем, получив представление о том «как это работает», попробуем написать нечто более интересное.  

Исходный код примера в папке "[.\demos\simple\](http://rouse.drkb.ru/blog/multitouch.zip)" в архиве с исходниками.  

#### 5\. Применяем мультач на практике.

Мой монитор держит 10 пальцев одновременно, можно даже написать приложение, эмулирующее рояль (правда в рояле есть еще педали и чувствительность к силе нажатия), но зачем идти сразу от сложного?  
Самое простое, что пришло мне в голову – это 10 квадратов на канвасе формы, которые я могу двигать во все стороны посредством тача.  
Этого вполне достаточно, чтобы «пощупать» мультитач в самом прямом смысле.  

Создаем новый проект.  

Каждый из квадратов будет описываться в виде такой структуры:  

    type
      TData = record
        Color: TColor;
        ARect, StartRect: TRect;
        StartPoint: TPoint;
        Touched: Boolean;
        TouchID: Integer;
      end;

По сути, самым важным полем данной структуры является TouchID, все остальное второстепенно.  

Нам нужно где-то хранить данные по каждому квадрату, поэтому объявим их в виде такого массива:  

    FData: array [0..9] of TData;

Ну, и выполним инициализацию:  

    procedure TdlgMultiTouchDemo.FormCreate(Sender: TObject);
    var
      I: Integer;
    begin
      DoubleBuffered := True;
      RegisterTouchWindow(Handle, 0);
      Randomize;
      for I := 0 to 9 do
      begin
        FData[I].Color := Random($FFFFFF);
        FData[I].ARect.Left := Random(ClientWidth - 100);
        FData[I].ARect.Top := Random(ClientHeight - 100);
        FData[I].ARect.Right := FData[I].ARect.Left + 100;
        FData[I].ARect.Bottom := FData[I].ARect.Top + 100;
      end;
    end;

А так же их отрисовку на канвасе формы (пока что не анализируйте обработчик FormPaint, мы дойдем до него чуть ниже):  

    procedure TdlgMultiTouchDemo.FormPaint(Sender: TObject);
    var
      I: Integer;
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
      for I := 0 to 9 do
      begin
        Canvas.Pen.Color := FData[I].Color xor $FFFFFF;
        if FData[I].Touched then
          Canvas.Pen.Width := 4
        else
          Canvas.Pen.Width := 1;
        Canvas.Brush.Color := FData[I].Color;
        Canvas.Rectangle(FData[I].ARect);
      end;
    end;

Запустите, получится как-то так:  

<div style="text-align:center;">![](https://habrastorage.org/files/988/a66/6de/988a666deea74cc99eb8457889b1cfa9.png)</div>

Обвес готов, теперь попробуем изменить картинку через обработку WM_TOUCH.  

Все что нам нужно в обработчике, это получить индекс квадрата, над которым пользователь нажал пальцем. Но для начала переведем координаты от каждой точки тача в координаты окна:  

    pt.X := TOUCH_COORD_TO_PIXEL(Inputs[I].x);
    pt.Y := TOUCH_COORD_TO_PIXEL(Inputs[I].y);
    pt := ScreenToClient(pt);

Имея на руках валидные координаты, мы можем узнать индекс квадрата в массиве, посредством вызова PtInRect.  

    function GetIndexAtPoint(pt: TPoint): Integer;
    var
      I: Integer;
    begin
      Result := -1;
      for I := 0 to 9 do
        if PtInRect(FData[I].ARect, pt) then
        begin
          Result := I;
          Break;
        end;
    end;

Когда пользователь только коснулся пальцем тачскрина (учитывая что каждая точка, обладает своим уникальным ID), мы присвоим найденному квадрату данный ID. Пригодится в дальнейшем:  

    if Inputs[I].dwFlags and TOUCHEVENTF_DOWN <> 0 then
    begin
      Index := GetIndexAtPoint(pt);
      if Index < 0 then Continue;
      FData[Index].Touched := True;
      FData[Index].TouchID := Inputs[I].dwID;
      FData[Index].StartRect := FData[Index].ARect;
      FData[Index].StartPoint := pt;
      Continue;
    end;

Это, скажем так, инициализация объекта и начало сессии тача.  

Следующее сообщение, которое мы получим, скорее всего будет WM_TOUCH с флагом TOUCHEVENTF_MOVE.  

**Тут нюанс:**  
В первом случае мы искали квадраты по их координатам, а сейчас это будет ошибкой, хотя бы потому, что позиции квадратов на форме могут пересекаться.  
Поэтому, в случае MOVE, мы будем искать квадраты по ID тача, который был выставлен через параметр TouchID:  

    function GetIndexFromID(ID: Integer): Integer;
    var
      I: Integer;
    begin
      Result := -1;
      for I := 0 to 9 do
        if FData[I].TouchID = ID then
        begin
          Result := I;
          Break;
        end;
    end;

Найдя необходимый нам квадрат, делаем подвижку, ориентируясь на структуру заданную в начале тач сессии:  

    R := FData[Index].StartRect;
    OffsetRect(R,
      pt.X - FData[Index].StartPoint.X,
      pt.Y - FData[Index].StartPoint.Y);
    FData[Index].ARect := R;

Ну, и концовка в виде обработки флага TOUCHEVENTF_UP:  

    if Inputs[I].dwFlags and TOUCHEVENTF_UP <> 0 then
    begin
      FData[Index].Touched := False;
      FData[Index].TouchID := -1;
      Continue;
    end;

В которой мы отключаем квадрат от тач сессии и перерисовываем сам канвас.  

Крайне простой примерчик, который, однако, работает и денег не просит.  
Запускайте и тестируйте – получается достаточно забавно:  

<iframe width="420" height="315" src="https://www.youtube.com/embed/1_BF8AaZkFY?feature=oembed" frameborder="0" allowfullscreen=""></iframe>  

Просто для «красявости», параметр Touched структуры TData, используется внутри FormPaint и отвечает за присутствие «жирной» рамки вокруг перемещаемого квадрата.  

Исходный код примера в папке "[.\demos\multutouch\](http://rouse.drkb.ru/blog/multitouch.zip)" в архиве с исходниками.  

#### 6\. Разбираемся с Gesture (жестами)

Мультитач – это только первый шажок, ибо нам хотелось бы работать с мультач-жестами, но…  
Давайте, для начала, посмотрим как реализовано в VCL распознавание жеста на основе одной Touch сессии (одним пальцем).  

За это отвечает класс TGestureEngine от которого потребуется, в принципе, только код функции IsGesture().  

Рассмотрим ее поподробнее:  

Она разделена ровно на две части, где первая часть проверяет стандартные жесты в цикле:  

    // Process standard gestures
    if gtStandard in GestureTypes then

А вторая – некие кастомные жесты, переданные пользователем:  

    // Process custom gestures
    if CustomGestureTypes * GestureTypes = CustomGestureTypes then

Так как кастомные пользовательские жесты нам по определению не нужны, рассмотрим только первую часть функции.  
Основная ее идея выглядит в виде поиска описателя жеста через вызов FindStandardGesture и сравнением его с переданным маршрутом посредством Recognizer.Match.  

Все остальные параметры, приходящие в IsGesture, по сути, можно исключить – они являются обвесом функции.  

Фишка в том, что Recognizer – это не интерфейс IGestureRecognizer, а VCL обертка.  
Вот она-то нам и нужна.  

Но прежде чем перейти к написанию демопримера, нужно разобраться с тем, что из себя представляет сам жест (Gerture):  

Это структура вида:  

    TStandardGestureData = record
      Points: TGesturePointArray;
      GestureID: TGestureID;
      Options: TGestureOptions;
      Deviation: Integer;
      ErrorMargin: Integer;
    end;

**Points** – это маршрут жеста, с которым сравнивается аналогичный маршрут из touch сессии пользователя.  
**GestureID** – уникальный идентификатор жеста.  

<div class="spoiler">**В ХЕ4 они перечислены в модуле Vcl.Controls:**

<div class="spoiler_text">

    const
      // Standard gesture id's
      sgiNoGesture       = 0;
      sgiLeft            = 1;
      sgiRight           = 2;
    ...

</div>

</div>

**Options** — в данном случае они нам не интересны.  

**Deviation и ErrorMargin** – параметры, указывающие величину, скажем так: «тремора» пальца в процессе жеста. Вряд ли вы сможете провести идеально ровную линию по оси Х влево без изменения позиции по оси Y, поэтому Deviation и ErrorMargin указывают на границы, в рамках которых перемещения точки будут валидны.  

<div class="spoiler">**Декларации параметров стандартных жестов можно найти в модуле Vcl.Touch.Gestures:**

<div class="spoiler_text">

    { Standard gesture definitions }

    const
      PDefaultLeft: array[0..1] of TPoint = ((X:200; Y:0), (X:0; Y:0));

      CDefaultLeft: TStandardGestureData = (
        GestureID: sgiLeft;
        Options: [goUniDirectional];
        Deviation: 30;
        ErrorMargin: 20);

      PDefaultRight: array[0..1] of TPoint = ((X:0; Y:0), (X:200; Y:0));

      CDefaultRight: TStandardGestureData = (
        GestureID: sgiRight;
        Options: [goUniDirectional];
        Deviation: 30;
        ErrorMargin: 20);

      PDefaultUp: array[0..1] of TPoint = ((X:0; Y:200), (X:0; Y:0));

      CDefaultUp: TStandardGestureData = (
        GestureID: sgiUp;
        Options: [goUniDirectional];
        Deviation: 30;
        ErrorMargin: 20);

    ...

</div>

</div>

Таким образом, зная о формате жестов, мы можем самостоятельно в рантайме подготовить собственный вариант жеста, заполнив его маршрут (Points) и выставив уникальный ID.  
Впрочем, сейчас нам это не понадобится. Посмотрим, что можно сделать на основе стандартных жестов.  

Пишем самый простой пример, при помощи которого Recognizer возвратит нам ID опознанного им жеста, в котором мы построим 4 массива точек, которые технически будут похожи на те маршруты, которые пользователь будет вводить посредством тачскрина.  

<div class="spoiler">**Например вот такой:**

<div class="spoiler_text">

    program recognizer_demo;

    {$APPTYPE CONSOLE}

    {$R *.res}

    uses
      Windows,
      Vcl.Controls,
      SysUtils,
      TypInfo,
      Vcl.Touch.Gestures;

    type
      TPointArray = array of TPoint;

    function GetGestureID(Value: TPointArray): Byte;
    var
      Recognizer: TGestureRecognizer;
      GestureID: Integer;
      Data: TStandardGestureData;
      Weight, TempWeight: Single;
    begin
      Weight := 0;
      Result := sgiNone;
      Recognizer := TGestureRecognizer.Create;
      try
        for GestureID := sgiLeft to sgiDown do
        begin
          FindStandardGesture(GestureID, Data);
          TempWeight := Recognizer.Match(Value, Data.Points, Data.Options,
            GestureID, Data.Deviation, Data.ErrorMargin);
          if TempWeight > Weight then
          begin
            Weight := TempWeight;
            Result := GestureID;
          end;
        end;
      finally
        Recognizer.Free;
      end;
    end;

    const
      gesture_id: array [sgiNone..sgiDown] of string =
        (
          'sgiNone',
          'sgiLeft',
          'sgiRight',
          'sgiUp',
          'sgiDown'
        );
    var
      I: Integer;
      Data: TPointArray;
    begin
      SetLength(Data, 11);
      // якобы делаем жест вправо
      for I := 0 to 10 do
      begin
        Data[I].X := I * 10;
        Data[I].Y := 0;
      end;
      Writeln(gesture_id[GetGestureID(Data)]);
      // якобы делаем жест влево
      for I := 0 to 10 do
      begin
        Data[I].X := 500 - I * 10;
        Data[I].Y := 0;
      end;
      Writeln(gesture_id[GetGestureID(Data)]);
      // якобы делаем жест вверх
      for I := 0 to 10 do
      begin
        Data[I].X := 0;
        Data[I].Y := 500 - I * 10;
      end;
      Writeln(gesture_id[GetGestureID(Data)]);
      // якобы делаем жест вниз
      for I := 0 to 10 do
      begin
        Data[I].X := 0;
        Data[I].Y := I * 10;
      end;
      Writeln(gesture_id[GetGestureID(Data)]);
      Readln;
    end.

</div>

</div>

После запуска должны увидеть следующую картинку:  

<div style="text-align:center;">![](//habrastorage.org/files/4f7/82b/d7b/4f782bd7b22749a193937dbb06fa71cf.png)</div>

Что и предполагалось.  
Исходный код примера в папке "[.\demos\recognizer\](http://rouse.drkb.ru/blog/multitouch.zip)" в архиве с исходниками.  

А теперь…  

#### 7\. Распознаем мультитач жесты (Gestures)

Данная глава описывает основную идею данной статьи, скажем так – фишку, ради которой и появился весь это текст.  
Сейчас – никаких технических деталей, только сам подход:  

**Итак, что нам сейчас доступно:**  

1.  Мы знаем как снимать данные с каждой тач-сессии;
2.  Мы можем распознавать жест каждой тач-сессии.

**К примеру:**  

1.  Пользователь нажал пальцем на тачскрин и провел влево;
2.  Мы зафиксировали начало сессии в обработчике ON_TOUCH + TOUCHEVENTF_DOWN, записали все точки маршрута по приходу TOUCHEVENTF_MOVE и в тот момент, когда нам пришел TOUCHEVENTF_UP, передали ранее записанный массив точек функции GetGestureID;
3.  Вывели результат.

Но представьте, что пользователь сделал все то же самое, только двумя пальцами одновременно:  

1.  Для каждого пальца мы стартуем собственную сессию;
2.  Пишем ее маршрут;
3.  По завершении каждой сессии передаем ее на распознание жеста.

Если ID жестов от двух сессий, произведенных над одним и тем-же окном, совпадут (к примеру, это будет sgiLeft), то мы можем сделать вывод – произошел свайп влево двумя пальцами.  

А что если все точки маршрута сессии содержат одни и те же координаты?  
Тогда жеста не было и произошел так называемый тап (одним или многими пальцами).  
Причем под данное условие попадет также жест «Press And Tap», при помощи которого обычно отображают PopupMenu.  

Таким образом, с учетом основной постановки задачи, мы можем контролировать все требуемые нам варианты жестов одним, двумя и тремя пальцами (впрочем, хоть всеми десятью).  

А что делать, если жесты от двух сессий не совпали?  
Анализировать их, и хотя в текущую постановку задачи это не входит, можно с уверенностью сказать, что жест sgiLeft от первой сессии плюс жест sgiRight от второй, может трактоваться как Zoom. Даже Rotate вполне возможно задетектировать на основе жестов sgiSemiCircleLeft или sgiSemiCircleRight только на основе двух тач сессий.  

Прониклись?  

Вот дефолтовый список жестов, которые таким образом легко можно эмулировать:  
[Windows Touch Gestures Overview](http://msdn.microsoft.com/en-us/library/windows/desktop/dd940543(v=vs.85).aspx)  

К сожалению, почему-то все это не реализовано в ХЕ4 и стало доступно только начиная с седьмой версии (и то не уверен что полностью).  

#### 8\. Техническое планирование движка

С теоретической частью закончили, теперь пришла пора все это применить на практике и сразу рассмотреть несколько проблем, встающих перед разработчиком.  

**Проблема номер раз:**  
В приложении обычно сотни окон – большинству из них достаточно того, что система генерирует при таче сообщения плана WM_LBUTTONCLICK и прочие, которых для нормального поведения окна достаточно (к примеру для кнопок, эдитов, скролов), но вот для того же SysListView32 скролирование, посредством жеста двумя пальцами, не происходит, ввиду отсутствия генерации сообщения WM_SCROLL. А ведь есть еще и кастомные контролы.  
Расширять оконную процедуру каждого окна – слишком много работы, поэтому нужно как-то определиться – какие окна должны поддерживать мультитач, причем сделать это необходимо наиболее универсально.  
Отсюда следует: нужен некий менеджер мультитача, в котором окна будут регистрироваться и который будет отвечать за всю работу с мультитачем.  

**Проблема номер два:**  
Раз мы пишем нечно универсальное, не переписывая каждый экземпляр TWinControl, то необходимо как-то отслеживать пересоздание окна, благо вызовы RecreateWnd один из штатных механизмом VCL. Если мы не будем этого делать, то при первом же пересоздании окна, ранее зарегистрированный нами TWinControl, перестанет получать сообщения WM_TOUCH и, таким образом, вся работа нашего менеджера будет нивелирована.  

**Проблема номер три:**  
Менеджер должен хранить все данные о тач-сессиях и уметь обрабатывать ситуации срыва начала и конца сессий (ибо не всегда приходят уведомления c флагами Down и Up), причем необходимо учитывать, что длина сессии может быть продолжительна по времени, что влечет за собой достаточно большой расход памяти, если сохранять все точки маршрута сессии.  

Еще хотелось бы чтобы менеджер мультитача мог различать жесты в рамках разных окон.  
К примеру – если пользователь поставил два пальца в левое окно и два пальца в правое (четыре мультитач сессии), после чего соединил пальцы в центре, левому окну должно прийти уведомление о двупальцевом жесте вправо, а правому о двупальцевом жесте влево.  

<div style="text-align:center;">![](//habrastorage.org/files/a7c/2d2/ff3/a7c2d2ff330f4a1d829d59802141b19b.png)</div>

Но, к сожалению, это не получится, т.к. сообщение WM_TOUCH будет приходить только тому окну, в котором началась сессия, остальные окна будут игнорироваться.  

#### 9\. Строим базовый каркас мультитач движка

Для начала определимся с нюансами реализации класса.  
Технически, самое удобное, с точки зрения внешнего программиста, будет реализация некоего универсального движка, который возьмет на себя всю работу и будет уведомлять разработчика разве что вызовом финальных событий.  

В таком случае, разработчику придется только единожды зарегистрировать нужное окно в движке и анализировать поступающие от него жесты (направленные конкретному окну), обрабатывая нужные. К примеру, эмулируя тот же скролл двупальцевым жестом.  

Сам движок будет реализован ввиде синглтона.  
**Во первых:** нет смысла плодить инстансы класса, которые всегда будут делать одно и тоже. Это не TStringList, заточенный под хранение данных, а все таки движок, реализующий единую логику работы для всех окон проекта.  
**А во вторых:** есть небольшой нюанс в реализации самого движка (о нем чуть позже), из-за которого реализация в виде синглтона будет самой простой, иначе придется кардинально переусложнять логику работы класса.  

Таким образом, движок должен предоставлять:  

1.  Методы регистрации окна и снятия окна с регистрации:
2.  Набор внешних событий, обработчики которых должен реализовать разработчик.

Внешние события могут быть примерно такими:  

**OnBeginTouch** — это событие будет вызываться при получении сообщения WM_TOUCH.  

Поясню: в четвертой главе был приведен следующий код:  

    // получаем количество точек тача
    InputsCount := Msg.WParam and $FFFF;

Т.е. реальных точек тача может быть несколько.  
Вот об их количестве мы и предупредим разработчика.  

**OnTouch** — в этом событии мы уведомим разработчика о данных, содержащихся в каждой структуре TTouchInput, только немного в более причесанном виде. (Переведем данные о точке в координаты окна, выставим правильные флаги и прочее, зачем нагружать разработчика избыточной информацией и заставлять его писать избыточный код?)  

**OnEndTouch** — этим мы скажем что цикл обработки сообщения WM_TOUCH завершен, можно, к примеру, вызвать Repaint.  

**OnGecture** — а это сообщение разработчик получит тогда, когда движок примет решение что жест распознан.  

Так как класс реализован в виде синглтона, а зарегистрированных в нем окон будет более чем одно, то объявить все четыре события в виде свойств класса не получится.  

Ну точнее как: можно конечно, но второе зарегистрированное окно сразу переназначит обработчики событий на себя и первому придется тихо курить в сторонке.  
Поэтому, помимо списка зарегистрированных окон, мы должны держать и обработчики событий движка, которые за ними закреплены.  

Впрочем, попробуем теперь это все реализовать на практике.  
Создайте новый проект и к нему добавьте новый модуль, с названием… ну, к примеру, SimpleMultiTouchEngine.  

Для начала объявим флаги, которые нам интересны при обработке WM_TOUCH:  

    type 
      TTouchFlag =
        (
          tfMove, // перемещаем точку
          tfDown, // создали току тача
          tfUp    // прекратили работу с точкой
        );
      TTouchFlags = set of TTouchFlag;

Опишем структуру, которую мы будем передавать на внешку разработчику о каждой точке:  

    TTouchData = record
      Index: Integer;      // порядковый номер точки в массиве TTouchInput
      ID: DWORD;           // неизменяющееся ID точки
      Position: TPoint;    // её координаты относительно окна
      Flags: TTouchFlags;  // флаги
    end;

Декларация события OnTouchBegin будет выглядеть так:  

    TTouchBeginEvent = procedure(Sender: TObject; nCount: Integer) of object;

А так будет выглядеть OnTouch:  

    TTouchEvent = procedure(Sender: TObject; Control: TWinControl;
      TouchData: TTouchData) of object;

Для OnEndTouch будет достаточно обычного TNotifyEvent.  

Данные о назначенных обработчиках событий, закрепленных за каждым зарегистрированным окном, будут хранится в такой структуре:  

    TTouchHandlers = record
      BeginTouch: TTouchBeginEvent;
      Touch: TTouchEvent;
      EndTouch: TNotifyEvent;
    end;

Декларируем новый класс:  

      TSimleMultiTouchEngine = class
      private const
        MaxFingerCount = 10;
      private type
        TWindowData = record
          Control: TWinControl;
          Handlers: TTouchHandlers;
        end;
      private
        FWindows: TList<TWindowData>;
        FMultiTouchPresent: Boolean;
      protected
        procedure DoBeginTouch(Value: TTouchBeginEvent; nCount: Integer); virtual;
        procedure DoTouch(Control: TWinControl; Value: TTouchEvent;
          TouchData: TTouchData); virtual;
        procedure DoEndTouch(Value: TNotifyEvent); virtual;
      protected
        procedure HandleTouch(Index: Integer; Msg: PMsg);
        procedure HandleMessage(Msg: PMsg);
      public
        constructor Create;
        destructor Destroy; override;
        procedure RegisterWindow(Value: TWinControl; Handlers: TTouchHandlers);
        procedure UnRegisterWindow(Value: TWinControl);
      end;

По порядку:  

Константа MaxFingerCount содержит максимальное количество точек тача, с которыми может работать наш класс.  

Структура TWindowData – содержит в себе зарегистрированное окно и список обработчиков, которые назначил программист.  

Поле FWindows: TList – список зарегистрированных окон и обработчиков, от которого мы и будем плясать на всем протяжении работы с класом.  

Поле FMultiTouchPresent – флаг, инициализирующийся в конструкторе класса.  
Содержит True, если наше железо держит мультитач. Опираясь на данный флаг будет отключаться часть логики класса (зачем делать лишние телодвижения тогда, когда мы их выполнить все равно не сможем?).  

Первая protected секция — просто для удобства вынесены все вызовы внешних событий.  

Процедура HandleTouch — основное ядро движка, именно она и отвечает за обработку сообщения WM_TOUCH.  

Процедура HandleMessage — вспомогательная. Ее задача определить к какому из зарегистрированных окон направлено сообщение и вызвать HandleTouch, передав индекс найденного окна.  

Паблик секция – конструктор, деструктор, регистрация окна и снятие его с регистрации.  

<div class="spoiler">**Прежде чем приступить к реализации класса, сразу напишем синглтон обвес:**

<div class="spoiler_text">

      function MultiTouchEngine: TSimleMultiTouchEngine;

    implementation

    var
      _MultiTouchEngine: TSimleMultiTouchEngine = nil;

    function MultiTouchEngine: TSimleMultiTouchEngine;
    begin
      if _MultiTouchEngine = nil then
        _MultiTouchEngine := TSimleMultiTouchEngine.Create;
      Result := _MultiTouchEngine;
    end;

    ...

    initialization

    finalization

      _MultiTouchEngine.Free;

    end.

</div>

</div>

<div class="spoiler">**И, в завершении всего, каллбэк ловушки, при помощи которой мы будем получать сообщения WM_TOUCH, отправленные зарегистрированным в движке окнам.**

<div class="spoiler_text">

    var
      FHook: HHOOK = 0;

    function GetMsgProc(nCode: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;
    begin
      if (nCode = HC_ACTION) and (WParam = PM_REMOVE) then
        if PMsg(LParam)^.message = WM_TOUCH then
          MultiTouchEngine.HandleMessage(PMsg(LParam));
      Result := CallNextHookEx(FHook, nCode, WParam, LParam);
    end;

</div>

</div>

<div class="spoiler">**На всякий случай, список используемых модулей выглядит так:**

<div class="spoiler_text">

    uses
      Windows,
      Messages,
      Classes,
      Controls,
      Generics.Defaults,
      Generics.Collections,
      Vcl.Touch.Gestures;

</div>

</div>

Ну а теперь пройдемся по реализации самого движка. Начнем, пожалуй, с конструктора.  

    constructor TSimleMultiTouchEngine.Create;
    var
      Data: Integer;
    begin
      // проверяем, есть ли поддержка мультитача
      Data := GetSystemMetrics(SM_DIGITIZER);
      FMultiTouchPresent :=
        (Data and NID_READY <> 0) and (Data and NID_MULTI_INPUT <> 0);

      // если нет, то и работать не с чем
      if not FMultiTouchPresent then Exit;

      // создаем список в котором будем хранить зарегистрированные окна
      FWindows := TList<TWindowData>.Create(
        // а чтобы IndexOf работал не по всей структуре а только по полю Control
        // дописываем свой компаратор
        TComparer<twindowdata>.Construct(
          function (const A, B: TWindowData): Integer
          begin
            Result := Integer(A.Control) - Integer(B.Control);
          end)
        );
    end;

Достаточно простенький конструктор без изысков, в комментариях видны все шаги.  
Впрочем и деструктор также прост:  

    destructor TSimleMultiTouchEngine.Destroy;
    begin
      if FHook <> 0 then
        UnhookWindowsHookEx(FHook);
      FWindows.Free;
      inherited;
    end;

Единственный нюанс деструктора – снятие ловушки, если она была ранее установлена.  
Теперь перейдем к реализации двух единственных публичных процедур, доступных разработчику извне.  

Регистрация окна в движке:  

    procedure TSimleMultiTouchEngine.RegisterWindow(Value: TWinControl;
      Handlers: TTouchHandlers);
    var
      WindowData: TWindowData;
    begin
      // если мультитач не поддерживается - выходим
      if not FMultiTouchPresent then Exit;

      // для того чтобы IndexOf отработал, инициализируем соответствующее поле структуры
      WindowData.Control := Value;

      // окно можно зарегистрировать только один раз,
      // повторная регистрация не поддерживается
      if FWindows.IndexOf(WindowData) < 0 then
      begin
        // запоминаем список обработчиков
        WindowData.Handlers := Handlers;
        // подключаем окно к тачу
        RegisterTouchWindow(Value.Handle, 0);
        // добавляем структуру к общему списку окон
        FWindows.Add(WindowData);
      end;
      // после добавления окна запускаем ловушку
      if FHook = 0 then
        FHook := SetWindowsHookEx(WH_GETMESSAGE, @GetMsgProc, HInstance, GetCurrentThreadId);
    end;

Все прокомментировано, впрочем единственный нюанс с вызовом IndexOf. Для того чтобы он работал не через CompareMem сравнивая две структуры между собой, а только по одному полю структуры (Control) и был реализован TComparer в конструкторе класса списка.  

Как можно увидеть из кода — логика проста, после добавления окна в общий список, класс стартует ловушку WH_GETMESSAGE (если она ранее не была запущена), причем работающую только в пределах текущей нити.  

Отдельно остановлюсь на переменной FMultiTouchPresent.  
Как видно из кода, она просто выполняет роль предохранителя, который отключает всю логику работы класса в том случае, если мы не можем сделать ничего полезного.  
Если убрать ее, то будет небольшой «overhead» в цикле выборки сообщений каждого окна нашего приложения из-за установленной ловушки в том случае, если наше «железо» вообще не имеет понятия о тачскрине. Оно нам надо?  

Снятие окна с регистрации идет по такому-же принципу, с отключением ловушки, если окон больше нет:  

    procedure TSimleMultiTouchEngine.UnRegisterWindow(Value: TWinControl);
    var
      Index: Integer;
      WindowData: TWindowData;
    begin
      // если мультитач не поддерживается - выходим
      if not FMultiTouchPresent then Exit;

      // для того чтобы IndexOf отработал, инициализируем соответствующее поле структуры
      WindowData.Control := Value;

      // ищем окно
      Index := FWindows.IndexOf(WindowData);
      if Index >= 0 then
        // если нашлось, удаляем окно из списка
        FWindows.Delete(Index);

      // если окон не осталось, то ловушка нам больше не нужна
      if FWindows.Count = 0 then
      begin
        // выключаем ее
        UnhookWindowsHookEx(FHook);
        FHook := 0;
      end;
    end;

Собственно вся логика движка проста: приняли окно на регистрацию, запустили ловушку, которая при получении сообщения WM_TOUCH вызывает процедуру HandleMessage, посредством обращения к синглтону класса.  

    procedure TSimleMultiTouchEngine.HandleMessage(Msg: PMsg);
    var
      I: Integer;
    begin
      for I := 0 to FWindows.Count - 1 do
        // ищем индекс окна, которому пришло сообщение
        if FWindows[I].Control.Handle = Msg^.hwnd then
        begin
          // и вызываем основной обработчик сообщения
          HandleTouch(I, Msg);
          Break;
        end;
    end;

И вот и центральная процедура класса, вокруг которой крутится вся логика работы:  

    procedure TSimleMultiTouchEngine.HandleTouch(Index: Integer; Msg: PMsg);
    var
      TouchData: TTouchData;
      I, InputsCount: Integer;
      Inputs: array of TTouchInput;
      Flags: DWORD;
    begin
      // Смотрим, сколько точек тача сейчас активно
      InputsCount := Msg^.wParam and $FFFF;
      if InputsCount = 0 then Exit;

      // Это количество не должно быть более максимально поддерживаемого значения
      if InputsCount > MaxFingerCount then
        InputsCount := MaxFingerCount;

      // получаем информацию по всем точкам тача
      SetLength(Inputs, InputsCount);
      if not GetTouchInputInfo(Msg^.LParam, InputsCount,
        @Inputs[0], SizeOf(TTouchInput)) then Exit;
      CloseTouchInputHandle(Msg^.LParam);

      // генерируем внешнее событие о начале процедуры
      // оповещения об изменениях в точках тача
      DoBeginTouch(FWindows[Index].Handlers.BeginTouch, InputsCount);

      for I := 0 to InputsCount - 1 do
      begin
        TouchData.Index := I;

        // в выдаваемой наружу структуре указываем ID каждой точки
        // она не меняется в течении всей сессии (от Down до Up)
        // и к ней можно делать привязку
        TouchData.ID := Inputs[I].dwID;

        // переводим координаты каждой точки в координаты окна
        TouchData.Position.X := TOUCH_COORD_TO_PIXEL(Inputs[I].x);
        TouchData.Position.Y := TOUCH_COORD_TO_PIXEL(Inputs[I].y);
        TouchData.Position :=
          FWindows[Index].Control.ScreenToClient(TouchData.Position);

        // заполняем выставленные флаги
        TouchData.Flags := [];
        Flags := Inputs[I].dwFlags;
        if Flags and TOUCHEVENTF_MOVE <> 0 then
          Include(TouchData.Flags, tfMove);
        if Flags and TOUCHEVENTF_DOWN <> 0 then
          Include(TouchData.Flags, tfDown);
        if Flags and TOUCHEVENTF_UP <> 0 then
          Include(TouchData.Flags, tfUp);

        // генерируем внешнее событие о изменении в состоянии каждой конкретной точки
        DoTouch(FWindows[Index].Control,
          FWindows[Index].Handlers.Touch, TouchData);
      end;

      // генерируем внешнее событие о завершении процедуры
      // оповещения об изменениях в точках тача
      DoEndTouch(FWindows[Index].Handlers.EndTouch);
    end;

Все это мы уже видели в пятой главе статьи, поэтому давать дополнительные пояснения по коду, смысла не имеет. Перейдем к работе с получившимся мультитач движком.  

Исходный код модуля SimleMultiTouchEngine.pas в папке "[.\demos\multitouch_engine_demo\](http://rouse.drkb.ru/blog/multitouch.zip)" в архиве с исходниками.  

#### 10\. Работаем с TSimleMultiTouchEngine

Придумывать что-то новое не будем и попробуем воспроизвести проект из пятой главы, в котором основным изменением будет то, что поддержку мультитача будет осуществлять TSimleMultiTouchEngine.  

В проект, созданный в девятой главе, добавьте декларацию структуры TData и массива FData из пятой главы, а также скопируйте обработчик FormPaint. Это все останется без изменений.  

Объявим два обработчика:  

    procedure OnTouch(Sender: TObject;
      Control: TWinControl; TouchData: TTouchData);
    procedure OnTouchEnd(Sender: TObject);

В используемые модули подключим SimleMultiTouchEngine и немного изменим конструктор класса:  

    procedure TdlgMultiTouchEngineDemo.FormCreate(Sender: TObject);
    var
      I: Integer;
      Handlers: TTouchHandlers;
    begin
      DoubleBuffered := True;
      // RegisterTouchWindow(Handle, 0);
      Randomize;
      for I := 0 to 9 do
      begin
        FData[I].Color := Random($FFFFFF);
        FData[I].ARect.Left := Random(ClientWidth - 100);
        FData[I].ARect.Top := Random(ClientHeight - 100);
        FData[I].ARect.Right := FData[I].ARect.Left + 100;
        FData[I].ARect.Bottom := FData[I].ARect.Top + 100;
      end;
      ZeroMemory(@Handlers, SizeOf(TTouchHandlers));
      Handlers.Touch := OnTouch;
      Handlers.EndTouch := OnTouchEnd;
      MultiTouchEngine.RegisterWindow(Self, Handlers);
    end;

Изменения, по сути, минимальны, вместо вызова RegisterTouchWindow, мы перекладываем работу на только что реализованный нами MultiTouchEngine.  

Обработчик OnTouchEnd простой:  

    procedure TdlgMultiTouchEngineDemo.OnTouchEnd(Sender: TObject);
    begin
      Repaint;
    end;

Просто вызываем перерисовку всей канвы.  

А теперь посмотрим, во что превратился код в обработчике OnTouch (ранее реализованный в обработчике WmTouch):  

    procedure TdlgMultiTouchEngineDemo.OnTouch(Sender: TObject;
      Control: TWinControl; TouchData: TTouchData);

      function GetIndexAtPoint(pt: TPoint): Integer;
      var
        I: Integer;
      begin
        Result := -1;
        for I := 0 to 9 do
          if PtInRect(FData[I].ARect, pt) then
          begin
            Result := I;
            Break;
          end;
      end;

      function GetIndexFromID(ID: Integer): Integer;
      var
        I: Integer;
      begin
        Result := -1;
        for I := 0 to 9 do
          if FData[I].TouchID = ID then
          begin
            Result := I;
            Break;
          end;
      end;

    var
      Index: Integer;
      R: TRect;
    begin
      if tfDown in TouchData.Flags then
      begin
        Index := GetIndexAtPoint(TouchData.Position);
        if Index < 0 then Exit;
        FData[Index].Touched := True;
        FData[Index].TouchID := TouchData.ID;
        FData[Index].StartRect := FData[Index].ARect;
        FData[Index].StartPoint := TouchData.Position;
        Exit;
      end;

      Index := GetIndexFromID(TouchData.ID);
      if Index < 0 then Exit;

      if tfUp in TouchData.Flags then
      begin
        FData[Index].Touched := False;
        FData[Index].TouchID := -1;
        Exit;
      end;

      if not (tfMove in TouchData.Flags) then Exit;

      if not FData[Index].Touched then Exit;

      R := FData[Index].StartRect;
      OffsetRect(R,
        TouchData.Position.X - FData[Index].StartPoint.X,
        TouchData.Position.Y - FData[Index].StartPoint.Y);
      FData[Index].ARect := R;
    end;

Идеология практически не изменилась, но читается он гораздо проще чем в старом варианте.  
И самое главное: работает также как и код из пятой главы.  

<iframe width="420" height="315" src="https://www.youtube.com/embed/LCEgn3ybpN4?feature=oembed" frameborder="0" allowfullscreen=""></iframe>  
Исходный код примера в папке "[.\demos\multitouch_engine_demo\](http://rouse.drkb.ru/blog/multitouch.zip)" в архиве с исходниками.  

Так в чем-же цимус, скорее всего спросите вы. Ведь размер кода в главной форме и алгоритм его работы практически не изменился, плюс до кучи появился дополнительный модуль аж на 277 строчек кода (с коментариями) в виде SimleMultiTouchEngine.pas.  
Может проще оставить как есть и реализовывать обработчик WM_TOUCH самостоятельно только там, где это действительно необходимо?  

В принципе так то оно — так, правда этот движок решает только первую задачу из трех, озвученных в восьмой главе.  

И цимус заключается в следующем…  

#### 11\. Включаем в движок поддержку жестов

В реализованном выше MultiTouchEngine нет решения остальных двух пунктов из запланированных проблем, без решения которых он превращается просто в лишний класс в иерархии проекта (конечно теперь этот класс может обеспечить мультитачем всех страждущих, но сути это не меняет).  

Начнем сразу с проблемы за номером три.  

Для начала объявим типы распознаваемых движком жестов и обработчик внешнего события:  

    // типы распознаваемых жестов
    TGestureType =
      (
        gtNone,                             // жест не распознан
        gtTap, gt2Tap, gt3Tap,              // обычные тапы (1, 2, 3 пальца)
        gtLeft, gtRight, gtUp, gtDown,      // свайп в стороны одним пальцем
        gt2Left, gt2Right, gt2Up, gt2Down,  // свайп в стороны двумя пальцами
        gt3Left, gt3Right, gt3Up, gt3Down   // свайп в стороны тремя пальцами
      );

    // декларация обработчика распознанных жестов
    TGestureEvent = procedure(Sender: TObject; Control: TWinControl;
      GestureType: TGestureType; Position: TPoint; Completed: Boolean) of object;

Наш класс должен будет уметь распознавать 15 различных жестов (если не считать gtNone).  

Обратите внимание на параметр Completed в декларации TGestureEvent. Этот флаг будет сообщать разработчику о завершении жеста (приходе сообщения WM_TOUCH + TOUCHEVENTF_UP).  

Для чего это сделано: к примеру пользователь нажал на тачскрин двумя пальцами и повел их влево, по идее необходимо скролировать окно, но если ждать окончания жеста, то правильного скролирования не получится, поэтому движок мультитача будет периодически генерировать внешнее событие OnGesture в котором можно будет произвести необходимый скрол прямо во время сессии тача. Именно в этом обработчике разработчик сможет понять по параметру Completed — завершен жест или нет (к примеру если нам приходит gtTap, а параметр Completed выставлен в False, то пока что делать ничего не надо и стоит подождать окончания).  

Частота, с которой будет генерироваться событие OnGesture в процессе сессии, напрямую зависит от константы GesturePartSize, которую я установил в 10\. Т.е. как только количество точек сессии стало кратно константе (остаток от деления по модулю равен нулю), генерируется событие.  

Данные каждой сессии будут хранится вот в таком массиве:  

    TPointArray = array of TPoint;

Ну а структуру, описывающую каждую сессию, задекларируем вот так:  

        TGestureItem = record
          ID,                      // ID тача, по которому собирается информация
          ControlIndex: Integer;   // индекс окна, в котором происходит событие тача
          Data: TList<TPoint>;     // массив точек, по которым прошел тач в течении сессии
          Done: Boolean;           // флаг указывающий на завершение сессии
        end;

<div class="spoiler">**Осталось, разве что, объявить класс, который будет хранить в себе данные по каждой тач-сессии:**

<div class="spoiler_text">

    // класс, хранящий в себе данные о всех сессиях мультитача
    // поддерживает одновременно 10 сессий
    TGesturesData = class
      ...
    strict private
      // массив данных для каждой сессии
      FData: array [0..MaxFingerCount - 1] of TGestureItem;
      ...
    public
      ...
      // старт сессии
      procedure StartGesture(ID, ControlIndex: Integer; Value: TPoint);
      // добавление новой точки в сессию
      function AddPoint(ID: Integer; Value: TPoint): Boolean;
      // завершение сессии
      procedure EndGesture(ID: Integer);
      // очистка ресурсов всех сессий ассоциированных с указаным окном
      procedure ClearControlGestures(ControlIndex: Integer);
      // функция возвращает маршрут точек сессии в виде массива
      function GetGesturePath(ID: Integer): TPointArray;
      // индекс окна по которому будет сгенерированно события OnEndAllGestures и OnPartComplete
      property LastControlIndex: Integer read FLastControlIndex;
      // событие о завершении всех сессий ассоциированных с окном LastControlIndex
      property OnEndAllGestures: TGesturesDataEvent read FEndAll write FEndAll;
      // событие о достижении лимита GesturePartSize для всех сессий ассоциированных с окном LastControlIndex
      property OnPartComplete: TGesturesDataEvent read FPart write FPart;
    end;

</div>

</div>

Это в действительности крайне простой класс, не содержащий никаких особых изысков, поэтому рассматривать реализацию каждой функции не будем, благо вы все сможете увидеть в [примере демопроекта](http://rouse.drkb.ru/blog/multitouch.zip), идущего вместе со статьей.  

Вся его задача, это:  

1.  хранить поступающие извне данные, через вызовы StartGesture и AddPoint;
2.  после каждого вызова AddPoint, проверять размер списка

    <pre>Data: TList<TPoint></pre>

    для каждой сессии, ассоциированной с окном ControlIndex и, по необходимости, вызывать OnPartComplete;
3.  после вызова EndGesture, проверять все сессии с одинаковым ControlIndex и, если они все завершены, вызывать OnEndAllGestures.

Это просто хранилище сессий для нашего движка и с хранимым им данными будет работать TGestureRecognizer.  

<div class="spoiler">**Расширим наш базовый класс, добавив следующие два поля:**

<div class="spoiler_text">

    // класс для хранения данных по каждой сессии мультитача
    FGesturesData: TGesturesData;
    // класс распознает жекст в рамках одной сессии
    FGestureRecognizer: TGestureRecognizer;

</div>

</div>

<div class="spoiler">**В конструкторе создадим и проинициализируем наше хранилище сессий:**

<div class="spoiler_text">

    FGesturesData := TGesturesData.Create;
    FGesturesData.OnEndAllGestures := OnEndAllGestures;
    FGesturesData.OnPartComplete := OnPartComplete;
    FGestureRecognizer := TGestureRecognizer.Create;

</div>

</div>

После чего вернемся обратно в методу HandleTouch(), где нам будет необходимо немного расширить код, который отвечал за установку флагов в структуре TouchData:  

    TouchData.Flags := [];
    Flags := Inputs[I].dwFlags;
    if Flags and TOUCHEVENTF_MOVE <> 0 then
    begin
      Include(TouchData.Flags, tfMove);
      // идет процетура перемещения точки, добавляем новые координаты
      // к сесии, ассоциированной с данной точкой
      if not FGesturesData.AddPoint(TouchData.ID, TouchData.Position) then
        // а если вдруг такая сессия отсутствует, то создаем ее
        FGesturesData.StartGesture(TouchData.ID, Index, TouchData.Position);
    end;
    if Flags and TOUCHEVENTF_DOWN <> 0 then
    begin
      Include(TouchData.Flags, tfDown);
      // пользователь только что нажал на тачустройство,
      // стартуем новую сессию с уникальным ID
      FGesturesData.StartGesture(TouchData.ID, Index, TouchData.Position);
    end;
    if Flags and TOUCHEVENTF_UP <> 0 then
    begin
      Include(TouchData.Flags, tfUp);
      // пользователь отпустил палец и завершил работу с сесией
      // то-же самое делаем и мы.
      // если все сессии связанные с текущим окном завершены,
      // то FGesturesData поднимет внутреннее событие о завершении всех сессий
      FGesturesData.EndGesture(TouchData.ID);
    end;

Собственно, это практически вся активная работа с хранилищем данных для каждой сессии.  

<div class="spoiler">**Обработчики событий о завершении тач-сессии и частичном завершении достаточно простые:**

<div class="spoiler_text">

    //
    //  Пришло внутренне событие о достижении лимита у всех сессий ассоциированным с окном
    //  Параметр Values содержит ID всех сессий,
    //  данные с которых будут использоваться для распознования жест
    // =============================================================================
    procedure TTouchManager.OnPartComplete(Values: TBytes);
    var
      Position: TPoint;
      GestureType: TGestureType;
    begin
      // смотрим что за жест у нас получился?
      GestureType := RecognizeGestures(Values, Position);
      // если жест рапознан, то генерируем внешнее событие
      if GestureType <> gtNone then
        DoGesture(
          FWindows[FGesturesData.LastControlIndex].Control,
          FWindows[FGesturesData.LastControlIndex].Handlers.Gesture,
          GestureType, Position,
          // с указанием флага что жест еще продолжается и пока распознан частично
          False);
    end;

и второй вариант, который практически ничем не отличается от первого:  

    //
    //  Пришло внутренне событие о завершении всех сессий ассоциированным с окном
    //  Параметр Values содержит ID всех завершенных сессий
    // =============================================================================
    procedure TTouchManager.OnEndAllGestures(Values: TBytes);
    var
      Position: TPoint;
      GestureType: TGestureType;
    begin
      try
        // смотрим что за жест у нас получился?
        GestureType := RecognizeGestures(Values, Position);
        // если жест рапознан, то генерируем внешнее событие
        if GestureType <> gtNone then
          DoGesture(
            FWindows[FGesturesData.LastControlIndex].Control,
            FWindows[FGesturesData.LastControlIndex].Handlers.Gesture,
            GestureType, Position,
            // с указанием флаза что жест завершен
            True);
      finally
        // в конце очищаем все данные по сессиям
        FGesturesData.ClearControlGestures(FGesturesData.LastControlIndex);
      end;
    end;

Единственно его отличие в том, что освобождаются ресурсы, занятые данными от сессий ассоциированных с окном, над которым жест был распознан.  
</div>

</div>

Ну и как понятно по коду обработчиков: вся основная работа идет в функции RecognizeGestures, логику которой я уже описал в седьмой главе.  

Выглядит она вот так:  

    //
    //  Функция распознающая тип жеста на основе маршрутов сессий из TGesturesData
    //  Параметр Values содержит ID всех сессий,
    //  данные с которых будут использоваться для распознования жест
    // =============================================================================
    function TTouchManager.RecognizeGestures(Values: TBytes;
      var Position: TPoint): TGestureType;
    var
      I, A, ValueLen, GestureLen: Integer;
      GestureID: Byte;
      GesturePath: TPointArray;
      NoMove: Boolean;
    begin
      Result := gtNone;

      // смотрим количество сессий, по которым будем рапознавать жест
      ValueLen := Length(Values);

      // если их больше трех (четыре и больше точки тача), то выходим
      if ValueLen > 3 then Exit;

      // общая идея такова:
      // мы можем распознать ID жеста по одной точке (вызовом GetGestureID),
      // к примеру пусть это будет sgiLeft
      // то в случае двух и более точек, если у каждой из них ID жеста совпал с первой,
      // считаем что этот тот-же жест только выполненный двумя или тремя пальцами
      // Единственный нюанс в рапознавании тапов двумя и более пальцами
      // Для этого смотрятся все координаты маршрута по каждой точке
      // и если эти координаты на всем протяжении не менялись,
      // то считаем что операции перемещения не было и произошел обычный тап

      GestureID := sgiNoGesture;
      NoMove := True;

      for I := 0 to ValueLen - 1 do
      begin

        // итак, получаем маршрут сессии в виде массива TPoint
        GesturePath := FGesturesData.GetGesturePath(Values[I]);
        GestureLen := Length(GesturePath);

        // Если один из маршрутов пуст - выходим, ибо пока что еще нечего распознавать
        if GestureLen = 0 then Exit;

        // детектируем отсутствие перемещения всех точек тача
        if NoMove then
          for A := 1 to GestureLen - 1 do
            if GesturePath[0] <> GesturePath[A] then
            begin
              NoMove := False;
              Break;
            end;

        // получаем координаты события.
        // вот тут конечно не верно. ибо если точек две и более, то координаты
        // берутся только у самой последней, но как бы это более правильно сделать я не знаю
        Position := GesturePath[GestureLen - 1];

        // запоминаем ID жеста для первой точки
        if I = 0 then
          GestureID := GetGestureID(GesturePath)
        else
          // и сравниваем ID жестов для всех остальных точек, они должны совпадать
          if GestureID <> GetGestureID(GesturePath) then
            Exit;
      end;

      // ну и на основе ID жеста и количества точек генерируем свой собственный результат
      if (GestureID = sgiNoGesture) then
      begin
        if NoMove then
          case ValueLen of
            1: Result := gtTap;
            2: Result := gt2Tap;
            3: Result := gt3Tap;
          end;
      end
      else
      begin
        Dec(ValueLen);
        Result := TGestureType(3 + GestureID + ValueLen * 4);
      end;

    end;

Этой функции требуется вспомогательная GetGestureID, аналог которой уже был показан в шестой главе.  

После всех этих манипуляций можно сказать, что проблема за номером 3, озвученная в восьмой главе, решена. То есть: мы умеем хранить данные о каждой сессии и, более того, знаем над каким окном она проводится.  

Осталось совсем немного – проблема номер два.  

#### 12\. Детектируем пересоздание окна

Как я ранее говорил, вызов RecreateWnd, по сути, является штатным механизмом VCL.  
Однако он может сильно попортить всю логику работы нашего движка, т.к. при пересоздании окна, пока что никто не производит повторный вызов RegisterTouchWindow на вновь созданный хэндл. Таким образом, хоть окно и продолжает быть зарегистрированным в движке, сообщения WM_TOUCH перестают ему приходить.  

Подойти к решению этой задачи можно несколькими способами: к примеру, раз уж мы установили ловушку, то почему бы не отлавливать сообщения WM_CREATE/WM_DESTROY до кучи к WM_TOUCH?  

А вот не хочу, ибо таких сообщений в рамках GUI потока будет море, а зачем нам ненужный оверхед в цикле выборки сообщений?  

Поэтому зайдем с другой стороны и напишем некий проксик, который будет представлять из себя невидимое окно, которому родителем будет выставлено окно, за которым мы должны следить. В этом случае, при разрушении основного окна, разрушится и окно нашего проксика, что можно задетектировать в обработчике DestroyHandle, а создание окна, после его разрушения поймаем в CreateWnd, где уже будет доступен валидный хэндл родителя, которому можно сказать RegisterTouchWindow, подключив его обратно к получению сообщений WM_TOUCH.  

Выглядит это безобразие следующим образом:  

    type
      // класс следящий за пересозданием зарегистрированного окна
      TWinControlProxy = class(TWinControl)
      protected
        procedure DestroyHandle; override;
        procedure CreateWnd; override;
        procedure CreateParams(var Params: TCreateParams); override;
      end;

    { TWinControlProxy }

    //
    //  При создании выставляем стиль WS_EX_TRANSPARENT, мы будем не заметными.
    // =============================================================================
    procedure TWinControlProxy.CreateParams(var Params: TCreateParams);
    begin
      inherited;
      Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
    end;

    //
    //  При создании окна сразу включаем ему поддержку тача
    // =============================================================================
    procedure TWinControlProxy.CreateWnd;
    begin
      inherited CreateWnd;
      if Parent.HandleAllocated then
        RegisterTouchWindow(Parent.Handle, 0);
      Visible := False;
    end;

    //
    //  При разрушении окна отключаем поддержку тача, дабы не мусорить
    // =============================================================================
    procedure TWinControlProxy.DestroyHandle;
    begin
      if Parent.HandleAllocated then
        UnregisterTouchWindow(Parent.Handle);
      Visible := True;
      inherited DestroyHandle;
    end;

Этот проксик ничего знать не знает о нашем движке и тихой сапой выполняет всего лишь одну единственную задачу – не допустить отключение окна от тачскрина.  

Для поддержки проксика необходимо немного расширить структуру TWindowData, добавив туда ссылку на ассоциированный с окном прокси:  

    TWindowData = record
      Control, Proxy: TWinControl;

После чего немного изменить процедуры регистрации окна:  

    if FWindows.IndexOf(WindowData) < 0 then
    begin
      // создаем следящий проксик,
      // который будет заного подключать окно к тачу после его пересоздания
      WindowData.Proxy := TWinControlProxy.Create(Value);
      WindowData.Proxy.Parent := Value;

      // старый код

      WindowData.Handlers := Handlers;
      WindowData.LastClickTime := 0;
      // подключаем окно к тачу
      RegisterTouchWindow(Value.Handle, 0);
      FWindows.Add(WindowData);
    end;

и снятия окна с регистрации:  

    if Index >= 0 then
    begin
      // удаляем следящий проксик, он при разрушении отключит окно от тача
      FWindows[Index].Proxy.Free;

      // старый код

      // и удаляем окно из списка
      FWindows.Delete(Index);
    end;

Вот собственно и все.  
Давайте посмотрим как это работает.  

#### 13\. Контрольный тест работы мультитач движка

Опять создаем новый проект и на главную форму кидаем TMemo, в который будут выводится результаты работы и кнопку.  

В обработчике кнопки будем пересоздавать главную форму, чтобы протестировать работу проксика:  

    procedure TdlgGesturesText.Button1Click(Sender: TObject);
    begin
      RecreateWnd;
    end;

В конструкторе формы подключим ее к движку мультитача:  

    procedure TdlgGesturesText.FormCreate(Sender: TObject);
    var
      Handlers: TTouchHandlers;
    begin
      ZeroMemory(@Handlers, SizeOf(TTouchHandlers));
      Handlers.Gesture := OnGesture;
      TouchManager.RegisterWindow(Self, Handlers);
    end;

После чего реализуем сам обработчик:  

    procedure TdlgGesturesText.OnGesture(Sender: TObject; Control: TWinControl;
      GestureType: TGestureType; Position: TPoint; Completed: Boolean);
    begin
      if not Completed then
        if not (GestureType in [gt2Left..gt2Down]) then Exit;
      Memo1.Lines.Add(Format('Control: "%s" gesture "%s" at %dx%d (completed: %s)',
        [
          Control.Name,
          GetEnumName(TypeInfo(TGestureType), Integer(GestureType)),
          Position.X,
          Position.Y,
          BoolToStr(Completed, True)
        ]));
    end;

Билдим, запускаем – вуаля.  

<iframe width="420" height="315" src="https://www.youtube.com/embed/EgabHZIzbLs?feature=oembed" frameborder="0" allowfullscreen=""></iframe>  

На видео наглядно видно распознавание всех 15 поддерживаемых жестов и также работа контролирующего зарегистрированное окно, проксика.  

Собственно, это и был тот самый цимус, о котором я говорил в конце 10 главы — буквально полтора десяток строчек кода и все работает из коробки.  

Исходный код примера в папке "[.\demos\gestures\](http://rouse.drkb.ru/blog/multitouch.zip)" в архиве с исходниками.  

#### 14\. Выводы

Жалко, конечно, что этот функционал отсутствует в ХЕ4.  
С другой стороны, если бы не этот момент, я бы так и не стал разбираться в том: «как оно там это все фунциклирует», так что есть плюсы.  

Минусы данного подхода в том, что полностью выпилена работа с сообщениями WM_GESTURE + WM_POINTS и распознавание жестов передано на откуп коду в движке.  
Согласен, но это сделано преднамеренно.  

Если вы сами начнете копаться в этом направлении, вероятно вы в итоге согласитесь с моим подходом, хотя как знать. По крайней мере у вас останется поле для фантазии, как можно еще подойти к решению такой задачи.  

Исходный код класса Common.TouchManager, предоставленный в демопримерах к статье не является окончательным и будет периодически развиваться, правда не уверен что я буду его сопровождать на паблике. Впрочем ваши предложения и замечания только приветствуются.  

Как всегда, благодарю участников форума «Мастера Дельфи» за вычитку статьи.  

Исходный код демопримеров доступен по [данной ссылке](http://rouse.drkb.ru/blog/multitouch.zip).  

Удачи!  

**Update:**  
К сожалению, или к счастью, выяснилось что из-за некоторых особенностей ОС Windows 8 и выше, ловушка WH_GETMESSAGE не будет перехватывать сообщение WM_TOUCH, таким образом этот код работать не будет.  

<div class="spoiler">**Чтобы исправить такую неприятность, нужно убрать работу с ловушкой и передать обработку сообщения WM_TOUCH на проксик, переписав его следующим образом:**

<div class="spoiler_text">

    type
      // класс следящий за пересозданием зарегистрированного окна
      TWinControlProxy = class(TWinControl)
      private
        FOldWndProc: TWndMethod;
        procedure ParentWndProc(var Message: TMessage);
      protected
        procedure DestroyHandle; override;
        procedure CreateWnd; override;
        procedure CreateParams(var Params: TCreateParams); override;
      public
        destructor Destroy; override;
        procedure InitParent(Value: TWinControl);
      end;

    { TWinControlProxy }

    //
    //  При создании выставляем стиль WS_EX_TRANSPARENT, мы будем не заметными.
    // =============================================================================
    procedure TWinControlProxy.CreateParams(var Params: TCreateParams);
    begin
      inherited;
      Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
    end;

    //
    //  При создании окна сразу включаем ему поддержку тача
    // =============================================================================
    procedure TWinControlProxy.CreateWnd;
    begin
      inherited CreateWnd;
      if Parent.HandleAllocated then
        RegisterTouchWindow(Parent.Handle, 0);
      Visible := False;
    end;

    //
    //  При разрушении, возвращаем оконную процедуру на место
    // =============================================================================
    destructor TWinControlProxy.Destroy;
    begin
      if Parent <> nil then
        Parent.WindowProc := FOldWndProc;
      inherited;
    end;

    //
    //  При разрушении окна отключаем поддержку тача, дабы не мусорить
    // =============================================================================
    procedure TWinControlProxy.DestroyHandle;
    begin
      if Parent.HandleAllocated then
        UnregisterTouchWindow(Parent.Handle);
      Visible := True;
      inherited DestroyHandle;
    end;

    //
    //  При инициализации парента, перекрываем его оконную процедуру
    // =============================================================================
    procedure TWinControlProxy.InitParent(Value: TWinControl);
    begin
      Parent := Value;
      FOldWndProc := Value.WindowProc;
      Value.WindowProc := ParentWndProc;
    end;

    //
    //  Перехватываем сообщение WM_TOUCH в оконной процедуре родителя
    // =============================================================================
    procedure TWinControlProxy.ParentWndProc(var Message: TMessage);
    var
      Msg: TMsg;
    begin
      if Message.Msg = WM_TOUCH then
      begin
        Msg.hwnd := Parent.Handle;
        Msg.wParam := Message.WParam;
        Msg.lParam := Message.LParam;
        TouchManager.HandleMessage(@Msg);
      end;
      FOldWndProc(Message);
    end;

</div>

</div>

В [архиве к статье](http://rouse.drkb.ru/blog/multitouch.zip), данные изменения уже произведены.</div>

</div>

# Franch Reference
http://www.jcolibri.com/articles/vcl_rtl/ecran_tactile_delphi_multi_touch/ecran_tactile_delphi_multi_touch.html
