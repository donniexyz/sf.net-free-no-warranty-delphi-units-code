unit TShapes;

{ Author: Ann Lynnworth, 21 January 1996.

  This was written to demonstrate some of the ideas of TComponentExtensions,
  a component originally published as part of TPack, by Michael Ax.

  To quickly get the idea: add this unit to your Delphi library, go to the
  Samples palette and then place a TFramedRedCircle on a form...

  Please see _Delphi In-Depth_, edited by Cary Jensen, for a complete
  description of this code.

  This unit is distributed free of charge.  
}

interface

uses
  SysUtils
  {$IFDEF WIN32}
  , Windows
  {$ELSE}
  , WinTypes
  {$ENDIF}
  , Messages, Classes, Graphics, Controls, ExtCtrls
  , Xtension {from TPack; needed for TComponentExtensions}
  ;

const
  perfectFrameWidth=3;  {used by TFrame}

{We are only customizing TFrame so that we can make it 'clear' and have
 a perfect width... }
type
  TFrame = class(TShape)
  public
    { Public declarations }
    Constructor Create(aOwner:TComponent); Override;
  end;

{ TFramedRedCircle is the "real example." }
type
  TFramedRedCircle = class(TShape)
    {Add a variable, cx, to "graft on" the functionality.  See
     xtension.pas for source to TComponentExtensions. }
    cx: TComponentExtensions;
  private
    { Private declarations }
    fFrame:TFrame;
  public
    { Public declarations }
    Constructor Create(aOwner:TComponent); Override;
    Destructor  Destroy; Override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
    procedure   Loaded; Override;
    procedure   Paint; Override;
  published
    { Published declarations }
    { here is the pointer to the other-object that we'll create
      automatically. }
    property Frame:TFrame read fFrame write fFrame;
  end;

procedure Register;

{------------------------------------------------------------------------------}

implementation

Constructor TFrame.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  Brush.Style:=bsClear; {it wouldn't be a frame if you couldn't see through}
  Pen.Width:=perfectFrameWidth; {anything > 1 is noticable}
end;

{------------------------------------------------------------------------------}

Constructor TFramedRedCircle.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);

  cx:= TComponentExtensions.Create(Self);

  {We want to do some fancy tricks if and only if we are in the
   designer.}

  {Here's some code that does NOT work because Delphi hasn't set this yet:
   if csDesigning in ComponentState }

  {Here's an alternative that DOES work.  The tForm Name property is
   only set while you're in the designer to aviod duplicate names with
   multiple instances of the form. This lets us 'sneak in' some
   design-time only code into a component's constructor *before*
   Delphi gets around to setting the 'ComponentState' property.}
  if aOwner.Name<>'' then begin {aOwner is the Form}
    Shape:=stCircle;
    Brush.Color:=clRed;
    cx.SetIfFoundExactly(fFrame,TFrame);
    cx.MakeIfNil(fFrame,TFrame); {create TFrame if none found on form}

    { It is NOT POSSIBLE to set the location of fFrame here because
      in this moment, our circle is sitting at 0,0 and has not yet
      been moved to its real location, as determined by your click.
      Paint is used to accomplish this instead.}

    end;
end;

procedure TFramedRedCircle.Paint;
var
  goHereLeft, goHereTop:integer;
  thisDiameter:integer;
  offset:integer;
begin
  inherited Paint;
  if fFrame<>nil then begin
    {offset=how much displacement between frame and circle}
    offset:=FFrame.pen.width+1;
    goHereLeft:=Left-offset;
    goHereTop :=Top-offset;
    {base the frame size on circle width}
    thisDiameter:=width+(offset*2);
    with fFrame do begin
      left:=goHereLeft;
      top :=goHereTop;
      height:=thisDiameter;
      width :=thisDiameter;
      end;
    end;
end;

Destructor TFramedRedCircle.Destroy;
begin
  cx.free;  {what we create, we must destroy}
  inherited Destroy;
end;

procedure TFramedRedCircle.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    cx.NilIfSet(fFrame,AComponent);  {disconnect if fFrame is being removed}
    end;
end;

procedure TFramedRedCircle.Loaded;
begin
  inherited Loaded;
  {this is sort of cool; if you run the form, the frame moves into place.}
  {i.e. this works even if you get rid of the Paint method.  with paint
   overridden, there is no need for this piece.}
  {if fFrame<>nil then
    with fFrame do begin
      left:=self.Left-(pen.width+1);
      top :=self.Top -(pen.width+1);
      end;}
end;

{------------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Samples', [TFramedRedCircle, TFrame]);
end;

end.

{ </PRE> }
