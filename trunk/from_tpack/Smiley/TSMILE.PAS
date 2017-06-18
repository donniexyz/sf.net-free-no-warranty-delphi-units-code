{    The TSmiley component is Copyright © 1995    }
{         by Nick Hodges All Rights Reserved      }
{          email: 71563.2250@compuserve.com       }

{ USED WITH PERMISSION for distribution with _Delphi 2.0 In-Depth_,
  Cary Jensen, Editor.

  MODIFIED by Michael Ax, ax@href.com, to demonstrate a few things
  about property editors...
}

unit TSmile;

interface

uses
  SysUtils
{$IFDEF WIN32}
, Windows
{$ELSE}
, WinProcs, WinTypes
{$ENDIF}
  , Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, ExtCtrls, StdCtrls, DsgnIntF, TypInfo;

type
    TSmileyMood = (smHappy, smSad, smShades, smTongue, smIndifferent, smOoh);

const
     smInitialMood = smHappy;
     smClickedMood = smSad;
     MoodString : array[TSmileyMood] of PChar = ('smHappy', 'smSad', 'smShades', 'smTongue', 'smIndifferent', 'smOoh');
     MaxHeight = 26;
     MaxWidth = 26;

type
  TSmiley = class(TImage)
  private
	{ Private declarations }
	Face: TBitmap;
	FMood: TSmileyMood;
	procedure SetBitmap;
	procedure SetSmileyMood(NewMood: TSmileyMood);
        procedure WMSize (var Message: TWMSize); message wm_paint;
        procedure MouseClicked(up:boolean);
        procedure MaxSize;
  public
	{ Public declarations }
        constructor Create(AOwner: TComponent); override;
        destructor Free;
        procedure Toggle;
        procedure MoodDialog;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
        property Mood: TSmileyMood read FMood write SetSmileyMood;
  end;

  TSmileyMoodDlg = class(TForm)
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Label1: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    FMood: TSmileyMood;
    procedure SetSmileyMood(NewMood: TSmileyMood);
  public
    { Public declarations }
    property Mood: TSmileyMood read FMood write SetSmileyMood;
  end;

  TSmileyMoodProperty = class( TEnumProperty )
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

{-------------------------------------------------
                 implementation
--------------------------------------------------}

implementation

{$R *.DFM}
{$IFDEF WIN32}
{$R TSmile32.res} 
{$ELSE}
{$R TSmile16.res} 
{$ENDIF}

{-------------------------------------------------
                        TSmiley
--------------------------------------------------}

constructor TSmiley.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Face := TBitmap.Create;  {Note dynamic allocation of the pointer}
  SetSmileyMood(smInitialMood);
end; {Create}

destructor TSmiley.Free;
begin
  Face.Free; {Use Free rather than Destroy, as Free checks for a nil pointer first}
  inherited Free;
end; {Free}

procedure TSmiley.Toggle;
begin
  if fMood = high(TSmileyMood) then  {Don't allow fMood to overflow}
    Mood := low(TSmileyMood)
  else
    Mood := Succ(fMood);
end; {Toggle}

procedure TSmiley.SetSmileyMood(NewMood: TSmileyMood);
begin
  if (face.handle=0) or (fMood<>NewMood) then begin
    FMood := NewMood;
    SetBitmap;
    end;
end; {SetSmileyMood}

procedure TSmiley.SetBitmap;
begin
  Face.Handle := LoadBitmap(hInstance, MoodString[fMood]);
  Self.Picture.Graphic := Face as TGraphic;  {Use RTTI to cast face as TGraphic, needed by TImage}
  MaxSize;
end; {SetBitmap}

{This method will respond to a mouse push on the Smiley by storing the
old face for later use and giving the "Sad" face.  Smileys don't like to get
clicked on!}
procedure TSmiley.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button=mbRight then
    MoodDialog                         {here's where we run the property editor at runtime}
  else
    MouseClicked(false);
end; {MouseDown}

{This method restores the old face when the mouse comes back up}
procedure TSmiley.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button<>mbRight then
    MouseClicked(true);
end; {MouseUp}

procedure TSmiley.MouseClicked(up:boolean);
const {the proc uses a constant rather than a field.}
  OldMood: TSmileyMood=smInitialMood;
begin
  if up then                  {button back up.}
    SetSmileyMood(OldMood)          {restore prior mood}
  else                        {button now down}
    if Mood<>smClickedMood then begin {store if different!}
      {windows/delphi can loose the up sometimes so we must
      not just act on any down but must see what it'd do first}
      OldMood:= Mood;         {store mood in local constant}
      SetSmileyMood(smClickedMood); {express clicked-on mood}
      end;
end;

procedure TSmiley.WMSize(var Message: TWMSize);
{This method keeps the user from sizing the Smiley at design time.
(remeber, it's a bitmap after all! You can use the 'csDesigning in
ComponentState' to control what the user can do at design time}
begin
  inherited;
  if (csDesigning in ComponentState) then
    MaxSize;
end;

procedure TSmiley.MaxSize;
begin
  Width := MaxWidth;
  Height := MaxHeight;
end;

procedure TSmiley.MoodDialog; {here's where we run the property editor at runtime}
begin
  with TSmileyMoodDlg.Create(Application) do try
    Mood:=Self.Mood;
    if ShowModal=mrOk then
      Self.Mood:=Mood;
  finally
    Free
  end;
end;

{-------------------------------------------------
                 TSmileyMoodDlg
--------------------------------------------------}

procedure TSmileyMoodDlg.SetSmileyMood(NewMood: TSmileyMood);
var
   Counter: Integer;
begin
{  if (fMood<>NewMood) or (label1.caption='Label1') then  }
    for Counter:= 0 to ComponentCount - 1 do
      if (Components[Counter] is TSpeedButton) then
        with TSpeedButton(Components[Counter]) do
          if Tag= Ord(NewMood) then begin
            Down:= True; {sets fMood vis click}
            fMood:= NewMood;
            with Label1 do begin
              Caption:= copy(strpas(MoodString[fMood]),3,255);
              Update;
              end;
            end;
end;

procedure TSmileyMoodDlg.SpeedButton1Click(Sender: TObject);
begin
  Mood := TSmileyMood((Sender as TSpeedButton).Tag);
end; {SpeedButton1Click}


{-------------------------------------------------
                 TMoodProperty
--------------------------------------------------}

function TSmileyMoodProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

procedure TSmileyMoodProperty.Edit; {here's where we run the property editor at design-time}
begin
  with TSmileyMoodDlg.Create(Application) do try
    Mood:=TSmileyMood(GetOrdValue);              {small difference only to MoodDialog above}
    if ShowModal=mrOk then
      SetOrdValue(Ord(Mood))
  finally
    Free
  end;
end; {Edit}

{-------------------------------------------------
             Smiley ends, keep smiling.
--------------------------------------------------}
procedure Register;
begin
  RegisterComponents('SAMPLES', [ TSmiley ] );
  RegisterPropertyEditor( TypeInfo( TSmileyMood ), TSmiley, 'Mood', TSmileyMoodProperty );
end;

end.
