unit tpLabel;  {Normal/Raised/Recessed label component; TLabel with methods to write the caption more easily.}

(*
Permission is hereby granted, on 24-Feb-2005, free of charge, to any person
obtaining a copy of this file (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies of the
Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

Author of original version of this file: Michael Ax
*)

interface

uses
  Forms, Dialogs, StdCtrls, SysUtils, Messages, Classes, Graphics, Controls,
  Windows,
  ucShell;

type
  TTextStyle=(tsNone,tsRaised,tsRecessed);
  TtpShellMode=(smNone,smOpen,smPrint);

  TtpLabel=class(TLabel)
  private
    fShellMode: TtpShellMode;
    fShellCommand: String;
    fPosDepth: byte;
    fNegDepth: byte;
    FTextStyle:TTextStyle;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure SetTextStyle(Value:TTextStyle);
    function  GetInteger: Longint;
    procedure SetInteger(Value:Longint);
  public
    constructor Create(AOwner: TComponent); Override;
    procedure Click; override;
    procedure Inc;
    procedure Dec;
    procedure DoDrawText(var Rect: TRect;
      Flags:{$IFDEF VER110}Word{$ELSE}LongInt{$ENDIF}); //Borland C++Builder 3.0
      override;
  published
    property ShellMode: TtpShellMode read fShellMode write fShellMode;
    property ShellCommand: String read fShellCommand write fShellCommand;
    property TextStyle : tTextStyle read fTextStyle write SetTextStyle default tsRecessed;
    property AsInteger : Longint read GetInteger write SetInteger stored false;
    property PosDepth: byte read fPosDepth write fPosDepth default 1;
    property NegDepth: byte read fNegDepth write fNegDepth default 1;
    end;

{------------------------------------------------------------------------------}

procedure Register;

implementation

{------------------------------------------------------------------------------}

constructor TtpLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTextStyle:=tsRecessed;
  fPosDepth:=1;
  fNegDepth:=1;
end;

procedure TtpLabel.SetTextStyle( Value : TTextStyle );
begin
  if Value<>fTextStyle then begin
    fTextStyle:=Value;
    Invalidate;
    end;
end;

procedure TtpLabel.DoDrawText(var Rect:TRect;
  Flags:{$IFDEF VER110}Word{$ELSE}Longint{$ENDIF}); //cpb

var
  Text       : array[ 0..255 ] of Char;
  TmpRect    : TRect;
  UpperColor : TColor;
  LowerColor : TColor;
begin
  GetTextBuf(Text, SizeOf(Text));
  if ( Flags and DT_CALCRECT <> 0) and
     ( ( Text[0] = #0 ) or ShowAccelChar and
       ( Text[0] = '&' ) and
       ( Text[1] = #0 ) ) then
    StrCopy(Text, ' ');

  if not ShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  Canvas.Font := Font;

  if FTextStyle = tsRecessed then begin
    UpperColor := clBtnShadow;
    LowerColor := clBtnHighlight;
    end
  else begin
    UpperColor := clBtnHighlight;
    LowerColor := clBtnShadow;
    end;

  if FTextStyle in [ tsRecessed, tsRaised ] then begin
    TmpRect := Rect;
    OffsetRect( TmpRect, fPosDepth, fPosDepth );
    Canvas.Font.Color := LowerColor;
    DrawText(Canvas.Handle, Text, StrLen(Text), TmpRect, Flags);

    TmpRect := Rect;
    OffsetRect( TmpRect, -fNegDepth, -fNegDepth );
    Canvas.Font.Color := UpperColor;
    DrawText(Canvas.Handle, Text, StrLen(Text), TmpRect, Flags);
    end;

  Canvas.Font.Color := Font.Color;
  if not Enabled then
    Canvas.Font.Color := clGrayText;
  DrawText(Canvas.Handle, Text, StrLen(Text), Rect, Flags);
end;


procedure TtpLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
begin
  with Canvas do begin
    if not Transparent then begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
      end;
    Brush.Style:=bsClear;
    Rect:=ClientRect;
    DoDrawText(Rect,(DT_EXPANDTABS or DT_WORDBREAK) or Alignments[Alignment]);
    end;
end;

{------------------------------------------------------------------------------}

function TtpLabel.GetInteger: Longint;
begin
  Result:=StrToIntDef(Caption,0);
end;

procedure TtpLabel.SetInteger(Value:Longint);
begin
  Caption:=IntToStr(Value);
end;

procedure TtpLabel.Inc;
begin
  Caption:=Inttostr(Succ(StrToIntDef(Caption,0)));
end;

procedure TtpLabel.Dec;
begin
  Caption:=Inttostr(Pred(StrToIntDef(Caption,0)));
end;

{-------------------------------------------------------------------------}

procedure TtpLabel.CMMouseEnter(var Message: TMessage);
begin
  if ShellMode<>smNone then
    Cursor:= crDrag;
  inherited;
end;

procedure TtpLabel.CMMouseLeave(var Message: TMessage);
begin
  if ShellMode<>smNone then
    Cursor:= crDefault;
  inherited;
end;

procedure TtpLabel.Click;
var
  a1:string;
begin
  inherited Click;
  if ShellMode=smNone then
    exit;
  //
  if ShellCommand<>'' then
    a1:=ShellCommand
  else
    if Hint<>'' then
      a1:=Hint
    else
      a1:=Caption;
  //
  case ShellMode of
    smOpen: WinShellOpen(a1);
    smPrint: WinShellPrint(a1);
    end;
end;

//----------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('TPACK', [TtpLabel]);
end;
//----------------------------------------------------------------------
{
procedure TForm1.UpdateBits(Value:Word);
var
  i:integer;
begin
  tplabel3.AsInteger:=Value;
  with tpLabel4 do begin
    Caption:='';
    for i:=0 to 15 do begin
      if Value and (1 shl i) = (1 shl i) then
        Caption:='1'+Caption
      else
        Caption:='0'+Caption;
      if (i>0) and (i<15) and (succ(i) mod 4 = 0) then
        Caption:='.'+Caption;
      end;
    end;
end;
}
end.
