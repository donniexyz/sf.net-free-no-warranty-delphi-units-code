unit bitbox;

(*
Permission is hereby granted, on 24-Mar-2003, free of charge, to any person
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

{THE POINT:  To create checkbox group components that will take a byte or word
and provide dynamically sized boxes containing selected items from a universe
of 8 or 16 choices. Allowing the user to check/set bits via a form.}

{this unit takes advantage of delphi's small set implementation, which works
in bytes and words for sets with less than 9/17 members respectively.}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls
, Forms, Dialogs, StdCtrls
{$IFDEF WIN32}
, Windows
{$ELSE}
, WinProcs, WinTypes
{$ENDIF}
, ucTypes;

type
  TBoxOrientation = (boxVertical,boxHorizontal);

  {define a common groupbox for byte and word use}
  TBitBox = class(TGroupBox)
  private
    fMembers: TWordSet;
    fMask: TWordSet;
    fCaptions: TStringList;
    fHints: TStringList;
    fOnChange: TNotifyEvent;
    fReadOnly: Boolean;

    fBoxOrientation: TBoxOrientation;
    fFromLeft: Byte;
    fFromRight: Byte;
    fColumns: Byte;
    fMinTextWidth:Byte;
    fMaxTextWidth:Integer;
    fFromTop:Byte;
    fRowHeight:Byte;
    fColumnSpacing:Byte;
    //
    function GetMember:Word;
    procedure SetMember(Value:Word);
    procedure SetMembers(Value:TWordSet);
    procedure SetMask(Value:TWordSet);
    procedure SetCaptions(Value:TStringList);
    procedure SetHints(Value:TStringList);
    function GetMaxTextWidth:Integer;
    procedure SetColumns(Value:Byte);
//    procedure SetFromTop(Value:Byte);
    procedure SetRowHeight(Value:Byte);
    procedure SetMinTextWidth(Value:Byte);
    procedure SetMaxTextWidth(Value:Integer);
    procedure SetColumnSpacing(Value:Byte);
    procedure SetBoxOrientation(Value:TBoxOrientation);
    procedure InitBox;
  protected
    procedure ChangeSelected(Sender:TObject); virtual;
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; Override;
    procedure Loaded; Override;
    procedure Init; {[re]creates checkboxes from fUniverse/fMembers}
    procedure UpdateAll;
    procedure UpdateBox(Bit:T16Bits;aChecked:Boolean;aCaption:String); {updates chekbox}
  published
    property Numeric:  Word read GetMember write SetMember;
    property Possible: TWordSet read fMask write SetMask;
    property Selected: TWordSet read fMembers write SetMembers;
    property ReadOnly: Boolean read fReadOnly write fReadOnly default False;
    property Captions: TStringList read fCaptions write SetCaptions;
    property Hints:    TStringList read fHints write SetHints;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    property BoxOrientation: TBoxOrientation read fBoxOrientation write SetBoxOrientation
    {$IFDEF START_HORIZONTALLY}
      default boxHorizontal;
    {$ELSE}
      default boxVertical;
    {$ENDIF}
    property Columns: Byte read fColumns write SetColumns
    {$IFDEF START_HORIZONTALLY}
      default 0;
    {$ELSE}
      default 2;
    {$ENDIF}
    property FromTop: Byte read fFromTop write fFromTop default 20;
    property FromLeft: Byte read fFromLeft write fFromLeft default 10;
    property FromRight: Byte read fFromRight write fFromRight default 5;
    property RowHeight: Byte read fRowHeight write SetRowHeight default 20;
    property ColumnSpacing:Byte read fColumnSpacing write SetColumnSpacing default 10;
    property MinTextWidth:Byte read fMinTextWidth write SetMinTextWidth default 16;
    property MaxTextWidth:Integer read GetMaxTextWidth write SetMaxTextWidth;
    end;

{----------------------------------------------------------------------------------------}

//procedure Register;

implementation

{----------------------------------------------------------------------------------------}

constructor TBitBox.Create(aOwner:TComponent);
var
  Bit: T16Bits;
begin
  inherited Create(aOwner);
  fColumnSpacing:=10;
  fFromTop:=20;
  fFromLeft:=10;
  fFromRight:=5;
  fRowHeight:=20;
  fMinTextWidth:=13;
  {$IFDEF START_HORIZONTALLY}
    fBoxOrientation:=boxHorizontal;
    fColumns:=0;
  {$ELSE}
    fBoxOrientation:=boxVertical;
    fMaxTextWidth:=100;
    fColumns:=2;
  {$ENDIF}
  fCaptions:=TStringList.Create;
  fHints:=TStringList.Create;
  for Bit:= Bit0 to BitF do begin
    Include(fMask,Bit);
    fCaptions.Add('(Bit'+inttoStr(ord(bit))+')');
    fHints.Add('(Bit'+inttoStr(ord(bit))+')');
    end;
end;

procedure TBitBox.Loaded;
begin
  inherited Loaded;
  InitBox;
end;

destructor TBitBox.Destroy;
begin
  fCaptions.Free;
  fCaptions:=nil;
  fHints.Free;
  fHints:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{ GET AND SET THE COMPONENT SPECIFIC PROPERTIES                                }
{------------------------------------------------------------------------------}

function TBitBox.GetMember:Word;
var
  s:TWordSet;
begin
  s:=fMembers;
  result:=tWord(s).x;
end;

procedure TBitBox.SetMember(Value:Word);
var
  s:TWordSet;
begin
  s:=TWordSet(Value);
  SetMembers(s);
end;

procedure TBitBox.SetMembers(Value:TWordSet);
begin
  if fMembers<>Value then begin
    fMembers:=Value;
    UpdateAll;
    end;
end;

procedure TBitBox.SetMask(Value:TWordSet);
begin
  if fMask<>Value then begin
    fMask:=Value;
    init;
    end;
end;

procedure TBitBox.SetCaptions(Value:TStringList);
begin
  if fCaptions<>Value then begin {wow}
    fCaptions.Assign(Value);
    UpdateAll;
    end;
end;

procedure TBitBox.SetHints(Value:TStringList);
begin
  if fHints<>Value then begin
    fHints.Assign(Value);
    UpdateAll;
    end;
end;

procedure TBitBox.SetColumns(Value:Byte);
begin
  if fColumns<>Value then begin
    fColumns:=Value;
    Init;
    end;
end;

procedure TBitBox.SetRowHeight(Value:Byte);
begin
  if fRowHeight<>Value then begin
    fRowHeight:=Value;
    Init;
    end;
end;

procedure TBitBox.SetBoxOrientation(Value:TBoxOrientation);
begin
  if fBoxOrientation<>Value then begin
    fBoxOrientation:=Value;
    Init;
    end;
end;

procedure TBitBox.SetMinTextWidth(Value:Byte);
begin
  if fMinTextWidth<>Value then begin
    fMinTextWidth:=Value;
    Init;
    end;
end;

function TBitBox.GetMaxTextWidth:Integer;
begin
  if fMaxTextWidth<fMinTextWidth then
    fMaxTextWidth:=fMinTextWidth;
  Result:= fMaxTextWidth;
end;

procedure TBitBox.SetMaxTextWidth(Value:Integer);
begin
  if fMaxTextWidth<>Value then begin
    fMaxTextWidth:=Value;
    Init;
    end;
end;

procedure TBitBox.SetColumnSpacing(Value:Byte);
begin
  if fColumnSpacing<>Value then begin
    fColumnSpacing:=Value;
    Init;
    end;
end;

{
procedure TBitBox.SetFromTop(Value:Byte);
begin
  if fFromTop<>Value then begin
    fFromTop:=Value;
    Init;
    end;
end;
}
{------------------------------------------------------------------------------}
{ INITIALIZE AND UPDATE ALL OR ONE CHECKBOX IN THE GROUP                       }
{------------------------------------------------------------------------------}

procedure TBitBox.Init;
begin
  if not (csLoading in ComponentState) then
    InitBox;
end;

procedure TBitBox.InitBox;
var
  Cols,i,n:integer;
  Bit:T16Bits;
  c:TCheckBox;
  col,row,percol:byte;
begin
  n:=ControlCount-1;
  if n>-1 then
    for i:=0 to n do
      Controls[0].Free;     {free all owned controls. really.}
  n:=0;
  for bit:= Bit0 to BitF do {step and count how many we'll be making}
    if bit in fMask then
      n:=n+1;
  if n=0 then {nothing to do} {shrink?}
    exit;
  Cols:=fColumns;
  if Cols<=0 then
    Cols:=1;
  if (fBoxOrientation=boxHorizontal) and (fColumns<2) then
    Cols:=n;  {adjust to all accross.}
  percol:=n div Cols;          {figure out how many rows that'll be}
  if (n mod Cols) >0 then  {adjust to get 2 rows for 3 items in 2 columns}
    percol:=percol+1;
  i:=FromLeft+FromRight+Cols*(fColumnSpacing+MaxTextWidth);
  if Width<i then
    Width:=i;
  row:=0;
  col:=0;
  for bit:= Bit0 to BitF do
    if bit in fMask then begin  {make new child controls}
      c:=TCheckBox.Create(self);
      with c do begin
        Checked:= bit in fMembers;
        end;
      with c do begin
        Tag:=ord(bit);
        Caption:= fCaptions.Strings[Tag];
        Hint:= fHints.Strings[Tag];
        Parent:=Self;
        OnClick:=ChangeSelected;
        Left:=FromLeft+Col*(fColumnSpacing+fMaxTextWidth);
        Width:=//MaxTextWidth;
                23+tform(self.owner).canvas.textwidth(Caption); //23 is just a guess
        Top:=fFromTop+(Row*fRowHeight);
        if fBoxOrientation=boxVertical then begin
          Row:=row+1;
          if Row=PerCol then begin
            Row:=0;
            Col:=Col+1;
            end;
          end
        else begin
          Col:=Col+1;
          if Cols=Cols then begin
            Col:=0;
            Row:=Row+1;
            end;
          end;
        end;
      end;
  if Height<(fRowHeight*(PerCol+1)) then;
    Height:=(fRowHeight*(PerCol+1));
end;

procedure TBitBox.UpdateAll;
var
  Bit:T16Bits;
begin
  for Bit:= Bit0 to BitF do
    if Bit in fMask then
      UpdateBox(Bit,Bit in fMembers,fCaptions.Strings[ord(Bit)])
end;

procedure TBitBox.UpdateBox(Bit:T16Bits;aChecked:Boolean;aCaption:String);
var
  i,n:integer;
begin
  n:=ControlCount-1;
  if n>-1 then
    for i:=0 to n do
      if Controls[i].Tag=ord(Bit) then
        with TCheckBox(Controls[i]) do begin
          Caption:=aCaption;
          Checked:=aChecked;
          break;
          end;
end;

{------------------------------------------------------------------------------}
{ PROCS FOR CUSTOM EVENTS                                                      }
{------------------------------------------------------------------------------}

procedure TBitBox.ChangeSelected(Sender:TObject);
var
  c:TCheckBox;
  b:T16Bits;
begin
  c:=TCheckBox(Sender);
  b:=T16Bits(c.Tag); {remember, we're using the tag to hold the bit value of the checkbox}
  if c.Checked then
    fMembers:=fMembers+[b]
  else
    fMembers:=fMembers-[b];
  if assigned(fOnChange) then
    fOnchange(Sender);
end;

{----------------------------------------------------------------------------------------}
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TBitBox]);
//end;
{----------------------------------------------------------------------------------------}

end.
