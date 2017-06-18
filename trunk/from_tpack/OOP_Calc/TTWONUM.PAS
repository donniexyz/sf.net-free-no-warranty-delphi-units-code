unit TTwoNum;

{ Author: Michael Ax, 
  Copyright (c) 1996 HREF Tools Corp.  
  Written for distribution with _Delphi 2.0 In-Depth_,
  Cary Jensen, Editor.

  TPack is here: https://www.href.com/tpack
  WebHub is here: https://www.href.com/

}

(*
Permission is hereby granted, on 03-Oct-1996, free of charge, to any person
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
*)


{ This unit implements a sample updating component, incorporating
  an edit form and verb. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls
, UpdateOk {tPack edit,update,verb,action,command levels}
, utForm;  {tPack slightly extended tform base classes for edit level}

type
  TTwoNumbersForm = class(TtpDocForm)
    {these three controls are named after the matching properties}
    NumberOne: TEdit;  {standard edit control for #1}
    NumberTwo: TEdit;  {standard edit control for #2}
    TheResult: TLabel; {label to show the result of the addition}
    {these controls play supporting roles}
    Button1: TButton;  {calls Recalculate method}
    Button2: TButton;  {set to mrOk}
    Label1: TLabel;    {#1}
    Label2: TLabel;    {#2}
    Shape1: TShape;    {line}
    procedure Recalculate(Sender: TObject); {button1.click}
    procedure UpdateForm(Sender: TObject);  {form.activate}
    procedure ExitNumberField(Sender: TObject); {sync properties}
    end;

  {Step zero: pick your base class.
  Define the class as derived from one of
  [TtpUpdate, TtpComponent, TtpAction, TtpCommand, TWebAction]
  depending on your goal. }

  {the rest of the steps don't necessarily happen top-down so
   read through to the end...}

type
  TTwoNumbers=class(TtpComponent)
  private     {step two: define the storage fields}
    fNumberOne: Longint; {first stored field}
    fNumberTwo: Longint; {second stored field}
    fTheResult: Longint; {buffer the result to make a point!}
  protected   {step three: define the access procedures}
    procedure SetNumberOne(Value:Longint);
    procedure SetNumberTwo(Value:Longint);
    function GetTheResult:Longint;
    {step five: list the procedures to override}
    function  DoUpdate:Boolean; Override; {update the result}
    function  EditFormClass:TComponentEditFormClass; override;
    {so far so good, lets add the edit verb with these procs:}
    function  GetLocalVerbCount: Integer;
    function  GetVerbCount: Integer; Override;
    function  GetVerb(Index: Integer): string; Override;
    procedure ExecuteVerb(Index: Integer); Override;
    {the recalc code is only called internally. We've now
    met the goal of encapsulating the actual business rules.}
    procedure Recalc; {adds the two numbers}
    {you can go from here and look through the webhub source
    code samples which will build on the principles shown here
    and introduce you to additional concepts and possibilities.}
  public
  published   {step one: define the published interface}
    property NumberOne:longint read fNumberOne write SetNumberOne;
    property NumberTwo:longint read fNumberTwo write SetNumberTwo;
    property TheResult:longint read GetTheResult write SetNoLongInt stored false;
    end;

procedure Register;

implementation

{$R *.DFM}

{step four: implement the access procedures.}
{hint! access procedures should be as simple as can be. When in
trouble, consider adding public properties as intermediaries.}

procedure TTwoNumbers.SetNumberOne(Value:Longint);
begin
  if Value<>fNumberOne then begin {New Value}
    tpUpdated:=false;   {un-update}
    fNumberOne:=Value;  {store}
    end;
end;

procedure TTwoNumbers.SetNumberTwo(Value:Longint);
begin
  if Value<>fNumberTwo then begin {New Value}
    tpUpdated:=false;   {un-update}
    fNumberTwo:=Value;  {store}
    end;
end;

function TTwoNumbers.GetTheResult:Longint;
begin
  if tpUpdated then
    Result:=fTheResult
  else
    Result:=-1;
end;

{-----------------------------------------------------}
{step seven: implement the business rules code}
{this might include DoExecute and SetCommand procedures
as well as addtl code to implement verbs}

procedure TTwoNumbers.Recalc;
begin
  fTheResult:=fNumberOne+fNumberTwo;
end;

{-----------------------------------------------------}
{step six: implement the overridden procedures}

function TTwoNumbers.DoUpdate:Boolean;
begin {recalculate}
  {in this case you can simply write:}
  Recalc;
  Result:=True;
  {or be more elaborate:}{
  Result:=inherited DoUpdate;
  if Result then
    Recalc;
  {or add errorchecking:}{
  if Result then try
    Recalc;
  except
    Result:=false;
    raise;
    end;{}
end;

{-----------------------------------------------------}
{this proc determines the form used by the edit method}

function TTwoNumbers.EditFormClass:TComponentEditFormClass;
begin
  Result:=TTwoNumbersForm;
end;

{-----------------------------------------------------}
{the next four procs add a verb to the component}

function TTwoNumbers.GetLocalVerbCount: Integer;
begin
  Result := 1;
end;

function TTwoNumbers.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + GetLocalVerbCount;
end;

function TTwoNumbers.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Numbers';
  else
    Result:=inherited GetVerb(Index - GetLocalVerbCount);
  end;
end;

procedure TTwoNumbers.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  else
    Inherited ExecuteVerb(Index - GetLocalVerbCount);
    end;
end;

{-----------------------------------------------------}
{these are the procedures used by the edit form to
link to the actual component at design and run-time}

procedure TTwoNumbersForm.UpdateForm(Sender: TObject);
begin {form.OnActivate}
  with TTwoNumbers(fPersistent) do begin
    {prepare the form}
    Self.NumberOne.Text:=IntToStr(NumberOne);
    Self.NumberTwo.Text:=IntToStr(NumberTwo);
    Self.TheResult.Caption:=IntToStr(TheResult);
    end;
end;

procedure TTwoNumbersForm.ExitNumberField(Sender: TObject);
begin
  with TTwoNumbers(fPersistent),tEdit(Sender) do begin
    case tag of  {tag of the last item in the with-list}
      1: begin    {oh yes.. the edit boxes have tag=1}
         NumberOne:=StrToIntDef(Self.NumberOne.Text,-1);
         Self.NumberOne.Text:=IntToStr(NumberOne);
         end;
      2: begin    {and tag=2.. no error checking here.}
         NumberTwo:=StrToIntDef(Self.NumberTwo.Text,-1);
         Self.NumberTwo.Text:=IntToStr(NumberTwo);
         end;
      end;
    Self.TheResult.Enabled:=IsUpdated; {show result state!}
    end;
end;

procedure TTwoNumbersForm.Recalculate(Sender: TObject);
begin {button1.OnClick}
  with TTwoNumbers(fPersistent) do begin
    Self.TheResult.Caption:=IntToStr(TheResult); {might! recalc}
    Self.TheResult.Enabled:=IsUpdated; {show result state!}
    end;
end;

{-----------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Samples',[TTwoNumbers]);
end;

end.
