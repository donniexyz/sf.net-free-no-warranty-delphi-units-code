unit ErrorMsg;
(*
Permission is hereby granted, on 6-May-2003, free of charge, to any person
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

// Author of original version of this file: Michael Ax

interface

uses Classes, Graphics, Forms, Controls, Buttons
, Windows
{$IFNDEF NoEDIT}
, tpDbg
{$ENDIF}
, StdCtrls, ExtCtrls, SysUtils, Dialogs
, UpdateOk
, tpAction
, uctypes
  ;

{------------------------------------------------------------------------------}

type
  TErrorDialog = class(TtpAction)
    Error:Exception;
    eSender: TObject;
  private
    fResult: TExceptionReAction;
    fCanRetry: Boolean; {can an exception be retried?}
    fCanIgnore: Boolean; {can an exception be ignored?}
  protected
    function GetTest:Boolean; override;
    procedure SetTest(Value:Boolean); override;
  public
    constructor Create(aOwner:Tcomponent); override;
    procedure ExceptionProc(Sender: TObject;E:Exception);
    procedure RetryException(Sender:TObject;E:Exception;var Action:TExceptionReAction);
    procedure DoExecute; Override;
  published
    property CanRetry: Boolean read fCanRetry write fCanRetry;
    property CanIgnore: Boolean read fCanIgnore write fCanIgnore default true;
    property Result: TExceptionReAction read fResult write fResult;
    end;

{----------------------------------------------------------------------------------------}

//procedure Register;

implementation

{----------------------------------------------------------------------------------------}

{xxx$R *.DFM}


constructor TErrorDialog.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  Error:=nil;
  eSender:=nil;
  fCanIgnore:=true;

  Application.OnException:=ExceptionProc;

end;


procedure TErrorDialog.ExceptionProc(Sender: TObject;E:Exception);
begin
{$IFNDEF NoEDIT}
  debuglog(sender,e.message);
{$ENDIF}
  messagebeep($FFFF);
  Error:=E;
  eSender:=Sender;
  Screen.Cursor:=crDefault;
  Execute;
{  Application.ShowException(E); }
end;


procedure TErrorDialog.SetTest(Value:Boolean);
begin
  Error:=Exception.Create('Test of '+Classname+'.'+Name);
  eSender:=Self;
  Execute;
  Error.Free;
  Error:=nil;
end;

function TErrorDialog.GetTest:Boolean;
begin
  Result:=(fResult=reRetry);
end;

{------------------------------------------------------------------------------}

procedure TErrorDialog.RetryException(Sender:TObject;E:Exception;var Action:TExceptionReAction);
begin
  eSender:=Sender;
  Error:=E;
  fResult:=Action;
  Execute;
  Action:=fResult;
end;

procedure TErrorDialog.DoExecute;
var
  a1:string;
  b:TMsgDlgButtons;
  Cursor:TCursor;
begin
  inherited DoExecute;
  Cursor:=Screen.Cursor;
  Screen.Cursor:=crDefault;

  if fCanIgnore and fCanRetry then
    b:=mbAbortRetryIgnore
  else
    if fCanIgnore then
      b:=[mbIgnore,mbCancel]
    else
      if fCanRetry then
        b:=[mbRetry,mbCancel]
      else
        b:=[mbCancel];

  a1:=Error.ClassName+'! in '+eSender.ClassName;
  if eSender is tComponent then
    with tComponent(eSender) do
      a1:=a1+'.'+Name;
  a1:=a1+#13+Error.Message;

  case MessageDlg(a1, mtError,b,0) of
    mrOk,
    mrRetry: fResult:=reRetry;
    mrIgnore: fResult:=reIgnore;
    else
      fResult:=reRaise;
    end;

  Screen.Cursor:=Cursor;
end;

{----------------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK', [TErrorDialog]);
//end;

(*
procedure TErrorDialogForm.DecodeError(Error:Exception);
var
  i,n:integer;
  plural:boolean;
  a:string[3];
begin
  if Error = nil then begin
    Memo1.Text:='';
    exit;
    end;
  plural:=false;
{
  if Error is EdbEngineError then
    with Error as EdbEngineError do begin
      n:=ErrorCount;
      plural:= n>1;
      Memo1.Text:='Received '+inttostr(n)+' Error';
      if plural then
        Memo1.Text:=Memo1.Text+'s';
      Memo1.Text:=Memo1.Text+' from the Database Engine that';
      end
  else
    if Error is EdbEngineError then
      Memo1.Text:='The Database Access layer reports an error that'
    else
}
      Memo1.Text:='An Error';

  if plural then
    a:='are'
  else
    a:='is';
  Memo1.Text:=Memo1.Text+' '+a+' detailed below. Please decide how to proceed.';
  Memo1.Lines.Add(' ');
{
  if Error is EdbEngineError then
    with Error as EdbEngineError do begin
      for i:=0 to n-1 do
        Memo1.Lines.Add(
          inttostr(i+1)+'/'+inttostr(n)+': '
          +'#'+inttostr(Errors[i].ErrorCode)+': '
          +Errors[i].Message);
      end
  else
}
    Memo1.Text:=Memo1.Text+' '+Error.ClassName+': '+Error.Message+'.';
end;

*)
{----------------------------------------------------------------------------------------}
end.
