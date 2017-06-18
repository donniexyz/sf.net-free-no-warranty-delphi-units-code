unit tpLogin;
(*
Permission is hereby granted, on 31-Oct-2003, free of charge, to any person
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

uses Classes, Graphics, Forms, Controls, StdCtrls, Buttons, Windows, BDE, 
  UpdateOk, tpAction, utForm, ExtCtrls;

{----------------------------------------------------------------------}

type
  TtpLoginDialogFm = class(TtpFitForm)
    Panel2: TPanel;
    Password: TEdit;
    pnlFooter: TPanel;
    pnlButtons: TPanel;
    btOK: TButton;
    btCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

  TtpLoginDialog = class(TTpAction)
  private
    fPassword: string;
        {type plain/network/windows; include screen save pw coder and tests against bindery}
    fRetries: Integer;
    fRequired: Boolean;
    fPasswordOk: Boolean;
    fCaseSensitive: Boolean;
  protected
  public
    constructor Create(aOwner:Tcomponent); Override;
    procedure Execute; Override;
    function Login(MasterPassword:String):Boolean;
  published
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
    property PassWord: String read fPassWord write fPassword;
    property PassWordOk: Boolean read fPassWordOk write fPasswordOk stored false;
    property Required: Boolean read fRequired write fRequired default true;
    property Retries: Integer read fRetries write fRetries default 2;
    end;

{------------------------------------------------------------------------------}

//procedure Register;

implementation

{------------------------------------------------------------------------------}

uses SysUtils;

{$R *.DFM}

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}

constructor TtpLoginDialog.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  fRequired:=True;
  fRetries:=2;
end;

procedure TtpLoginDialog.Execute;
var
  Cursor:TCursor;
  i: integer;
begin
  if (not fRequired) or (fPassword='') then begin
    fPasswordOk:=True;
    exit;
    end;
  fPasswordOk:=False;
  Cursor:=Screen.Cursor;
  Screen.Cursor:=crDefault;
  With TtpLoginDialogFm.Create(Application) do try
    for i:=0 to fRetries do begin
      Password.SelectAll;
      if ShowModal<>IdOk then
        break;
      if fCaseSensitive then
        fPasswordOk:= (Password.Text=fPassword)
      else
        fPasswordOk:= (uppercase(Password.Text)=uppercase(fPassword));
      if fPasswordOk then
        break;
      end;
  finally
    Free;
    end;
  Screen.Cursor:=Cursor;
end;

function TtpLoginDialog.Login(MasterPassword:String):boolean;
begin
  fPassWord:= MasterPassword;
  Execute;
  fPassword:='';
  Result:= fPasswordOk;
end;

{------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpLoginDialog]);
//end;

{----------------------------------------------------------------------------------------}
end.

