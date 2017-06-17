unit retrydlg;
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

uses 
  Classes, SysUtils, ExtCtrls, Forms, Dialogs, Controls, 
  UcTypes, retry, errormsg; 

type
  TRetryDialog = class(TRetry)
  private
    fErrorDialog: TErrorDialog; {can use a linked in error dialog before ours but after event}
  protected
    procedure DoOnException(Sender:TObject;E:Exception;var Action:TExceptionReAction); override;
  public
  published
    property ErrorDialog: TErrorDialog read fErrorDialog write fErrorDialog;
    end;

implementation

procedure TRetryDialog.DoOnException(Sender:TObject;E:Exception;var Action:TExceptionReAction);
begin
  inherited DoOnException(Sender,E,Action);
  if fErrorDialog<>nil then                       {call fancier error proc}
    with fErrorDialog do begin
      CanRetry:=True;
      CanIgnore:=Self.CanIgnore;
      RetryException(Sender,E,Action);
      end;
end;

end.
