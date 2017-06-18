unit uWaitASec;

(*
Permission is hereby granted, on 23-Apr-2005, free of charge, to any person
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

interface

procedure WaitASec;      // pauses for up to a second

implementation

{ TPause }

type
  TPause = class(TObject)
  private
    FTimer: TTimer;
    FDone: Boolean;
    procedure DoTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Wait(AMilliseconds: Cardinal);
  end;

constructor TPause.Create;
begin
  inherited Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
end;

destructor TPause.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TPause.DoTimer(Sender: TObject);
begin
  FDone := True;
  FTimer.Enabled := False;
end;

procedure TPause.Wait(AMilliseconds: Cardinal);
begin
  FTimer.Enabled := False;
  FTimer.Interval := AMilliseconds;
  FDone := False;
  FTimer.Enabled := True;
  repeat Application.HandleMessage until FDone;
end;


procedure WaitASec;
begin
  with TPause.Create do
  try
    Wait(1000);
  finally
    Free;
  end;
end;

end.
 