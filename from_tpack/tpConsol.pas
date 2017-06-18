unit tpConsol;	{Write/Writeln capable Text Output Console component}
(*
Permission is hereby granted, on 10-May-2003, free of charge, to any person
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
  Classes, SysUtils;

type
  TtpConsole= class;

  TOnConsoleWrite = procedure (Sender:TtpConsole; Buffer:PChar) of object;

  TtpConsole = class(TComponent)
  private
    fInput: Boolean;
    fOutput: Boolean;
    fOnWrite: TOnConsoleWrite;
    procedure SetInput(Value:Boolean);
    procedure SetOutput(Value:Boolean);
  protected
    procedure WriteConsole(Buffer: PChar; Count: Word);
    function ReadConsole(Buffer: PChar; Count: Word): Word;
  public
  published
    property UseInput: Boolean read fInput write SetInput;
    property UseOutput: Boolean read fOutput write SetOutput;
    property OnWrite: TOnConsoleWrite read fOnWrite write fOnWrite;
    end;

//function CrtOutput(var F: TTextRec): Integer; far;
//function CrtInput(var F: TTextRec): Integer; far;
//function CrtClose(var F: TTextRec): Integer; far;
//function CrtOpen(var F: TTextRec): Integer; far;

procedure Register;

implementation

uses
  ucDlgs,
  ucinteg;

//------------------------------------------------------------------------------
//system level procs to plug into the system unit

{ TTextRec }
{type
  TTextRec = record
    Handle: Word;
    Mode: Word;
    BufSize: Word;
    Reserved: Word;
    BufPos: Word;
    BufEnd: Word;
    BufPtr: PChar;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..16] of Byte;
    Name: array[0..79] of Char;
    Buffer: array[0..127] of Char;
    end;
const
  fmClosed = $D7B0;
  fmInput = $D7B1;
  fmOutput = $D7B2;
  fmInOut = $D7B3;
}

{ Text file device driver output function }

function CrtOutput(var F: TTextRec): Integer; far;
begin
  with F do
    if (BufPos>0) then
      with TtpConsole((@UserData)^) do begin
        WriteConsole(PChar(BufPtr), BufPos);
        BufPos:= 0;
        end;
  Result:= 0;
end;

{ Text file device driver input function }

function CrtInput(var F: TTextRec): Integer; far;
begin
  with F do
    with TtpConsole((@UserData)^) do
      BufEnd:= ReadConsole(PChar(BufPtr), BufSize);
  F.BufPos:=0;
  Result:= 0;
end;

{ Text file device driver close function }

function CrtClose(var F: TTextRec): Integer; far;
begin
  Result:= 0;
end;

{ Text file device driver open function }

function CrtOpen(var F: TTextRec): Integer; far;
begin
  with F do begin
    if Mode=fmInput then begin
      InOutFunc:= @CrtInput;
      FlushFunc:= nil;
      end
    else begin
      Mode:= fmOutput;
      InOutFunc:= @CrtOutput;
      FlushFunc:= @CrtOutput;
      end;
    CloseFunc:= @CrtClose;
    end;
  Result:= 0;
end;

procedure AssignSystemIO(var F: TTextRec; Sender:TObject);
// WARNING!!!!
// Below the size of the buffer is declared to be one less than it is.
// this is done to leave room for a null terminator added to the end of
// the string written to in TtpConsole.WriteConsole.
// I HAVE NOT CONFIRMED WHAT WILL HAPPEN IF YOU REASSIGN THE BUFFER TO
// A LARGER BUFFER WITHOUT ADJUSTING FOR THE EXTRA SPACE.
begin
  with F do begin
    Handle := $FFFF;
    Mode := fmClosed;
    BufSize := SizeOf(Buffer)-1; //trailing #0
    BufPtr := @Buffer;
    OpenFunc := @CrtOpen;
    Name[0] := #0;
    Move(Sender, UserData[1],4);
    end;
end;

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//Console Component wrapper

procedure TtpConsole.WriteConsole(Buffer: PChar; Count: Word);
begin
  if Assigned(fOnWrite) then begin //might be beyond end of external buffer.
    Buffer[Count]:=#0;     //warning above. THIS SAVES US FRON MAKING A COPY!
    fOnWrite(Self,Buffer); //Pass on the block as a proper pChar.
    end;
end;

function TtpConsole.ReadConsole(Buffer: PChar; Count: Word): Word;
begin
  Result:=0;
end;

//-----------------------------------------------------------------------------
{ Assign text file to CRT device }

procedure TtpConsole.SetOutput(Value:Boolean);
var
  sysOut:TTextRec absolute System.Output;
begin
  fOutput:=Value;
  if not (csDesigning in ComponentState) then { don't open files at design time }
  if Value then
    with sysout do begin
      if (Mode<>fmClosed) and (Mode<>0) then
        raise Exception.Create('TConsole.SetOptions: Standard Output is already open');
      AssignSystemIO(sysout,Self);
      Rewrite(Output);
      end
  else
    with sysout do begin
      if (Mode<>fmClosed) and (Mode<>0) then
        System.Close(Output);
      end;
end;

procedure TtpConsole.SetInput(Value:Boolean);
var
  sysIn:TTextRec absolute System.Input;
begin
  fInput:=Value;
  if not (csDesigning in ComponentState) then { don't open files at design time }
  if Value then
    with sysin do begin
      if (Mode<>fmClosed) and (Mode<>0) then
        raise Exception.Create('TConsole.SetOptions: Standard Input is already open');
      AssignSystemIO(sysin,Self);
      Reset(Input);
      end
  else
    with sysin do
      if (Mode<>fmClosed) and (Mode<>0) then
        System.Close(Input);
end;

//

//-----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('TPACK', [TtpConsole]);
end;


end.




