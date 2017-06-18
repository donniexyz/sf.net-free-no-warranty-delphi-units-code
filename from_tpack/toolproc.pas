unit toolproc;	{application wide tool procedures, used to link to dll's or use/link objects into the current project}

(*
Author of original version of this file: Michael Ax

Permission is hereby granted, on 02-Jun-2003, free of charge, to any person
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


{the idea is to literally centralize calls for well defined stuff here.
this makes it easier to use bound and unbound code by a compiler switch.}

interface

{ Put this comment at the beginning of a line to use the DLL instead: $DEFINE USEDLL}
{xx $DEFINE USEDLL}
{ Alternatively select Project Options|Directories/Conditionals and specify USEDLL there}

uses Forms,
  TpMenu,
{$IFDEF USEDLL}
  tpShell;
{$ELSE}
  TpAbout;
{$ENDIF}

procedure AboutBox;

implementation

procedure AboutBox;
begin
  {$IFDEF USEDLL}
    with TDLLShell.Create(Application) do try
      Run('AxSystem','AboutBox');
    finally
      Free;
      end
  {$ELSE}
    With TtPackAboutBox.Create(Application) do try
      Execute;
    finally
      Free;
      end;
  {$ENDIF}
end;

initialization
  AboutBoxProc:=AboutBox;
finalization
  AboutBoxProc:=nil;
end.
