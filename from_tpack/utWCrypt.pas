unit utWCrypt;	{THE WINDOWS SCREENSAVER PASSWORD ENCRYPTION ALGORITHM EN PASCAL}
(*
Permission is hereby granted, on 21-Mar-2003, free of charge, to any person 
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

//this is a one way encyption, use it to scramble strings

interface

uses
  sysutils;

procedure WinEncrypt(Strg: PChar);
procedure EncryptCString(S : PChar);
function EncryptString(const S : String) : string;

implementation

//------------------------------------------------------------------------------

procedure WinEncrypt(Strg: PChar);
var
  StrgPt, Strglg : Integer;                                { Local Vars }
  TheByte : Byte;                                          { Working Char }

  procedure Exor (x1: byte; var x2: byte);
  const  { the last three are '[]=' - not allowed in profile string }
    NotAllowed = [0..$20, $7f..$90, $93..$9f, $3d, $5b, $5d];
  begin
    if not ((x2 xor x1) in NotAllowed) then
      x2 := x2 xor x1;
  end; { Exor }

begin
  StrgLg := lstrlen(Strg);                                 { Get String Length }
  if (StrgLg = 0) then exit;                               { empty string => nothing to do }
  AnsiUpper (Strg);                                        { capitalize the string }

  for StrgPt := 0 to StrgLg - 1 do begin                   { proceed from left to right }
    TheByte := byte (Strg [StrgPt]);                       { get character to encrypt }
    Exor (StrgLg, TheByte);                                { xor it using string length...}
    if (StrgPt = 0) then                                   { If EOS }
      Exor ($2a, TheByte)                                  {...a constant...}
    else begin
      Exor (StrgPt, TheByte);                              {...actual string pointer...}
      Exor (byte (Strg [StrgPt-1]), TheByte);              {...previous character }
      end;
    Strg [StrgPt] := char (TheByte);                       { store encrypted byte back }
    end; { for };

  if (StrgLg > 1) then                                     { no second pass for one-byte-strings }
    for StrgPt := StrgLg-1 downto 0 do begin               { proceed from right to left }
      TheByte := byte (Strg [StrgPt]);                     {  encrypt similar as in first pass }
      Exor (StrgLg, TheByte);                              { xor it using string length...}
      if (StrgPt = StrgLg - 1) then                        { If BOS }
        Exor ($2a, TheByte)                                {...a constant...}
      else begin
        Exor (StrgPt, TheByte);                            {...actual string pointer...}
        Exor (byte (Strg [StrgPt+1]), TheByte);            {...Next character }
        end;
      Strg [StrgPt] := char (TheByte);                     { store encrypted byte back }
      end; { for };
end;

Procedure EncryptCString(S:PChar);
Begin
  WinEncrypt(S);
end;

Function EncryptString(const S:String):string;
begin
  Result:= S;
  WinEncrypt(@Result[1]);
  Result:=copy(Result,1,length(Result));
end;

//------------------------------------------------------------------------------
end.
