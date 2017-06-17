unit listbox;
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

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, 
  Windows, ComCtrls;

type
  TtpListBox = class(TListBox)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property OnDrawItem;                 //surface these events
    property OnMeasureItem;
    end;

{------------------------------------------------------------------------------}

//procedure Register;

implementation

{------------------------------------------------------------------------------}
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpListBox]);
//end;
{------------------------------------------------------------------------------}

end.
