unit ucMemMgr;
(*
Permission is hereby granted, on 1-Nov-2003, free of charge, to any person
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

{$DENYPACKAGEUNIT ON}

//Delphi 2/3 replacement Heap Manager.
// *** Do not include in packages
// *** List this unit first in your projects!

//{DEFINE DEBUGMM} //keep stats

{$IFDEF DEBUGMM}
var
  GetMemCount: Integer;
  FreeMemCount: Integer;
  ReallocMemCount: Integer;
{$ENDIF}

implementation

uses
  Windows;

//------------------------------------------------------------------------------

var
  ProcessHeap: THandle;

function tpGetMem(Size: Integer): Pointer;
//allocate memory and return a pointer to uninitialized memory
begin
{$IFDEF DEBUGMM}
  inc(GetMemCount);
{$ENDIF}
  Result:= HeapAlloc(ProcessHeap, 0, Size);
end;

function tpFreeMem(Ptr: Pointer): Integer;
//free memory allocated for the pointer! (0=ok,1-no)
begin
{$IFDEF DEBUGMM}
  inc(FreeMemCount);
{$ENDIF}
  if HeapFree(ProcessHeap, 0, Ptr) then
    Result:= 0
  else
    Result:= 1;
end;

function tpReallocMem(Ptr: Pointer; Size: Integer): Pointer;
//Reallocate size of memory at pointer. returns new pointer
begin
{$IFDEF DEBUGMM}
  inc(ReallocMemCount);
{$ENDIF}
  Result:= HeapRealloc(ProcessHeap, 0, Ptr, Size);
end;

//------------------------------------------------------------------------------

var
  OrgMemMgr: TMemoryManager;

const
  NewMemMgr: TMemoryManager =
  ( GetMem: tpGetMem;
    FreeMem: tpFreeMem;
    ReallocMem: tpReallocMem);

initialization
  ProcessHeap:= GetProcessHeap;
  GetMemoryManager(OrgMemMgr);
  SetMemoryManager(NewMemMgr);
finalization
  //SetMemoryManager(OrgMemMgr);
end.

