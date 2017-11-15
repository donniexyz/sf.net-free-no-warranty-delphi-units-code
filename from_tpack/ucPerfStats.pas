unit ucPerfStats;

// see also
// http://stackoverflow.com/questions/33571061/get-the-percentage-of-total-cpu-usage

(*
Permission is hereby granted, on 14-Nov-2017, free of charge, to any person 
obtaining a copy of this software (the "Software"), to deal in the Software 
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

function CPU_Usage: Boolean;

implementation

uses
  Windows, Registry, // CPU Stats
  SysUtils,
  ZM_CodeSiteInterface;

function CPU_Usage: Boolean;
const
  cFn = 'CPU_Usage';
var
  Dummy: array [0 .. 1024] of byte;
  Reg: TRegistry;
  CPUUsage: Integer;

  procedure StopStats;
  begin
    Reg.OpenKey('PerfStats\StopStat',false);
    Reg.ReadBinaryData('KERNEL\CPUUsage',Dummy,SizeOf(Dummy));
    Reg.Free;
  end;

  procedure StartStats;
  begin
    // Stats started by Button1 hit
    Reg.OpenKey('PerfStats\StartStat', False);
    // Open this key first to start collecting performance data
    Reg.ReadBinaryData('KERNEL\CPUUsage', Dummy, SizeOf(Dummy));
    Reg.CloseKey;
  end;

begin

  Reg := nil;
  try
    try
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_DYN_DATA;
      StopStats;
      StartStats;
      Reg.OpenKey('PerfStats\StatData', False);
      Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(Integer));
      Reg.CloseKey;
      if CPUUsage > 0 then
        CSSendWarning(Format('%s: %d', [cFn, CPUUsage]));
      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

end.
