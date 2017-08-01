unit test_tpCritSect;

(*
Permission is hereby granted, on 24-June-2017, free of charge, to any person
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

{$I hrefdefines.inc}
{
  Master copy of hrefdefines.inc is versioned on Source Forge in the ZaphodsMap project:
  https://sourceforge.net/p/zaphodsmap/code/HEAD/tree/trunk/ZaphodsMap/Source/hrefdefines.inc
}

uses

  SysUtils, Types,
// Kylix:  Libc,
  DUnitX.TestFrameWork,
  tpCritSect;

type
  [TestFixture]
  TTest_tpCritSect = class(TObject)
  private
    FCS: TtpCriticalSection;
  public
    [Setup]
    procedure SetUp; 
    [TearDown]
    procedure TearDown; 
  public
    [Test]
    procedure Test_tpCritSect_UseCriticalSection;
  end;

implementation

procedure TTest_tpCritSect.SetUp;
begin
  inherited;
  FCS := TtpCriticalSection.Create;
end;

procedure TTest_tpCritSect.TearDown;
begin
  inherited;
  FreeAndNil(FCS);
end;

procedure TTest_tpCritSect.Test_tpCritSect_UseCriticalSection;
begin
  try
    FCS.Lock;
    Assert.IsNotNull(FCS);
  finally
    FCS.UnLock;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTest_tpCritSect);

end.
