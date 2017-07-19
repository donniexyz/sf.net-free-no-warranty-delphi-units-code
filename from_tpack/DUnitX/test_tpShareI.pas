unit test_tpShareI;

(*
Permission is hereby granted, on 18-Jul-2017, free of charge, to any person
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

uses
  SysUtils, Types,
  DUnitX.TestFrameWork,
  tpShareI;

type
  [TestFixture]
  TTest_tpShareI = class(TObject)
  private
    FCopy1: TtpSharedInt32;
  public
    [Setup]
    procedure SetUp; 
    [TearDown]
    procedure TearDown; 
  public
    [Test]
    procedure Test_tpShareI_NameSame;
    [Test]
    procedure Test_tpShareI_NameDifferent;
  end;

implementation

uses
  System.Classes,
  System.Threading;

procedure TTest_tpShareI.Setup;
begin
  inherited;
  if NOT Assigned(FCopy1) then
  begin
    FCopy1 := TtpSharedInt32.Create(nil);
    FCopy1.Name := 'FCopy1';
    FCopy1.GlobalName := 'DUnitSharedInt';
    // cReadWriteSharedMem, cLocalSharedMem);
    FCopy1.GlobalInteger := 42;
  end;
end;

procedure TTest_tpShareI.TearDown;
begin
  FreeAndNil(FCopy1);
  inherited;
end;

procedure TTest_tpShareI.Test_tpShareI_NameSame;
const cFn = 'Test_tpShareI_NameSame';
var
  x1, x2: Integer;
  FCopy2: TtpSharedInt32;
begin
  FCopy2 := nil;

  TTask.Run(procedure
    begin
      try
        FCopy2 := TtpSharedInt32.Create(nil); // in separate THREAD
        FCopy2.Name := 'FCopy2';
        FCopy2.GlobalName := 'DUnitSharedInt';
      x1 := FCopy1.GlobalInteger;
      x2 := FCopy2.GlobalInteger;
        Assert.AreEqual(x1, x2, 'from GlobalInteger');
        Assert.AreEqual(FCopy1.GlobalInteger, FCopy2.GlobalInteger,
          'more directly GlobalInteger');
      finally
        FreeAndNil(FCopy2);
      end;
    end);
end;

procedure TTest_tpShareI.Test_tpShareI_NameDifferent;
var
  FCopy2: TtpSharedInt32;
begin
  FCopy2 := nil;
  try
    FCopy2 := TtpSharedInt32.Create(nil); // (nil,
    FCopy2.Name := 'FCopy2';
    FCopy2.GlobalName := 'X';
    //, cReadonlySharedMem, cLocalSharedMem);
    Assert.AreNOTEqual(FCopy1.GlobalInteger, FCopy2.GlobalInteger, 'GlobalInteger vs X');
  finally
    FreeAndNil(FCopy2);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTest_tpShareI);

end.
