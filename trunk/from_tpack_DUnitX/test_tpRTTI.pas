unit test_tpRTTI;

(*
  Copyright (c) 2000-2018 HREF Tools Corp.

  Permission is hereby granted, on 18-Jun-2017, free of charge, to any person
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
  SysUtils, Forms,
  DUnitX.TestFrameWork,
  tpRTTI;

type
  [TestFixture]
  TTest_tpRTTI = class(TObject)
  private
    FForm: TForm;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  public
    [Test]
    procedure TestFindComponentByName;
    [Test]
    procedure TestGetPropertyAsInteger;
    [Test]
    procedure TestGetPropertyAsString;
    [Test]
    procedure TestHasProperty;
  end;

implementation

{ TTest_tpRTTI }

procedure TTest_tpRTTI.SetUp;
begin
  Application.CreateForm(TForm, FForm);
  FForm.Name := ClassName + '_Form';
{$IFDEF DUNIT}
  Cleanup_tpRTTI_Cache;
{$ENDIF}
end;

procedure TTest_tpRTTI.TearDown;
begin
  FreeAndNil(FForm);
{$IFDEF DUNIT}
  Cleanup_tpRTTI_Cache;
{$ENDIF}
end;

procedure TTest_tpRTTI.TestFindComponentByName;
begin
  Assert.IsTrue(FForm = FindComponentByName(FForm.Name));
end;

procedure TTest_tpRTTI.TestGetPropertyAsInteger;
begin
  FForm.Width := 400;
  Assert.AreEqual(FForm.Width, GetPropertyAsInteger(FForm.Name, 'Width'));
end;

procedure TTest_tpRTTI.TestGetPropertyAsString;
begin
  FForm.Caption := 'Test Caption';
  Assert.AreEqual(FForm.Caption, GetPropertyAsString(FForm.Name, 'Caption'));
end;

procedure TTest_tpRTTI.TestHasProperty;
begin
  Assert.IsTrue(HasProperty(FForm.Name, 'ActiveControl'));
end;

initialization
  TDUnitX.RegisterTestFixture(TTest_tpRTTI);

end.
