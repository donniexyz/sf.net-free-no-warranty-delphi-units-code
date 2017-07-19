unit test_utIPCObj;

{ ---------------------------------------------------------------------------- }
{ * Copyright (c) 2000-2017 HREF Tools Corp.  All Rights Reserved Worldwide. * }
{ *                                                                          * }
{ * This source code file is part of TPACK and WebHub. Please obtain         * }
{ * a development license from HREF Tools Corp. before using this file, and  * }
{ * refer friends and colleagues to http://www.href.com/tpack. Thanks!       * }
{ ---------------------------------------------------------------------------- }

interface

{$I hrefdefines.inc}

{$IFNDEF MSWINDOWS} !! requires Windows API !! {$ENDIF}

uses
  SysUtils,
  DUnitX.TestFrameWork,
  utIpcObj;

type
  [TestFixture]
  TTest_utIpcObj = class(TObject)
  private
  public
    [Setup]
    procedure SetUp; 
    [TearDown]
    procedure TearDown;
  public
    [Test]
    procedure Test_Compilation;
  public
    [Test]
    procedure Test_utIpcObj_CreateFree_SharedMem;
    //[Test]
    //procedure Test_utIpcObj_SharedMemByte;
  end;

implementation

{ TTest_utIpcObj }

procedure TTest_utIpcObj.SetUp;
begin
  inherited;
//
end;

procedure TTest_utIpcObj.TearDown;
begin
  inherited;
  //
end;

procedure TTest_utIpcObj.Test_Compilation;
begin
  Assert.IsTrue(True);
end;

procedure TTest_utIpcObj.Test_utIpcObj_CreateFree_SharedMem;
var
  sm: TSharedMem;
begin
  sm := nil;
  try
    sm := TSharedMem.CreateNamed('abc', 15);
    PByte(sm.Buffer)^ := 23;
  finally
    FreeAndNil(sm);
  end;
end;

(*
procedure TTest_utIpcObj.Test_utIpcObj_SharedMemByte;
var
  sm1, sm2: TSharedMem;
begin
  sm1 := nil;
  sm2 := nil;
  try
    sm1 := TSharedMem.CreateNamed('abc', 15);
    sm2 := TSharedMem.CreateNamed('abc', 15);
    PByte(sm1.Buffer)^ := 23;
    Assert.AreEqual(Byte(23), PByte(sm2.Buffer)^, 'first byte contains 23');
  finally
    FreeAndNil(sm1);
    FreeAndNil(sm2);
  end;
end;
*)

initialization
  TDUnitX.RegisterTestFixture(TTest_utIpcObj);

end.
